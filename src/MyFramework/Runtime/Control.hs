module MyFramework.Runtime.Control
  ( ControlAction
  , ControlDiagnostic (..)
  , ControlFailure (..)
  , ControlGateResult (..)
  , ControlOutcome (..)
  , ControlRegistry (..)
  , ControlResult (..)
  , ControlScope (..)
  , DemandInvocation (..)
  , LoopPolicy (..)
  , SettlementAudit (..)
  , SettlementRisk (..)
  , auditControlResultSettlement
  , auditSettlementDelta
  , controlResultSucceeded
  , emptyControlScope
  , runControlPlan
  , runControlTree
  ) where

import Control.Exception
  ( SomeException
  , displayException
  , try
  )
import Data.List
  ( find
  , sortOn
  )
import qualified Data.Map.Strict as Map

import MyFramework.Ast
  ( AstPath
  , ChoiceKey
  , MiddlewareRef
  )
import MyFramework.Control
import MyFramework.CURDE.Core
  ( DemandNodeId (..)
  , ImplementationId (..)
  )
import MyFramework.CURDE.Types
  ( HandleId )
import MyFramework.Runtime.Branch
  ( BranchMergeConflict (..)
  , BranchMergeResult (..)
  , BranchSnapshot (..)
  , mergeParallelBranches
  , mergeStrictBranchDelta
  )
import MyFramework.Runtime.Cancellation
  ( normalizeCancelledState )
import MyFramework.Runtime.Concurrent
  ( WorkerOutcome (..)
  , WorkerRace (..)
  , WorkerReport (..)
  , runParallelWorkers
  , runRaceWorkers
  )
import MyFramework.Runtime.State
  ( RuntimeStateParts (..)
  , executionStatusFor
  , runtimeStateParts
  )
import MyFramework.Runtime.Types
  ( CommitState (..)
  , ExecutionStatus (..)
  , ImplementationStatus (..)
  , RuntimeFailure (..)
  )

-- | Runtime-only implementation scope. It is derived from the erased
-- ControlTree and never enters the serializable authoring surface.
newtype ControlScope = ControlScope
  { controlScopeImplementations :: [ImplementationId]
  }
  deriving (Eq, Show)

emptyControlScope :: ControlScope
emptyControlScope =
  ControlScope
    { controlScopeImplementations = []
    }

data DemandInvocation = DemandInvocation
  { demandInvocationPath :: AstPath
  , demandInvocationNode :: DemandNodeId
  , demandInvocationScope :: ControlScope
  }
  deriving (Eq, Show)

data ControlFailure
  = ControlRejected AstPath RuntimeFailure
  | ControlRegistryRaised AstPath String String
  | ControlChoiceUnavailable AstPath ChoiceKey
  | ControlParallelMergeConflict AstPath BranchMergeConflict
  | ControlFallbackMergeConflict
      AstPath
      Int
      BranchMergeConflict
  | ControlFallbackUnsafeSettlement
      AstPath
      Int
      [SettlementRisk]
  | ControlFallbackExhausted AstPath
  | ControlRaceMergeConflict
      AstPath
      Int
      BranchMergeConflict
  | ControlRaceExhausted AstPath
  | ControlRaceSettlementUncertain
      AstPath
      Int
      [SettlementRisk]
  | ControlWorkerRaised AstPath Int String
  | ControlInvalidLoopLimit AstPath Int
  | ControlLoopLimitReached AstPath Int
  deriving (Eq, Show)

data ControlDiagnostic
  = ControlDemandSkippedSucceeded AstPath DemandNodeId
  | ControlParallelConflictObserved AstPath BranchMergeConflict
  | ControlRaceBranchCancelled
      AstPath
      Int
      (Maybe BranchSnapshot)
  | ControlWorkerExceptionObserved AstPath Int String
  deriving (Eq, Show)

data ControlOutcome
  = ControlSucceeded
  | ControlSuspended
  | ControlFailed [ControlFailure]
  deriving (Eq, Show)

data ControlResult = ControlResult
  { controlResultSnapshot :: BranchSnapshot
  , controlResultOutcome :: ControlOutcome
  , controlResultDiagnostics :: [ControlDiagnostic]
  }
  deriving (Eq, Show)

data ControlGateResult
  = ControlGateReady BranchSnapshot
  | ControlGateBlocked BranchSnapshot
  | ControlGateFailed BranchSnapshot ControlFailure
  deriving (Eq, Show)

type ControlAction =
  BranchSnapshot ->
  IO ControlResult

-- | Runtime registries provide operations without expanding the serializable
-- AST facade. Middleware, callback, and context entries receive the child
-- action and may wrap it; Wait and Suspense return one non-blocking gate
-- decision per interpreter step.
data ControlRegistry = ControlRegistry
  { controlDemandCallback ::
      DemandInvocation ->
      ControlAction
  , controlWaitCallback ::
      AstPath ->
      StatusPlan ->
      BranchSnapshot ->
      IO ControlGateResult
  , controlSuspenseCallback ::
      AstPath ->
      HandleId ->
      BranchSnapshot ->
      IO ControlGateResult
  , controlMiddlewareCallback ::
      MiddlewareRef ->
      AstPath ->
      ControlAction ->
      ControlAction
  , controlCallbackCallback ::
      HandleId ->
      AstPath ->
      ControlAction ->
      ControlAction
  , controlLoopPolicy ::
      AstPath ->
      LoopPolicy
  }

-- | Loop evaluation is an explicit bounded fixed-point search. The predicate
-- compares the previous and next immutable snapshots. A false result permits
-- another iteration only while the limit remains.
data LoopPolicy = LoopPolicy
  { loopIterationLimit :: Int
  , loopStabilityPredicate ::
      BranchSnapshot ->
      BranchSnapshot ->
      Bool
  }

-- | A branch may be discarded only when its runtime-state delta proves that
-- no external settlement was observed. Missing snapshots are deliberately
-- uncertain: an exception is not evidence that external work did not commit.
data SettlementAudit
  = SettlementNoExternalCommit
  | SettlementUncertain [SettlementRisk]
  deriving (Eq, Show)

data SettlementRisk
  = SettlementSnapshotUnavailable
  | SettlementExecutionPending HandleId
  | SettlementExecutionRunning HandleId
  | SettlementExecutionSucceeded HandleId
  | SettlementExecutionOutcomeUnknown HandleId RuntimeFailure
  | SettlementExecutionFailedAfterCommit HandleId RuntimeFailure
  | SettlementImplementationPending ImplementationId
  | SettlementImplementationRunning ImplementationId
  | SettlementImplementationSucceededAfterCommit
      ImplementationId
      HandleId
  | SettlementImplementationSucceededWithoutTargetExecution
      ImplementationId
      HandleId
  | SettlementImplementationSucceededWithInconsistentTarget
      ImplementationId
      HandleId
      ExecutionStatus
  | SettlementImplementationFailedAfterCommit
      ImplementationId
      RuntimeFailure
  | SettlementControlRejected AstPath RuntimeFailure
  | SettlementActionRaised AstPath String String
  | SettlementWorkerRaised AstPath Int String
  | SettlementPropagatedFallbackUnsafe
      AstPath
      Int
      [SettlementRisk]
  | SettlementPropagatedRaceUncertain
      AstPath
      Int
      [SettlementRisk]
  deriving (Eq, Show)

auditSettlementDelta ::
  RuntimeStateParts ->
  Maybe RuntimeStateParts ->
  SettlementAudit
auditSettlementDelta _ Nothing =
  SettlementUncertain [SettlementSnapshotUnavailable]
auditSettlementDelta baseParts (Just branchParts) =
  case executionRisks ++ implementationRisks of
    [] ->
      SettlementNoExternalCommit
    currentRisks ->
      SettlementUncertain currentRisks
  where
    executionRisks =
      concatMap
        executionSettlementRisks
        ( changedEntries
            (runtimePartExecutionStatuses baseParts)
            (runtimePartExecutionStatuses branchParts)
        )
    implementationRisks =
      concatMap
        implementationSettlementRisks
        ( changedEntries
            (runtimePartImplementationStatuses baseParts)
            (runtimePartImplementationStatuses branchParts)
        )

    executionSettlementRisks (currentHandle, currentStatus) =
      case currentStatus of
        ExecutionPending ->
          [SettlementExecutionPending currentHandle]
        ExecutionRunning ->
          [SettlementExecutionRunning currentHandle]
        ExecutionSucceeded ->
          [SettlementExecutionSucceeded currentHandle]
        ExecutionOutcomeUnknown currentFailure ->
          [ SettlementExecutionOutcomeUnknown
              currentHandle
              currentFailure
          ]
        ExecutionFailed currentFailure
          | runtimeFailureCommitState currentFailure
              /= NoExternalCommit ->
              [ SettlementExecutionFailedAfterCommit
                  currentHandle
                  currentFailure
              ]
        _ ->
          []

    implementationSettlementRisks
      (currentImplementation, currentStatus) =
        case currentStatus of
          ImplementationPending ->
            [SettlementImplementationPending currentImplementation]
          ImplementationRunning ->
            [SettlementImplementationRunning currentImplementation]
          ImplementationSucceeded ->
            [implementationSucceededRisk currentImplementation]
          ImplementationFailed currentFailure
            | runtimeFailureCommitState currentFailure
                /= NoExternalCommit ->
                [ SettlementImplementationFailedAfterCommit
                    currentImplementation
                    currentFailure
                ]
          _ ->
            []

    implementationSucceededRisk currentImplementation =
      case
          Map.lookup
            currentTarget
            (runtimePartExecutionStatuses branchParts) of
        Nothing ->
          SettlementImplementationSucceededWithoutTargetExecution
            currentImplementation
            currentTarget
        Just currentExecution
          | executionStatusCommitted currentExecution ->
              SettlementImplementationSucceededAfterCommit
                currentImplementation
                currentTarget
          | otherwise ->
              SettlementImplementationSucceededWithInconsistentTarget
                currentImplementation
                currentTarget
                currentExecution
      where
        currentTarget =
          implementationIdTarget currentImplementation

    executionStatusCommitted currentExecution =
      case currentExecution of
        ExecutionSucceeded ->
          True
        ExecutionFailed currentFailure ->
          runtimeFailureCommitState currentFailure
            == ExternalCommitted
        ExecutionOutcomeUnknown currentFailure ->
          runtimeFailureCommitState currentFailure
            == ExternalCommitted
        _ ->
          False

changedEntries ::
  (Ord key, Eq value) =>
  Map.Map key value ->
  Map.Map key value ->
  [(key, value)]
changedEntries baseMap branchMap =
  [ (currentKey, currentValue)
  | (currentKey, currentValue) <- Map.toAscList branchMap
  , Map.lookup currentKey baseMap /= Just currentValue
  ]

-- | Prove settlement safety from both the immutable state delta and the
-- failure provenance carried by the returned control result.
auditControlResultSettlement ::
  BranchSnapshot ->
  ControlResult ->
  SettlementAudit
auditControlResultSettlement currentBase currentResult =
  settlementAuditFromRisks
    ( settlementAuditRisks stateAudit
        ++ concatMap
          controlFailureSettlementRisks
          (resultFailures currentResult)
    )
  where
    stateAudit =
      auditSettlementDelta
        (runtimeStateParts (branchRuntimeState currentBase))
        ( Just
            ( runtimeStateParts
                ( branchRuntimeState
                    (controlResultSnapshot currentResult)
                )
            )
        )

controlFailureSettlementRisks ::
  ControlFailure ->
  [SettlementRisk]
controlFailureSettlementRisks currentFailure =
  case currentFailure of
    ControlRejected currentPath currentRuntimeFailure
      | runtimeFailureCommitState currentRuntimeFailure
          /= NoExternalCommit ->
          [ SettlementControlRejected
              currentPath
              currentRuntimeFailure
          ]
    ControlRegistryRaised currentPath currentAction currentMessage ->
      [ SettlementActionRaised
          currentPath
          currentAction
          currentMessage
      ]
    ControlFallbackUnsafeSettlement
      currentPath
      currentIndex
      currentRisks ->
        [ SettlementPropagatedFallbackUnsafe
            currentPath
            currentIndex
            currentRisks
        ]
    ControlRaceSettlementUncertain
      currentPath
      currentIndex
      currentRisks ->
        [ SettlementPropagatedRaceUncertain
            currentPath
            currentIndex
            currentRisks
        ]
    ControlWorkerRaised currentPath currentIndex currentMessage ->
      [ SettlementWorkerRaised
          currentPath
          currentIndex
          currentMessage
      ]
    _ ->
      []

settlementAuditRisks :: SettlementAudit -> [SettlementRisk]
settlementAuditRisks currentAudit =
  case currentAudit of
    SettlementNoExternalCommit ->
      []
    SettlementUncertain currentRisks ->
      currentRisks

settlementAuditFromRisks :: [SettlementRisk] -> SettlementAudit
settlementAuditFromRisks currentRisks =
  case currentRisks of
    [] ->
      SettlementNoExternalCommit
    _ ->
      SettlementUncertain currentRisks

controlResultSucceeded :: ControlResult -> Bool
controlResultSucceeded currentResult =
  case controlResultOutcome currentResult of
    ControlSucceeded ->
      True
    _ ->
      False

-- | Interpret the single validated boot root.
runControlPlan ::
  ControlRegistry ->
  ControlPlan ->
  BranchSnapshot ->
  IO ControlResult
runControlPlan currentRegistry currentPlan =
  runControlTree
    currentRegistry
    (controlPlanBoot currentPlan)

runControlTree ::
  ControlRegistry ->
  ControlTree ->
  BranchSnapshot ->
  IO ControlResult
runControlTree currentRegistry =
  runTree
    currentRegistry
    emptyControlScope

runTree ::
  ControlRegistry ->
  ControlScope ->
  ControlTree ->
  BranchSnapshot ->
  IO ControlResult
runTree currentRegistry currentScope currentTree currentSnapshot =
  case controlTreeNode currentTree of
    ControlDemand currentNode ->
      runDemand
        currentRegistry
        currentScope
        currentPath
        currentNode
        currentSnapshot
    ControlWithImplementation currentImplementation child ->
      runTree
        currentRegistry
        (pushImplementation currentImplementation currentScope)
        child
        currentSnapshot
    ControlSequence children ->
      runSequence
        currentRegistry
        currentScope
        children
        currentSnapshot
    ControlParallel children ->
      runParallel
        currentRegistry
        currentScope
        currentPath
        children
        currentSnapshot
    ControlFallback children ->
      runFallback
        currentRegistry
        currentScope
        currentPath
        children
        currentSnapshot
    ControlRace children ->
      runRace
        currentRegistry
        currentScope
        currentPath
        children
        currentSnapshot
    ControlChoice currentSelection branches ->
      case lookup currentSelection branches of
        Just currentBranch ->
          runTree
            currentRegistry
            currentScope
            currentBranch
            currentSnapshot
        Nothing ->
          pure
            ( failedResult
                currentSnapshot
                [ ControlChoiceUnavailable
                    currentPath
                    currentSelection
                ]
            )
    ControlWait currentStatus child ->
      runWait
        currentRegistry
        currentScope
        currentPath
        currentStatus
        child
        currentSnapshot
    ControlLoop child ->
      runLoop
        currentRegistry
        currentScope
        currentPath
        child
        (controlLoopPolicy currentRegistry currentPath)
        currentSnapshot
    ControlMiddleware currentMiddleware child ->
      runWrappedRegistry
        currentPath
        "middleware"
        currentSnapshot
        ( controlMiddlewareCallback
            currentRegistry
            currentMiddleware
            currentPath
            (runTree currentRegistry currentScope child)
        )
    ControlCallback currentHandle child ->
      runWrappedRegistry
        currentPath
        "callback"
        currentSnapshot
        ( controlCallbackCallback
            currentRegistry
            currentHandle
            currentPath
            (runTree currentRegistry currentScope child)
        )
    ControlSuspense currentHandle ->
      runSuspense
        currentRegistry
        currentPath
        currentHandle
        currentSnapshot
  where
    currentPath =
      controlTreePath currentTree

runDemand ::
  ControlRegistry ->
  ControlScope ->
  AstPath ->
  DemandNodeId ->
  BranchSnapshot ->
  IO ControlResult
runDemand currentRegistry currentScope currentPath currentNode currentSnapshot
  | demandAlreadySucceeded currentNode currentSnapshot =
      pure
        ControlResult
          { controlResultSnapshot = currentSnapshot
          , controlResultOutcome = ControlSucceeded
          , controlResultDiagnostics =
              [ ControlDemandSkippedSucceeded
                  currentPath
                  currentNode
              ]
          }
  | otherwise =
      tryControlAction
        currentPath
        "demand"
        currentSnapshot
        ( controlDemandCallback
            currentRegistry
            DemandInvocation
              { demandInvocationPath = currentPath
              , demandInvocationNode = currentNode
              , demandInvocationScope = currentScope
              }
            currentSnapshot
        )

demandAlreadySucceeded :: DemandNodeId -> BranchSnapshot -> Bool
demandAlreadySucceeded currentNode currentSnapshot =
  executionStatusFor currentTarget (branchRuntimeState currentSnapshot)
    == ExecutionSucceeded
  where
    currentTarget =
      case currentNode of
        HandleNode currentHandle ->
          currentHandle
        ImplementationNode currentImplementation ->
          implementationIdTarget currentImplementation

pushImplementation :: ImplementationId -> ControlScope -> ControlScope
pushImplementation currentImplementation currentScope =
  currentScope
    { controlScopeImplementations =
        currentImplementation
          : controlScopeImplementations currentScope
    }

runSequence ::
  ControlRegistry ->
  ControlScope ->
  [ControlTree] ->
  BranchSnapshot ->
  IO ControlResult
runSequence _ _ [] currentSnapshot =
  pure (succeededResult currentSnapshot)
runSequence currentRegistry currentScope (currentTree : remaining) currentSnapshot = do
  currentResult <-
    runTree
      currentRegistry
      currentScope
      currentTree
      currentSnapshot
  case controlResultOutcome currentResult of
    ControlSucceeded -> do
      nextResult <-
        runSequence
          currentRegistry
          currentScope
          remaining
          (controlResultSnapshot currentResult)
      pure
        ( prependDiagnostics
            (controlResultDiagnostics currentResult)
            nextResult
        )
    _ ->
      pure currentResult

runParallel ::
  ControlRegistry ->
  ControlScope ->
  AstPath ->
  [ControlTree] ->
  BranchSnapshot ->
  IO ControlResult
runParallel currentRegistry currentScope currentPath children currentBase = do
  currentReports <-
    runParallelWorkers
      [ runTree
          currentRegistry
          currentScope
          currentChild
          currentBase
      | currentChild <- children
      ]
  let currentResults =
        map (workerResult currentPath currentBase) currentReports
      currentMerge =
        mergeParallelBranches
          currentBase
          (map controlResultSnapshot currentResults)
      currentConflicts =
        branchMergeConflicts currentMerge
      currentFailures =
        concatMap resultFailures currentResults
          ++ map
            (ControlParallelMergeConflict currentPath)
            currentConflicts
      currentOutcome
        | not (null currentFailures) =
            ControlFailed currentFailures
        | any resultSuspended currentResults =
            ControlSuspended
        | otherwise =
            ControlSucceeded
      currentDiagnostics =
        concatMap controlResultDiagnostics currentResults
          ++ map
            (ControlParallelConflictObserved currentPath)
            currentConflicts
  pure
    ControlResult
      { controlResultSnapshot =
          branchMergeSnapshot currentMerge
      , controlResultOutcome = currentOutcome
      , controlResultDiagnostics = currentDiagnostics
      }

runFallback ::
  ControlRegistry ->
  ControlScope ->
  AstPath ->
  [ControlTree] ->
  BranchSnapshot ->
  IO ControlResult
runFallback currentRegistry currentScope currentPath children currentBase =
  tryBranches 0 [] [] children
  where
    tryBranches _ currentFailures currentDiagnostics [] =
      pure
        ControlResult
          { controlResultSnapshot = currentBase
          , controlResultOutcome =
              ControlFailed
                ( currentFailures
                    ++ [ControlFallbackExhausted currentPath]
                )
          , controlResultDiagnostics = currentDiagnostics
          }
    tryBranches
      currentIndex
      currentFailures
      currentDiagnostics
      (currentChild : remaining) = do
        currentResult <-
          runTree
            currentRegistry
            currentScope
            currentChild
            currentBase
        case controlResultOutcome currentResult of
          ControlSucceeded ->
            case
                mergeStrictBranchDelta
                  currentIndex
                  currentBase
                  currentBase
                  (controlResultSnapshot currentResult) of
              Right currentSnapshot ->
                pure
                  currentResult
                    { controlResultSnapshot = currentSnapshot
                    , controlResultDiagnostics =
                        currentDiagnostics
                          ++ controlResultDiagnostics currentResult
                    }
              Left currentConflicts ->
                pure
                  ControlResult
                    { controlResultSnapshot = currentBase
                    , controlResultOutcome =
                        ControlFailed
                          ( map
                              ( ControlFallbackMergeConflict
                                  currentPath
                                  currentIndex
                              )
                              currentConflicts
                          )
                    , controlResultDiagnostics =
                        currentDiagnostics
                          ++ controlResultDiagnostics currentResult
                    }
          ControlSuspended ->
            pure
              ( prependDiagnostics
                  currentDiagnostics
                  currentResult
              )
          ControlFailed nextFailures ->
            case
                auditControlResultSettlement
                  currentBase
                  currentResult of
              SettlementNoExternalCommit ->
                tryBranches
                  (currentIndex + 1)
                  (currentFailures ++ nextFailures)
                  ( currentDiagnostics
                      ++ controlResultDiagnostics currentResult
                  )
                  remaining
              SettlementUncertain currentRisks ->
                let currentMerge =
                      mergeParallelBranches
                        currentBase
                        ( replicate currentIndex currentBase
                            ++ [controlResultSnapshot currentResult]
                        )
                    currentConflicts =
                      branchMergeConflicts currentMerge
                 in pure
                      ControlResult
                        { controlResultSnapshot =
                            branchMergeSnapshot currentMerge
                        , controlResultOutcome =
                            ControlFailed
                              ( currentFailures
                                  ++ nextFailures
                                  ++ [ ControlFallbackUnsafeSettlement
                                         currentPath
                                         currentIndex
                                         currentRisks
                                     ]
                                  ++ map
                                    ( ControlFallbackMergeConflict
                                        currentPath
                                        currentIndex
                                    )
                                    currentConflicts
                              )
                        , controlResultDiagnostics =
                            currentDiagnostics
                              ++ controlResultDiagnostics currentResult
                        }

runRace ::
  ControlRegistry ->
  ControlScope ->
  AstPath ->
  [ControlTree] ->
  BranchSnapshot ->
  IO ControlResult
runRace currentRegistry currentScope currentPath children currentBase = do
  currentRace <-
    runRaceWorkers
      controlResultSucceeded
      [ runTree
          currentRegistry
          currentScope
          currentChild
          currentBase
      | currentChild <- children
      ]
  case currentRace of
    WorkerRaceExhausted currentReports ->
      pure
        (raceExhaustedResult
          currentPath
          currentBase
          currentReports
        )
    WorkerRaceWon
      currentWinner
      currentCancelled
      currentReports ->
        pure
          ( raceWinnerResult
              currentPath
              currentBase
              currentWinner
              currentCancelled
              currentReports
          )

raceExhaustedResult ::
  AstPath ->
  BranchSnapshot ->
  [WorkerReport ControlResult] ->
  ControlResult
raceExhaustedResult currentPath currentBase currentReports =
  ControlResult
    { controlResultSnapshot =
        branchMergeSnapshot currentMerge
    , controlResultOutcome =
        if null currentFailures
          && any resultSuspended currentResults
          then ControlSuspended
          else
            ControlFailed
              ( currentFailures
                  ++ [ControlRaceExhausted currentPath]
              )
    , controlResultDiagnostics =
        allReportDiagnostics
          currentPath
          currentBase
          currentReports
    }
  where
    currentResults =
      map (workerResult currentPath currentBase) currentReports
    currentSettlements =
      raceSettlements
        currentBase
        Nothing
        []
        currentReports
    currentMerge =
      raceConservativeMerge
        currentBase
        Nothing
        Nothing
        currentSettlements
        currentReports
    currentSettlementFailures =
      raceSettlementFailures currentPath currentSettlements
    currentMergeFailures =
      raceMergeFailures
        currentPath
        (branchMergeConflicts currentMerge)
    currentFailures =
      concatMap resultFailures currentResults
        ++ currentSettlementFailures
        ++ currentMergeFailures

raceWinnerResult ::
  AstPath ->
  BranchSnapshot ->
  Int ->
  [Int] ->
  [WorkerReport ControlResult] ->
  ControlResult
raceWinnerResult
  currentPath
  currentBase
  currentWinner
  currentCancelled
  currentReports =
    case currentWinnerResult of
      Nothing ->
        ControlResult
          { controlResultSnapshot =
              branchMergeSnapshot currentMerge
          , controlResultOutcome =
              ControlFailed
                ( [ ControlWorkerRaised
                      currentPath
                      currentWinner
                      "race winner did not publish a successful result"
                  ]
                    ++ currentSettlementFailures
                    ++ currentMergeFailures
                )
          , controlResultDiagnostics = currentDiagnostics
          }
      Just currentResult ->
        if null currentFailures
          then
            currentResult
              { controlResultSnapshot =
                  branchMergeSnapshot currentMerge
              , controlResultDiagnostics = currentDiagnostics
              }
          else
            ControlResult
              { controlResultSnapshot =
                  branchMergeSnapshot currentMerge
              , controlResultOutcome =
                  ControlFailed currentFailures
              , controlResultDiagnostics = currentDiagnostics
              }
  where
    currentWinnerResult =
      winnerResult currentWinner currentReports
    currentSettledWinner =
      case currentWinnerResult of
        Just _ ->
          Just currentWinner
        Nothing ->
          Nothing
    currentSettlements =
      raceSettlements
        currentBase
        currentSettledWinner
        currentCancelled
        currentReports
    currentMerge =
      raceConservativeMerge
        currentBase
        currentSettledWinner
        currentWinnerResult
        currentSettlements
        currentReports
    currentSettlementFailures =
      raceSettlementFailures currentPath currentSettlements
    currentMergeFailures =
      raceMergeFailures
        currentPath
        (branchMergeConflicts currentMerge)
    currentFailures =
      currentSettlementFailures ++ currentMergeFailures
    currentDiagnostics =
      allReportDiagnostics
        currentPath
        currentBase
        currentReports
        ++ cancellationDiagnostics
          currentPath
          currentBase
          currentCancelled
          currentReports

winnerResult ::
  Int ->
  [WorkerReport ControlResult] ->
  Maybe ControlResult
winnerResult currentWinner currentReports =
  case
      find
        ((== currentWinner) . workerReportIndex)
        currentReports of
    Just currentReport ->
      case workerReportOutcome currentReport of
        WorkerReturned currentResult
          | controlResultSucceeded currentResult ->
              Just currentResult
        _ ->
          Nothing
    Nothing ->
      Nothing

data RaceSettlement = RaceSettlement
  { raceSettlementIndex :: Int
  , raceSettlementSnapshot :: Maybe BranchSnapshot
  , raceSettlementAudit :: SettlementAudit
  }
  deriving (Eq, Show)

raceSettlements ::
  BranchSnapshot ->
  Maybe Int ->
  [Int] ->
  [WorkerReport ControlResult] ->
  [RaceSettlement]
raceSettlements
  currentBase
  currentWinner
  currentCancelled
  currentReports =
    map auditReport currentLosers
  where
    currentLosers =
      [ currentReport
      | currentReport <- sortOn workerReportIndex currentReports
      , Just (workerReportIndex currentReport) /= currentWinner
      ]

    auditReport currentReport =
      let currentResult =
            raceReportResult
              currentBase
              currentCancelled
              currentReport
          currentSnapshot =
            controlResultSnapshot <$> currentResult
          currentAudit =
            case currentResult of
              Nothing ->
                auditSettlementDelta
                  (runtimeStateParts (branchRuntimeState currentBase))
                  Nothing
              Just nextResult ->
                auditControlResultSettlement
                  currentBase
                  nextResult
       in RaceSettlement
            { raceSettlementIndex =
                workerReportIndex currentReport
            , raceSettlementSnapshot = currentSnapshot
            , raceSettlementAudit = currentAudit
            }

raceReportResult ::
  BranchSnapshot ->
  [Int] ->
  WorkerReport ControlResult ->
  Maybe ControlResult
raceReportResult currentBase currentCancelled currentReport =
  case workerReportOutcome currentReport of
    WorkerReturned currentResult
      | workerReportIndex currentReport `elem` currentCancelled ->
          Just
            currentResult
              { controlResultSnapshot =
                  normalizeCancelledSnapshot
                    currentBase
                    (controlResultSnapshot currentResult)
              }
      | otherwise ->
          Just currentResult
    WorkerRaised _ ->
      Nothing

raceConservativeMerge ::
  BranchSnapshot ->
  Maybe Int ->
  Maybe ControlResult ->
  [RaceSettlement] ->
  [WorkerReport ControlResult] ->
  BranchMergeResult
raceConservativeMerge
  currentBase
  currentWinner
  currentWinnerResult
  currentSettlements
  currentReports =
    mergeParallelBranches
      currentBase
      [ snapshotForMerge currentReport
      | currentReport <- sortOn workerReportIndex currentReports
      ]
  where
    snapshotForMerge currentReport
      | Just currentIndex == currentWinner =
          case currentWinnerResult of
            Just currentResult ->
              controlResultSnapshot currentResult
            Nothing ->
              currentBase
      | otherwise =
          case
              find
                ((== currentIndex) . raceSettlementIndex)
                currentSettlements of
            Just currentSettlement ->
              case
                  ( raceSettlementAudit currentSettlement
                  , raceSettlementSnapshot currentSettlement
                  ) of
                (SettlementUncertain _, Just currentSnapshot) ->
                  currentSnapshot
                _ ->
                  currentBase
            Nothing ->
              currentBase
      where
        currentIndex =
          workerReportIndex currentReport

raceSettlementFailures ::
  AstPath ->
  [RaceSettlement] ->
  [ControlFailure]
raceSettlementFailures currentPath =
  concatMap currentFailure
  where
    currentFailure currentSettlement =
      case raceSettlementAudit currentSettlement of
        SettlementNoExternalCommit ->
          []
        SettlementUncertain currentRisks ->
          [ ControlRaceSettlementUncertain
              currentPath
              (raceSettlementIndex currentSettlement)
              currentRisks
          ]

raceMergeFailures ::
  AstPath ->
  [BranchMergeConflict] ->
  [ControlFailure]
raceMergeFailures currentPath =
  map
    ( \currentConflict ->
        ControlRaceMergeConflict
          currentPath
          (branchConflictIndex currentConflict)
          currentConflict
    )

branchConflictIndex :: BranchMergeConflict -> Int
branchConflictIndex currentConflict =
  case currentConflict of
    BranchValueConflict currentIndex _ _ _ _ _ ->
      currentIndex
    BranchObservationConflict currentIndex _ ->
      currentIndex
    BranchEventPrefixConflict currentIndex _ _ _ ->
      currentIndex

allReportDiagnostics ::
  AstPath ->
  BranchSnapshot ->
  [WorkerReport ControlResult] ->
  [ControlDiagnostic]
allReportDiagnostics currentPath currentBase =
  concatMap reportDiagnostics
  where
    reportDiagnostics currentReport =
      case workerReportOutcome currentReport of
        WorkerReturned currentResult ->
          controlResultDiagnostics currentResult
        WorkerRaised _ ->
          controlResultDiagnostics
            (workerResult currentPath currentBase currentReport)

cancellationDiagnostics ::
  AstPath ->
  BranchSnapshot ->
  [Int] ->
  [WorkerReport ControlResult] ->
  [ControlDiagnostic]
cancellationDiagnostics currentPath currentBase currentCancelled currentReports =
  [ ControlRaceBranchCancelled
      currentPath
      currentIndex
      (normalizedCancelledResult currentIndex)
  | currentIndex <- currentCancelled
  ]
  where
    normalizedCancelledResult currentIndex =
      case
          find
            ((== currentIndex) . workerReportIndex)
            currentReports of
        Just currentReport ->
          case workerReportOutcome currentReport of
            WorkerReturned currentResult ->
              Just
                ( normalizeCancelledSnapshot
                    currentBase
                    (controlResultSnapshot currentResult)
                )
            WorkerRaised _ ->
              Nothing
        Nothing ->
          Nothing

normalizeCancelledSnapshot ::
  BranchSnapshot ->
  BranchSnapshot ->
  BranchSnapshot
normalizeCancelledSnapshot currentBase currentBranch =
  currentBranch
    { branchRuntimeState =
        normalizeCancelledState
          (branchRuntimeState currentBase)
          (branchRuntimeState currentBranch)
    }

workerResult ::
  AstPath ->
  BranchSnapshot ->
  WorkerReport ControlResult ->
  ControlResult
workerResult currentPath currentBase currentReport =
  case workerReportOutcome currentReport of
    WorkerReturned currentResult ->
      currentResult
    WorkerRaised currentException ->
      let currentMessage =
            displayException currentException
       in ControlResult
            { controlResultSnapshot = currentBase
            , controlResultOutcome =
                ControlFailed
                  [ ControlWorkerRaised
                      currentPath
                      (workerReportIndex currentReport)
                      currentMessage
                  ]
            , controlResultDiagnostics =
                [ ControlWorkerExceptionObserved
                    currentPath
                    (workerReportIndex currentReport)
                    currentMessage
                ]
            }

runWait ::
  ControlRegistry ->
  ControlScope ->
  AstPath ->
  StatusPlan ->
  ControlTree ->
  BranchSnapshot ->
  IO ControlResult
runWait
  currentRegistry
  currentScope
  currentPath
  currentStatus
  currentChild
  currentSnapshot = do
    currentGate <-
      tryRegistry
        currentPath
        "wait"
        ( controlWaitCallback
            currentRegistry
            currentPath
            currentStatus
            currentSnapshot
        )
    case currentGate of
      Left currentFailure ->
        pure (failedResult currentSnapshot [currentFailure])
      Right (ControlGateReady nextSnapshot) ->
        runTree
          currentRegistry
          currentScope
          currentChild
          nextSnapshot
      Right (ControlGateBlocked nextSnapshot) ->
        pure (suspendedResult nextSnapshot)
      Right (ControlGateFailed nextSnapshot currentFailure) ->
        pure (failedResult nextSnapshot [currentFailure])

runSuspense ::
  ControlRegistry ->
  AstPath ->
  HandleId ->
  BranchSnapshot ->
  IO ControlResult
runSuspense currentRegistry currentPath currentHandle currentSnapshot = do
  currentGate <-
    tryRegistry
      currentPath
      "suspense"
      ( controlSuspenseCallback
          currentRegistry
          currentPath
          currentHandle
          currentSnapshot
      )
  case currentGate of
    Left currentFailure ->
      pure (failedResult currentSnapshot [currentFailure])
    Right (ControlGateReady nextSnapshot) ->
      pure (succeededResult nextSnapshot)
    Right (ControlGateBlocked nextSnapshot) ->
      pure (suspendedResult nextSnapshot)
    Right (ControlGateFailed nextSnapshot currentFailure) ->
      pure (failedResult nextSnapshot [currentFailure])

runWrappedRegistry ::
  AstPath ->
  String ->
  BranchSnapshot ->
  ControlAction ->
  IO ControlResult
runWrappedRegistry currentPath currentName currentSnapshot currentAction =
  tryControlAction
    currentPath
    currentName
    currentSnapshot
    (currentAction currentSnapshot)

runLoop ::
  ControlRegistry ->
  ControlScope ->
  AstPath ->
  ControlTree ->
  LoopPolicy ->
  BranchSnapshot ->
  IO ControlResult
runLoop
  currentRegistry
  currentScope
  currentPath
  currentChild
  currentPolicy
  currentSnapshot
    | currentLimit <= 0 =
        pure
          ( failedResult
              currentSnapshot
              [ControlInvalidLoopLimit currentPath currentLimit]
          )
    | otherwise =
        nextIteration 1 [] currentSnapshot
  where
    currentLimit =
      loopIterationLimit currentPolicy
    nextIteration
      currentIteration
      currentDiagnostics
      previousSnapshot = do
        currentResult <-
          runTree
            currentRegistry
            currentScope
            currentChild
            previousSnapshot
        let nextDiagnostics =
              currentDiagnostics
                ++ controlResultDiagnostics currentResult
            nextSnapshot =
              controlResultSnapshot currentResult
        case controlResultOutcome currentResult of
          ControlSucceeded
            | loopStabilityPredicate
                currentPolicy
                previousSnapshot
                nextSnapshot ->
                pure
                  currentResult
                    { controlResultDiagnostics = nextDiagnostics
                    }
            | currentIteration >= currentLimit ->
                pure
                  ControlResult
                    { controlResultSnapshot = nextSnapshot
                    , controlResultOutcome =
                        ControlFailed
                          [ ControlLoopLimitReached
                              currentPath
                              currentLimit
                          ]
                    , controlResultDiagnostics = nextDiagnostics
                    }
            | otherwise ->
                nextIteration
                  (currentIteration + 1)
                  nextDiagnostics
                  nextSnapshot
          _ ->
            pure
              currentResult
                { controlResultDiagnostics = nextDiagnostics
                }

tryControlAction ::
  AstPath ->
  String ->
  BranchSnapshot ->
  IO ControlResult ->
  IO ControlResult
tryControlAction currentPath currentName currentSnapshot currentAction = do
  currentOutcome <-
    tryAny currentAction
  case currentOutcome of
    Left currentException ->
      pure
        ( failedResult
            currentSnapshot
            [ ControlRegistryRaised
                currentPath
                currentName
                (displayException currentException)
            ]
        )
    Right currentResult ->
      pure currentResult

tryRegistry ::
  AstPath ->
  String ->
  IO value ->
  IO (Either ControlFailure value)
tryRegistry currentPath currentName currentAction = do
  currentOutcome <-
    tryAny currentAction
  case currentOutcome of
    Left currentException ->
      pure
        ( Left
            ( ControlRegistryRaised
                currentPath
                currentName
                (displayException currentException)
            )
        )
    Right currentValue ->
      pure (Right currentValue)

tryAny :: IO value -> IO (Either SomeException value)
tryAny =
  try

succeededResult :: BranchSnapshot -> ControlResult
succeededResult currentSnapshot =
  ControlResult
    { controlResultSnapshot = currentSnapshot
    , controlResultOutcome = ControlSucceeded
    , controlResultDiagnostics = []
    }

suspendedResult :: BranchSnapshot -> ControlResult
suspendedResult currentSnapshot =
  ControlResult
    { controlResultSnapshot = currentSnapshot
    , controlResultOutcome = ControlSuspended
    , controlResultDiagnostics = []
    }

failedResult ::
  BranchSnapshot ->
  [ControlFailure] ->
  ControlResult
failedResult currentSnapshot currentFailures =
  ControlResult
    { controlResultSnapshot = currentSnapshot
    , controlResultOutcome = ControlFailed currentFailures
    , controlResultDiagnostics = []
    }

prependDiagnostics ::
  [ControlDiagnostic] ->
  ControlResult ->
  ControlResult
prependDiagnostics currentDiagnostics currentResult =
  currentResult
    { controlResultDiagnostics =
        currentDiagnostics
          ++ controlResultDiagnostics currentResult
    }

resultFailures :: ControlResult -> [ControlFailure]
resultFailures currentResult =
  case controlResultOutcome currentResult of
    ControlFailed currentFailures ->
      currentFailures
    _ ->
      []

resultSuspended :: ControlResult -> Bool
resultSuspended currentResult =
  case controlResultOutcome currentResult of
    ControlSuspended ->
      True
    _ ->
      False
