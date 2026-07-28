module MyFramework.Runtime.Demand
  ( DemandSession
  , demandSessionBootRunId
  , demandInSession
  , forkDemandSession
  , forkDemandSessionFrom
  , forkDemandSessionFromAuthorized
  , newDemandSession
  , newDemandSessionFrom
  , replaceDemandSessionSnapshot
  , snapshotDemandSession
  ) where

import Control.Concurrent
  ( MVar
  , ThreadId
  , modifyMVar
  , modifyMVar_
  , myThreadId
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , tryReadMVar
  )
import Control.Exception
  ( SomeException
  , displayException
  , evaluate
  , mask
  , try
  , uninterruptibleMask_
  )
import Control.Monad
  ( foldM )
import Data.List
  ( sortOn )
import qualified Data.Map.Strict as Map

import MyFramework.CURDE.Core
import MyFramework.CURDE.Expression
  ( ImplementationDecl (..)
  , RExprDecl (..)
  )
import MyFramework.CURDE.Types
  ( CURDE (..)
  , HandleDecl (..)
  , HandleId
  , ReadSource (..)
  , SchemaIdentity
  , renderHandleId
  )
import MyFramework.Handler.Internal
  ( CudeInvocation (..)
  , HandlerRegistry
  , ObservationCapture (..)
  , ReadInvocation (..)
  , invokeCude
  , invokeRead
  , normalizeRegisteredReadData
  )
import MyFramework.Runtime.Branch
  ( BranchSnapshot (..)
  )
import MyFramework.Runtime.Demand.Delta
  ( HandleActionDeltaConflict
  , projectHandleActionDelta
  )
import MyFramework.Runtime.Expression
  ( RExprEvaluationError
  , interpretRExprDecl
  )
import MyFramework.Runtime.Observation
  ( ObservationError (..)
  , emptyObservationStore
  , publishObservationFailureFor
  , publishObservationFor
  , readInputObservationDecl
  )
import MyFramework.Runtime.State
  ( RuntimeState
  , emptyRuntimeState
  , executionStatusFor
  , handlerInputFor
  , implementationStatusFor
  , putReadValue
  , readStatusFor
  , readValueFor
  , recordHandlerInvocation
  , recordObservationCaptured
  , recordObservationUnavailable
  , recordReadFailure
  , setExecutionStatus
  , setImplementationStatus
  , setReadStatus
  )
import MyFramework.Runtime.Types
import MyFramework.Runtime.Value
  ( RuntimeData
  , RuntimeDataBinding (..)
  )

data DemandContext = DemandContext
  { contextGraph :: DemandGraph
  , contextHandlers :: HandlerRegistry
  }

newtype DemandCoordinator = DemandCoordinator
  { coordinatorFlights ::
      MVar (Map.Map HandleId HandleFlight)
  }

data DemandSession = DemandSession
  { demandSessionContext :: DemandContext
  , demandSessionCoordinator :: DemandCoordinator
  , demandSessionSnapshot :: MVar BranchSnapshot
  , demandSessionBootRunId :: BootRunId
  , demandSessionExecutionPermit :: Maybe ExecutionPermit
  }

data HandleFlight = HandleFlight
  { handleFlightLeader :: ThreadId
  , handleFlightCompletion :: MVar HandleFlightCompletion
  }

data HandleFlightCompletion = HandleFlightCompletion
  { handleFlightBaseSnapshot :: BranchSnapshot
  , handleFlightFinalSnapshot :: BranchSnapshot
  , handleFlightOutcome :: Either RuntimeFailure ()
  }

data HandleFlightDecision
  = LeadHandle HandleFlight
  | FollowHandle HandleFlight
  | ReenteredHandle

data TargetPreparation
  = TargetPrepared
  | PrepareTarget

-- | Start one boot run. Every call creates a fresh HandleId coordinator, so
-- completed actions never leak into a later boot.
newDemandSession ::
  BootRunId ->
  HandlerRegistry ->
  DemandGraph ->
  IO DemandSession
newDemandSession currentBootRun currentHandlers currentGraph =
  newDemandSessionFrom
    currentBootRun
    currentHandlers
    currentGraph
    BranchSnapshot
      { branchRuntimeState = emptyRuntimeState
      , branchObservationStore = emptyObservationStore
      }

newDemandSessionFrom ::
  BootRunId ->
  HandlerRegistry ->
  DemandGraph ->
  BranchSnapshot ->
  IO DemandSession
newDemandSessionFrom
  currentBootRun
  currentHandlers
  currentGraph
  currentSnapshot = do
    currentState <- newMVar currentSnapshot
    currentCoordinator <-
      DemandCoordinator <$> newMVar Map.empty
    pure
      DemandSession
        { demandSessionContext =
            DemandContext
              { contextGraph = currentGraph
              , contextHandlers = currentHandlers
              }
        , demandSessionCoordinator = currentCoordinator
        , demandSessionSnapshot = currentState
        , demandSessionBootRunId = currentBootRun
        , demandSessionExecutionPermit = Nothing
        }

-- | Forks copy RuntimeState and ObservationStore into a new mutable cell.
-- Only the boot coordinator and immutable runtime registries are shared.
forkDemandSession :: DemandSession -> IO DemandSession
forkDemandSession currentSession = do
  currentSnapshot <-
    snapshotDemandSession currentSession
  forkDemandSessionFrom currentSession currentSnapshot

forkDemandSessionFrom ::
  DemandSession ->
  BranchSnapshot ->
  IO DemandSession
forkDemandSessionFrom currentSession currentSnapshot = do
  currentState <- newMVar currentSnapshot
  pure
    DemandSession
      { demandSessionContext =
          demandSessionContext currentSession
      , demandSessionCoordinator =
          demandSessionCoordinator currentSession
      , demandSessionSnapshot = currentState
      , demandSessionBootRunId =
          demandSessionBootRunId currentSession
      , demandSessionExecutionPermit =
          demandSessionExecutionPermit currentSession
      }

forkDemandSessionFromAuthorized ::
  ExecutionPermit ->
  DemandSession ->
  BranchSnapshot ->
  IO DemandSession
forkDemandSessionFromAuthorized currentPermit currentSession currentSnapshot = do
  nextSession <-
    forkDemandSessionFrom currentSession currentSnapshot
  pure
    nextSession
      { demandSessionExecutionPermit = Just currentPermit
      }

snapshotDemandSession :: DemandSession -> IO BranchSnapshot
snapshotDemandSession =
  readMVar . demandSessionSnapshot

replaceDemandSessionSnapshot ::
  DemandSession ->
  BranchSnapshot ->
  IO ()
replaceDemandSessionSnapshot currentSession nextSnapshot =
  modifyMVar_
    (demandSessionSnapshot currentSession)
    (const (pure nextSnapshot))

demandInSession ::
  DemandSession ->
  DemandNodeId ->
  IO (Either RuntimeFailure ())
demandInSession currentSession currentNode =
  case demandSessionExecutionPermit currentSession of
    Nothing ->
      pure
        ( Left
            ( invariantFailure
                "demand evaluation requires AST execution provenance"
            )
        )
    Just _ ->
      demandNode currentSession currentNode


demandNode ::
  DemandSession ->
  DemandNodeId ->
  IO (Either RuntimeFailure ())
demandNode currentSession currentId =
  case
      Map.lookup
        currentId
        ( demandGraphNodes
            (contextGraph (demandSessionContext currentSession))
        ) of
    Nothing ->
      pure
        ( Left
            (invariantFailure ("unknown demand node " ++ show currentId))
        )
    Just currentNode ->
      case currentNode of
        DemandHandleNode currentHandle
          | handleDeclKind currentHandle == R ->
              demandRead currentSession currentHandle
          | otherwise ->
              demandCommand currentSession currentHandle
        DemandImplementationNode
          currentImplementationId
          currentImplementation ->
            demandImplementation
              currentSession
              PrepareTarget
              currentImplementationId
              currentImplementation

demandRead ::
  DemandSession ->
  HandleDecl ->
  IO (Either RuntimeFailure ())
demandRead currentSession currentRead = do
  currentSnapshot <-
    snapshotDemandSession currentSession
  let currentId =
        handleDeclId currentRead
      currentStatus =
        readStatusFor
          currentId
          (branchRuntimeState currentSnapshot)
  case readEntryResult currentId currentStatus of
    Just currentResult ->
      pure currentResult
    Nothing -> do
      modifyRuntimeState currentSession
        (setReadStatus currentId ReadPending)
      currentDependency <-
        demandPrerequisites
          currentSession
          (HandleNode currentId)
          [InputDependency]
      case currentDependency of
        Left currentFailure ->
          failRead
            currentSession
            currentId
            (dependencyFailure currentId currentFailure)
        Right () ->
          runHandleAction
            currentSession
            currentRead
            ( \currentActionSession ->
                evaluateReadSourceAction
                  currentActionSession
                  currentRead
            )

readEntryResult ::
  HandleId ->
  ReadStatus ->
  Maybe (Either RuntimeFailure ())
readEntryResult currentId currentStatus =
  case currentStatus of
    ReadUnused ->
      Nothing
    ReadPending ->
      Just (Left (handleCycleFailure currentId))
    ReadRunning ->
      Just (Left (handleCycleFailure currentId))
    ReadAvailable ->
      Just (Right ())
    ReadObservationUnavailable currentFailure ->
      Just (Left currentFailure)
    ReadFailed currentFailure ->
      Just (Left currentFailure)

demandCommand ::
  DemandSession ->
  HandleDecl ->
  IO (Either RuntimeFailure ())
demandCommand currentSession currentHandle = do
  currentSnapshot <-
    snapshotDemandSession currentSession
  let currentId =
        handleDeclId currentHandle
      currentStatus =
        executionStatusFor
          currentId
          (branchRuntimeState currentSnapshot)
  case executionEntryResult currentId currentStatus of
    Just currentResult ->
      pure currentResult
    Nothing -> do
      modifyRuntimeState currentSession
        (setExecutionStatus currentId ExecutionPending)
      currentInputs <-
        demandPrerequisites
          currentSession
          (HandleNode currentId)
          [InputDependency]
      case currentInputs of
        Left currentFailure ->
          failExecution
            currentSession
            currentId
            (dependencyFailure currentId currentFailure)
        Right () ->
          case
              implementationPrerequisites
                (demandSessionContext currentSession)
                currentId of
            [ ( currentImplementationId
              , currentImplementation
              ) ] ->
                demandImplementation
                  currentSession
                  TargetPrepared
                  currentImplementationId
                  currentImplementation
            [] ->
              failExecution currentSession currentId
                ( invariantFailure
                    ( "missing Implementation for "
                        ++ renderHandleId currentId
                    )
                )
            _ ->
              failExecution currentSession currentId
                ( invariantFailure
                    ( "conflicting Implementations for "
                        ++ renderHandleId currentId
                    )
                )

executionEntryResult ::
  HandleId ->
  ExecutionStatus ->
  Maybe (Either RuntimeFailure ())
executionEntryResult currentId currentStatus =
  case currentStatus of
    ExecutionUnused ->
      Nothing
    ExecutionPending ->
      Just (Left (handleCycleFailure currentId))
    ExecutionRunning ->
      Just (Left (handleCycleFailure currentId))
    ExecutionSucceeded ->
      Just (Right ())
    ExecutionFailed currentFailure ->
      Just (Left currentFailure)
    ExecutionOutcomeUnknown currentFailure ->
      Just (Left currentFailure)

implementationPrerequisites ::
  DemandContext ->
  HandleId ->
  [(ImplementationId, ImplementationDecl)]
implementationPrerequisites currentContext currentId =
  [ (currentImplementationId, currentImplementation)
  | currentEdge <-
      prerequisitesFor
        currentContext
        (HandleNode currentId)
  , demandEdgeKind currentEdge == InvokeImplementation
  , ImplementationNode currentImplementationId <-
      [demandEdgePrerequisite currentEdge]
  , Just
      ( DemandImplementationNode
          _
          currentImplementation
        ) <-
      [ Map.lookup
          (ImplementationNode currentImplementationId)
          (demandGraphNodes (contextGraph currentContext))
      ]
  ]

demandImplementation ::
  DemandSession ->
  TargetPreparation ->
  ImplementationId ->
  ImplementationDecl ->
  IO (Either RuntimeFailure ())
demandImplementation
  currentSession
  currentPreparation
  currentImplementationId
  currentImplementation = do
    currentSnapshot <-
      snapshotDemandSession currentSession
    let currentState =
          branchRuntimeState currentSnapshot
        currentTargetId =
          implementationIdTarget currentImplementationId
        currentStatus =
          implementationStatusFor
            currentImplementationId
            currentState
    case
        implementationEntryResult
          currentImplementationId
          currentStatus of
      Just currentResult ->
        pure currentResult
      Nothing ->
        case
            prepareImplementationTarget
              currentPreparation
              currentTargetId
              currentState of
          Left currentFailure ->
            failImplementation
              currentSession
              currentImplementationId
              currentTargetId
              currentFailure
          Right shouldMarkPending -> do
            modifyRuntimeState currentSession
              ( setImplementationStatus
                  currentImplementationId
                  ImplementationPending
              )
            if shouldMarkPending
              then
                modifyRuntimeState currentSession
                  ( setExecutionStatus
                      currentTargetId
                      ExecutionPending
                  )
              else pure ()
            evaluateImplementation
              currentSession
              currentImplementationId
              currentImplementation

implementationEntryResult ::
  ImplementationId ->
  ImplementationStatus ->
  Maybe (Either RuntimeFailure ())
implementationEntryResult currentId currentStatus =
  case currentStatus of
    ImplementationUnused ->
      Nothing
    ImplementationPending ->
      Just (Left (implementationCycleFailure currentId))
    ImplementationRunning ->
      Just (Left (implementationCycleFailure currentId))
    ImplementationSucceeded ->
      Just (Right ())
    ImplementationFailed currentFailure ->
      Just (Left currentFailure)

prepareImplementationTarget ::
  TargetPreparation ->
  HandleId ->
  RuntimeState ->
  Either RuntimeFailure Bool
prepareImplementationTarget currentPreparation currentId currentState =
  case currentPreparation of
    TargetPrepared ->
      case executionStatusFor currentId currentState of
        ExecutionPending ->
          Right False
        currentStatus ->
          Left
            ( invariantFailure
                ( "prepared Implementation target is not Pending for "
                    ++ renderHandleId currentId
                    ++ ": "
                    ++ show currentStatus
                )
            )
    PrepareTarget ->
      case executionStatusFor currentId currentState of
        ExecutionUnused ->
          Right True
        ExecutionSucceeded ->
          Right False
        ExecutionFailed currentFailure ->
          Left currentFailure
        ExecutionOutcomeUnknown currentFailure ->
          Left currentFailure
        ExecutionPending ->
          Left (handleCycleFailure currentId)
        ExecutionRunning ->
          Left (handleCycleFailure currentId)

evaluateImplementation ::
  DemandSession ->
  ImplementationId ->
  ImplementationDecl ->
  IO (Either RuntimeFailure ())
evaluateImplementation
  currentSession
  currentImplementationId
  currentImplementation = do
    let currentTargetId =
          implementationIdTarget currentImplementationId
    currentDependencies <-
      demandPrerequisites
        currentSession
        (ImplementationNode currentImplementationId)
        [InputDependency, ArgumentUse (FieldPath [])]
    case currentDependencies of
      Left currentFailure ->
        failImplementation
          currentSession
          currentImplementationId
          currentTargetId
          (dependencyFailure currentTargetId currentFailure)
      Right () -> do
        currentSnapshot <-
          snapshotDemandSession currentSession
        currentEvaluation <-
          tryRExprEvaluation
            ( interpretRExprDecl
                ( expressionBindings
                    (branchRuntimeState currentSnapshot)
                    (implementationArguments currentImplementation)
                )
                (implementationArguments currentImplementation)
            )
        case currentEvaluation of
          Left currentException ->
            failImplementation
              currentSession
              currentImplementationId
              currentTargetId
              RuntimeFailure
                { runtimeFailurePhase = ArgumentBindingPhase
                , runtimeFailureCommitState = NoExternalCommit
                , runtimeFailureMessage = displayException currentException
                }
          Right (Left currentError) ->
            failImplementation
              currentSession
              currentImplementationId
              currentTargetId
              RuntimeFailure
                { runtimeFailurePhase = ArgumentBindingPhase
                , runtimeFailureCommitState = NoExternalCommit
                , runtimeFailureMessage = show currentError
                }
          Right (Right currentArguments) ->
            case lookupCommandHandle currentSession currentTargetId of
              Left currentFailure ->
                failImplementation
                  currentSession
                  currentImplementationId
                  currentTargetId
                  currentFailure
              Right currentTarget
                | handleDeclKind currentTarget
                    /= implementationDeclaredKind currentImplementation ->
                    failImplementation
                      currentSession
                      currentImplementationId
                      currentTargetId
                      ( invariantFailure
                          ( "Implementation kind does not match target "
                              ++ renderHandleId currentTargetId
                          )
                      )
                | otherwise -> do
                    modifyRuntimeState currentSession
                      ( setImplementationStatus
                          currentImplementationId
                          ImplementationRunning
                      )
                    currentResult <-
                      runHandleAction
                        currentSession
                        currentTarget
                        ( \currentActionSession ->
                            invokeCommandAction
                              currentActionSession
                              currentTarget
                              currentArguments
                        )
                    case currentResult of
                      Left currentFailure ->
                        failImplementation
                          currentSession
                          currentImplementationId
                          currentTargetId
                          currentFailure
                      Right () -> do
                        modifyRuntimeState currentSession
                          ( setImplementationStatus
                              currentImplementationId
                              ImplementationSucceeded
                          )
                        pure (Right ())

-- | The single-flight boundary starts only after every input and RExpr
-- prerequisite has completed. Its base/final snapshots therefore describe
-- one direct Handle action and never include nested dependency deltas.
runHandleAction ::
  DemandSession ->
  HandleDecl ->
  (DemandSession -> IO (Either RuntimeFailure ())) ->
  IO (Either RuntimeFailure ())
runHandleAction currentSession currentHandle currentAction = do
  currentSnapshot <-
    snapshotDemandSession currentSession
  case
      terminalHandleResult
        currentHandle
        (branchRuntimeState currentSnapshot) of
    Just currentResult ->
      pure currentResult
    Nothing -> do
      currentDecision <-
        claimHandleFlight
          currentSession
          (handleDeclId currentHandle)
      case currentDecision of
        ReenteredHandle ->
          pure
            ( Left
                (handleCycleFailure (handleDeclId currentHandle))
            )
        FollowHandle currentFlight ->
          awaitHandleFlight
            currentSession
            currentHandle
            currentFlight
        LeadHandle currentFlight ->
          runHandleLeader
            currentSession
            currentHandle
            currentFlight
            currentAction

claimHandleFlight ::
  DemandSession ->
  HandleId ->
  IO HandleFlightDecision
claimHandleFlight currentSession currentId = do
  currentThread <- myThreadId
  modifyMVar
    ( coordinatorFlights
        (demandSessionCoordinator currentSession)
    )
    ( \currentFlights ->
        case Map.lookup currentId currentFlights of
          Just currentFlight -> do
            currentCompletion <-
              tryReadMVar (handleFlightCompletion currentFlight)
            pure
              ( currentFlights
              , case currentCompletion of
                  Just _ ->
                    FollowHandle currentFlight
                  Nothing
                    | handleFlightLeader currentFlight
                        == currentThread ->
                        ReenteredHandle
                    | otherwise ->
                        FollowHandle currentFlight
              )
          Nothing -> do
            currentCompletion <- newEmptyMVar
            let currentFlight =
                  HandleFlight
                    { handleFlightLeader = currentThread
                    , handleFlightCompletion = currentCompletion
                    }
            pure
              ( Map.insert currentId currentFlight currentFlights
              , LeadHandle currentFlight
              )
    )

runHandleLeader ::
  DemandSession ->
  HandleDecl ->
  HandleFlight ->
  (DemandSession -> IO (Either RuntimeFailure ())) ->
  IO (Either RuntimeFailure ())
runHandleLeader
  currentSession
  currentHandle
  currentFlight
  currentAction =
    mask
      ( \restore -> do
          currentBase <-
            snapshotDemandSession currentSession
          -- The direct action runs in a private cell cloned after all
          -- prerequisites. Its final delta therefore cannot absorb unrelated
          -- branch-local dependency or Implementation updates.
          currentActionSession <-
            forkDemandSessionFrom currentSession currentBase
          currentRawOutcome <-
            tryHandleAction
              (restore (currentAction currentActionSession))
          currentOutcome <-
            case currentRawOutcome of
              Right currentResult ->
                pure currentResult
              Left currentException -> do
                let currentFailure =
                      unexpectedHandleActionFailure
                        (handleDeclId currentHandle)
                        currentException
                recordUnhandledHandleFailure
                  currentActionSession
                  currentHandle
                  currentFailure
                pure (Left currentFailure)
          currentFinal <-
            snapshotDemandSession currentActionSession
          let currentCompletion =
                HandleFlightCompletion
                  { handleFlightBaseSnapshot = currentBase
                  , handleFlightFinalSnapshot = currentFinal
                  , handleFlightOutcome = currentOutcome
                  }
          -- The completion is published for success, ordinary failure,
          -- exception, and asynchronous cancellation alike.
          uninterruptibleMask_
            ( putMVar
                (handleFlightCompletion currentFlight)
                currentCompletion
            )
          deliverHandleCompletion
            currentSession
            currentHandle
            currentCompletion
      )

awaitHandleFlight ::
  DemandSession ->
  HandleDecl ->
  HandleFlight ->
  IO (Either RuntimeFailure ())
awaitHandleFlight currentSession currentHandle currentFlight = do
  currentCompletion <-
    readMVar (handleFlightCompletion currentFlight)
  deliverHandleCompletion
    currentSession
    currentHandle
    currentCompletion

deliverHandleCompletion ::
  DemandSession ->
  HandleDecl ->
  HandleFlightCompletion ->
  IO (Either RuntimeFailure ())
deliverHandleCompletion currentSession currentHandle currentCompletion = do
  currentSnapshot <-
    snapshotDemandSession currentSession
  case
      terminalHandleResult
        currentHandle
        (branchRuntimeState currentSnapshot) of
    Just currentResult ->
      pure currentResult
    Nothing -> do
      currentProjection <-
        modifyMVar
          (demandSessionSnapshot currentSession)
          ( \nextCurrent ->
              case
                  projectHandleActionDelta
                    (handleFlightBaseSnapshot currentCompletion)
                    nextCurrent
                    (handleFlightFinalSnapshot currentCompletion) of
                Left currentConflicts ->
                  pure
                    ( nextCurrent
                    , Left
                        (handleFlightMergeFailure currentConflicts)
                    )
                Right nextSnapshot ->
                  pure (nextSnapshot, Right ())
          )
      case currentProjection of
        Left currentFailure -> do
          recordUnhandledHandleFailure
            currentSession
            currentHandle
            currentFailure
          pure (Left currentFailure)
        Right () ->
          pure (handleFlightOutcome currentCompletion)

terminalHandleResult ::
  HandleDecl ->
  RuntimeState ->
  Maybe (Either RuntimeFailure ())
terminalHandleResult currentHandle currentState
  | handleDeclKind currentHandle == R =
      case
          readStatusFor
            (handleDeclId currentHandle)
            currentState of
        ReadAvailable ->
          Just (Right ())
        ReadObservationUnavailable currentFailure ->
          Just (Left currentFailure)
        ReadFailed currentFailure ->
          Just (Left currentFailure)
        _ ->
          Nothing
  | otherwise =
      case
          executionStatusFor
            (handleDeclId currentHandle)
            currentState of
        ExecutionSucceeded ->
          Just (Right ())
        ExecutionFailed currentFailure ->
          Just (Left currentFailure)
        ExecutionOutcomeUnknown currentFailure ->
          Just (Left currentFailure)
        _ ->
          Nothing

tryHandleAction ::
  IO (Either RuntimeFailure ()) ->
  IO
    ( Either
        SomeException
        (Either RuntimeFailure ())
    )
tryHandleAction =
  try

recordUnhandledHandleFailure ::
  DemandSession ->
  HandleDecl ->
  RuntimeFailure ->
  IO ()
recordUnhandledHandleFailure currentSession currentHandle currentFailure
  | handleDeclKind currentHandle == R =
      modifyRuntimeState currentSession
        (recordReadFailure (handleDeclId currentHandle) currentFailure)
  | otherwise =
      modifyRuntimeState currentSession
        (setExecutionFailure (handleDeclId currentHandle) currentFailure)

evaluateReadSourceAction ::
  DemandSession ->
  HandleDecl ->
  IO (Either RuntimeFailure ())
evaluateReadSourceAction currentSession currentRead = do
  modifyRuntimeState currentSession
    (setReadStatus currentId ReadRunning)
  currentSnapshot <-
    snapshotDemandSession currentSession
  let currentState =
        branchRuntimeState currentSnapshot
      currentInput =
        handlerInputFor
          (handleDeclInput currentRead)
          currentState
      currentHandlers =
        contextHandlers (demandSessionContext currentSession)
  case handleDeclReadSource currentRead of
    Nothing ->
      failRead currentSession currentId
        ( invariantFailure
            ("R handle has no read source: " ++ renderHandleId currentId)
        )
    Just ReadFromHandler -> do
      case executionPermitFor currentSession of
        Left currentFailure ->
          failRead currentSession currentId currentFailure
        Right currentPermit -> do
          modifyRuntimeState currentSession
            ( recordHandlerInvocation
                (executionPermitProvenance currentPermit)
                (HandleNode currentId)
                currentId
            )
          currentInvocation <-
            invokeRead currentPermit currentHandlers currentId currentInput
          case currentInvocation of
            ReadInvocationSucceeded currentValue ->
              publishReadValue currentSession currentRead currentValue
            ReadInvocationSourceRequired currentSource ->
              failRead currentSession currentId
                ( invariantFailure
                    ( "registered R handler unexpectedly requires source "
                        ++ show currentSource
                        ++ ": "
                        ++ renderHandleId currentId
                    )
                )
            ReadInvocationFailed currentFailure ->
              failRead currentSession currentId currentFailure
    Just ReadFromInputValue ->
      case handleDeclInput currentRead of
        Nothing ->
          failRead currentSession currentId
            ( invariantFailure
                ( "value-source R has no input: "
                    ++ renderHandleId currentId
                )
            )
        Just currentSource ->
          case readValueFor currentSource currentState of
            Nothing ->
              failRead currentSession currentId
                ( invariantFailure
                    ( "input R value unavailable for "
                        ++ renderHandleId currentId
                        ++ ": "
                        ++ renderHandleId currentSource
                    )
                )
            Just currentValue ->
              normalizeAndPublishRead
                currentSession
                currentRead
                currentValue
    Just ReadFromInputObservation ->
      case
          readInputObservationDecl
            currentRead
            (branchObservationStore currentSnapshot) of
        Left currentError ->
          failObservationRead
            currentSession
            currentId
            currentError
        Right currentValue ->
          normalizeAndPublishRead
            currentSession
            currentRead
            currentValue
  where
    currentId =
      handleDeclId currentRead

normalizeAndPublishRead ::
  DemandSession ->
  HandleDecl ->
  RuntimeData ->
  IO (Either RuntimeFailure ())
normalizeAndPublishRead currentSession currentRead currentValue =
  case
      normalizeRegisteredReadData
        (contextHandlers (demandSessionContext currentSession))
        (handleDeclId currentRead)
        currentValue of
    Left currentFailure ->
      failRead
        currentSession
        (handleDeclId currentRead)
        currentFailure
    Right normalizedValue ->
      publishReadValue currentSession currentRead normalizedValue

publishReadValue ::
  DemandSession ->
  HandleDecl ->
  RuntimeData ->
  IO (Either RuntimeFailure ())
publishReadValue currentSession currentRead currentValue =
  case handleDeclPublicValueSchema currentRead of
    Nothing ->
      failRead currentSession currentId
        ( invariantFailure
            ("R handle has no public schema: " ++ renderHandleId currentId)
        )
    Just currentSchema -> do
      currentResult <-
        modifyRuntimeStateEither
          currentSession
          ( putReadValue
              currentId
              RuntimeDataBinding
                { runtimeDataBindingHandle = currentId
                , runtimeDataBindingSchema = currentSchema
                , runtimeDataBindingValue = currentValue
                }
          )
      case currentResult of
        Left currentFailure ->
          failRead currentSession currentId currentFailure
        Right () ->
          pure (Right ())
  where
    currentId =
      handleDeclId currentRead

failObservationRead ::
  DemandSession ->
  HandleId ->
  ObservationError ->
  IO (Either RuntimeFailure ())
failObservationRead currentSession currentId currentError = do
  let currentFailure =
        RuntimeFailure
          { runtimeFailurePhase = ReadPhase
          , runtimeFailureCommitState = NoExternalCommit
          , runtimeFailureMessage = show currentError
          }
      nextStatus =
        case currentError of
          ObservationValueMissing _ _ ->
            ReadObservationUnavailable currentFailure
          ObservationSourceFailed _ _ _ ->
            ReadObservationUnavailable currentFailure
          _ ->
            ReadFailed currentFailure
  modifyRuntimeState currentSession
    (setReadStatus currentId nextStatus)
  pure (Left currentFailure)

invokeCommandAction ::
  DemandSession ->
  HandleDecl ->
  RuntimeData ->
  IO (Either RuntimeFailure ())
invokeCommandAction currentSession currentHandle currentArguments = do
  modifyRuntimeState currentSession
    (setExecutionStatus currentId ExecutionRunning)
  currentSnapshot <-
    snapshotDemandSession currentSession
  let currentInput =
        handlerInputFor
          (handleDeclInput currentHandle)
          (branchRuntimeState currentSnapshot)
  case executionPermitFor currentSession of
    Left currentFailure -> do
      modifyRuntimeState currentSession
        (setExecutionFailure currentId currentFailure)
      pure (Left currentFailure)
    Right currentPermit -> do
      modifyRuntimeState currentSession
        ( recordHandlerInvocation
            (executionPermitProvenance currentPermit)
            (HandleNode currentId)
            currentId
        )
      currentInvocation <-
        invokeCude
          currentPermit
          (contextHandlers (demandSessionContext currentSession))
          currentId
          currentInput
          currentArguments
      case currentInvocation of
        CudeInvocationFailed currentFailure -> do
          modifyRuntimeState currentSession
            (setExecutionFailure currentId currentFailure)
          pure (Left currentFailure)
        CudeInvocationSucceeded currentCapture -> do
          -- CUDE settlement becomes terminal before private observation
          -- processing. Capture failure cannot overwrite this success.
          modifyRuntimeState currentSession
            (setExecutionStatus currentId ExecutionSucceeded)
          recordObservationCapture
            currentSession
            currentHandle
            currentCapture
          pure (Right ())
  where
    currentId =
      handleDeclId currentHandle

executionPermitFor ::
  DemandSession ->
  Either RuntimeFailure ExecutionPermit
executionPermitFor currentSession =
  case demandSessionExecutionPermit currentSession of
    Nothing ->
      Left
        ( invariantFailure
            "handler invocation requires AST execution provenance"
        )
    Just currentPermit ->
      Right currentPermit

recordObservationCapture ::
  DemandSession ->
  HandleDecl ->
  ObservationCapture ->
  IO ()
recordObservationCapture currentSession currentHandle currentCapture =
  case currentCapture of
    ObservationDiscarded ->
      pure ()
    ObservationCaptured currentValue -> do
      currentPublish <-
        modifyMVar
          (demandSessionSnapshot currentSession)
          ( \currentSnapshot ->
              case
                  publishObservationFor
                    currentHandle
                    currentValue
                    (branchObservationStore currentSnapshot) of
                Left currentError ->
                  pure (currentSnapshot, Left currentError)
                Right currentStore ->
                  let nextSnapshot =
                        currentSnapshot
                          { branchRuntimeState =
                              recordObservationCaptured
                                (handleDeclId currentHandle)
                                (branchRuntimeState currentSnapshot)
                          , branchObservationStore = currentStore
                          }
                   in pure (nextSnapshot, Right ())
          )
      case currentPublish of
        Left currentError ->
          recordObservationFailure
            currentSession
            currentHandle
            (observationFailure currentError)
        Right () ->
          pure ()
    ObservationCaptureFailed currentFailure ->
      recordObservationFailure
        currentSession
        currentHandle
        currentFailure

recordObservationFailure ::
  DemandSession ->
  HandleDecl ->
  RuntimeFailure ->
  IO ()
recordObservationFailure currentSession currentHandle currentFailure =
  modifyMVar_
    (demandSessionSnapshot currentSession)
    ( \currentSnapshot ->
        let nextStore =
              case
                  publishObservationFailureFor
                    currentHandle
                    currentFailure
                    (branchObservationStore currentSnapshot) of
                Left _ ->
                  branchObservationStore currentSnapshot
                Right currentStore ->
                  currentStore
         in pure
              currentSnapshot
                { branchRuntimeState =
                    recordObservationUnavailable
                      (handleDeclId currentHandle)
                      currentFailure
                      (branchRuntimeState currentSnapshot)
                , branchObservationStore = nextStore
                }
    )

demandPrerequisites ::
  DemandSession ->
  DemandNodeId ->
  [DemandEdgeKind] ->
  IO (Either RuntimeFailure ())
demandPrerequisites currentSession currentId currentKinds =
  foldM demandOne (Right ()) currentEdges
  where
    currentEdges =
      sortOn edgePriority
        [ currentEdge
        | currentEdge <-
            prerequisitesFor
              (demandSessionContext currentSession)
              currentId
        , edgeKindIncluded
            (demandEdgeKind currentEdge)
            currentKinds
        ]
    demandOne (Left currentFailure) _ =
      pure (Left currentFailure)
    demandOne (Right ()) currentEdge =
      demandNode
        currentSession
        (demandEdgePrerequisite currentEdge)

edgeKindIncluded ::
  DemandEdgeKind ->
  [DemandEdgeKind] ->
  Bool
edgeKindIncluded currentKind currentKinds =
  any sameKind currentKinds
  where
    sameKind expectedKind =
      case (currentKind, expectedKind) of
        (InvokeImplementation, InvokeImplementation) ->
          True
        (InputDependency, InputDependency) ->
          True
        (ArgumentUse _, ArgumentUse _) ->
          True
        _ ->
          False

edgePriority :: DemandEdge -> Int
edgePriority currentEdge =
  case demandEdgeKind currentEdge of
    InputDependency ->
      0
    ArgumentUse _ ->
      1
    InvokeImplementation ->
      2

prerequisitesFor ::
  DemandContext ->
  DemandNodeId ->
  [DemandEdge]
prerequisitesFor currentContext currentId =
  [ currentEdge
  | currentEdge <-
      demandGraphEdges (contextGraph currentContext)
  , demandEdgeDependent currentEdge == currentId
  ]

lookupCommandHandle ::
  DemandSession ->
  HandleId ->
  Either RuntimeFailure HandleDecl
lookupCommandHandle currentSession currentId =
  case
      Map.lookup
        (HandleNode currentId)
        ( demandGraphNodes
            (contextGraph (demandSessionContext currentSession))
        ) of
    Just (DemandHandleNode currentHandle)
      | handleDeclKind currentHandle /= R ->
          Right currentHandle
    _ ->
      Left
        ( invariantFailure
            ("missing CUDE target handle " ++ renderHandleId currentId)
        )

expressionBindings ::
  RuntimeState ->
  RExprDecl ->
  [RuntimeDataBinding]
expressionBindings currentState currentExpression =
  [ RuntimeDataBinding
      { runtimeDataBindingHandle = currentId
      , runtimeDataBindingSchema = currentSchema
      , runtimeDataBindingValue = currentValue
      }
  | (currentId, currentSchema) <-
      uniqueReferences (expressionReferences currentExpression)
  , Just currentValue <- [readValueFor currentId currentState]
  ]

expressionReferences ::
  RExprDecl ->
  [(HandleId, SchemaIdentity)]
expressionReferences currentExpression =
  case currentExpression of
    RReferenceDecl currentId currentSchema ->
      [(currentId, currentSchema)]
    RLiteralDecl _ _ ->
      []
    RProductDecl _ currentFields ->
      concatMap
        (expressionReferences . snd)
        currentFields

uniqueReferences ::
  [(HandleId, SchemaIdentity)] ->
  [(HandleId, SchemaIdentity)]
uniqueReferences =
  Map.toAscList . Map.fromList

tryRExprEvaluation ::
  Either RExprEvaluationError RuntimeData ->
  IO
    ( Either
        SomeException
        (Either RExprEvaluationError RuntimeData)
    )
tryRExprEvaluation =
  try . evaluate

failRead ::
  DemandSession ->
  HandleId ->
  RuntimeFailure ->
  IO (Either RuntimeFailure ())
failRead currentSession currentId currentFailure = do
  modifyRuntimeState currentSession
    (recordReadFailure currentId currentFailure)
  pure (Left currentFailure)

failExecution ::
  DemandSession ->
  HandleId ->
  RuntimeFailure ->
  IO (Either RuntimeFailure ())
failExecution currentSession currentId currentFailure = do
  modifyRuntimeState currentSession
    (setExecutionFailure currentId currentFailure)
  pure (Left currentFailure)

failImplementation ::
  DemandSession ->
  ImplementationId ->
  HandleId ->
  RuntimeFailure ->
  IO (Either RuntimeFailure ())
failImplementation
  currentSession
  currentImplementationId
  currentTargetId
  currentFailure = do
    modifyRuntimeState currentSession
      ( setImplementationStatus
          currentImplementationId
          (ImplementationFailed currentFailure)
      )
    currentSnapshot <-
      snapshotDemandSession currentSession
    case
        executionStatusFor
          currentTargetId
          (branchRuntimeState currentSnapshot) of
      ExecutionSucceeded ->
        pure ()
      ExecutionFailed _ ->
        pure ()
      ExecutionOutcomeUnknown _ ->
        pure ()
      _ ->
        modifyRuntimeState currentSession
          (setExecutionFailure currentTargetId currentFailure)
    pure (Left currentFailure)

setExecutionFailure ::
  HandleId ->
  RuntimeFailure ->
  RuntimeState ->
  RuntimeState
setExecutionFailure currentId currentFailure =
  setExecutionStatus
    currentId
    ( case runtimeFailureCommitState currentFailure of
        ExternalCommitUnknown ->
          ExecutionOutcomeUnknown currentFailure
        _ ->
          ExecutionFailed currentFailure
    )

modifyRuntimeState ::
  DemandSession ->
  (RuntimeState -> RuntimeState) ->
  IO ()
modifyRuntimeState currentSession currentUpdate =
  modifyMVar_
    (demandSessionSnapshot currentSession)
    ( \currentSnapshot ->
        pure
          currentSnapshot
            { branchRuntimeState =
                currentUpdate (branchRuntimeState currentSnapshot)
            }
    )

modifyRuntimeStateEither ::
  DemandSession ->
  (RuntimeState -> Either RuntimeFailure RuntimeState) ->
  IO (Either RuntimeFailure ())
modifyRuntimeStateEither currentSession currentUpdate =
  modifyMVar
    (demandSessionSnapshot currentSession)
    ( \currentSnapshot ->
        case currentUpdate (branchRuntimeState currentSnapshot) of
          Left currentFailure ->
            pure (currentSnapshot, Left currentFailure)
          Right nextState ->
            pure
              ( currentSnapshot
                  { branchRuntimeState = nextState
                  }
              , Right ()
              )
    )

dependencyFailure ::
  HandleId ->
  RuntimeFailure ->
  RuntimeFailure
dependencyFailure currentId currentCause =
  RuntimeFailure
    { runtimeFailurePhase = DependencyPhase
    , runtimeFailureCommitState =
        runtimeFailureCommitState currentCause
    , runtimeFailureMessage =
        "dependency failed for "
          ++ renderHandleId currentId
          ++ ": "
          ++ runtimeFailureMessage currentCause
    }

observationFailure ::
  ObservationError ->
  RuntimeFailure
observationFailure currentError =
  RuntimeFailure
    { runtimeFailurePhase = ObservationPhase
    , runtimeFailureCommitState = ExternalCommitted
    , runtimeFailureMessage = show currentError
    }

handleCycleFailure :: HandleId -> RuntimeFailure
handleCycleFailure currentId =
  invariantFailure
    ("HandleId re-entered pending/running action " ++ renderHandleId currentId)

implementationCycleFailure ::
  ImplementationId ->
  RuntimeFailure
implementationCycleFailure currentId =
  invariantFailure
    ("Implementation re-entered pending/running binding " ++ show currentId)

unexpectedHandleActionFailure ::
  HandleId ->
  SomeException ->
  RuntimeFailure
unexpectedHandleActionFailure currentId currentException =
  RuntimeFailure
    { runtimeFailurePhase = RuntimeInvariantPhase
    , runtimeFailureCommitState = ExternalCommitUnknown
    , runtimeFailureMessage =
        "Handle action raised for "
          ++ renderHandleId currentId
          ++ ": "
          ++ displayException currentException
    }

handleFlightMergeFailure ::
  [HandleActionDeltaConflict] ->
  RuntimeFailure
handleFlightMergeFailure currentConflicts =
  RuntimeFailure
    { runtimeFailurePhase = RuntimeInvariantPhase
    , runtimeFailureCommitState = ExternalCommitUnknown
    , runtimeFailureMessage =
        "completed Handle action delta conflict: "
          ++ show currentConflicts
    }

invariantFailure :: String -> RuntimeFailure
invariantFailure currentMessage =
  RuntimeFailure
    { runtimeFailurePhase = RuntimeInvariantPhase
    , runtimeFailureCommitState = NoExternalCommit
    , runtimeFailureMessage = currentMessage
    }
