module MyFramework.Runtime.Cancellation
  ( normalizeCancelledState
  ) where

import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.CURDE.Core
  ( ImplementationId (..)
  )
import MyFramework.Runtime.State
  ( RuntimeEvent (..)
  , RuntimeState
  , RuntimeStateParts (..)
  , runtimeStateFromParts
  , runtimeStateParts
  )
import MyFramework.Runtime.Types

-- | Normalize only work that became Pending/Running in the branch relative
-- to its fork base. Pre-existing active work and already-settled outcomes are
-- left untouched.
normalizeCancelledState :: RuntimeState -> RuntimeState -> RuntimeState
normalizeCancelledState currentBase currentBranch =
  runtimeStateFromParts
    branchParts
      { runtimePartExecutionStatuses = nextExecution
      , runtimePartReadStatuses = nextReads
      , runtimePartValidities = nextValidities
      , runtimePartImplementationStatuses = nextImplementations
      , runtimePartReadValues = nextReadValues
      , runtimePartEvents =
          runtimePartEvents branchParts ++ normalizationEvents
      }
  where
    baseParts =
      runtimeStateParts currentBase
    branchParts =
      runtimeStateParts currentBranch

    cancelledExecutions =
      newlyActive
        executionActive
        (runtimePartExecutionStatuses baseParts)
        (runtimePartExecutionStatuses branchParts)
    cancelledReads =
      newlyActive
        readActive
        (runtimePartReadStatuses baseParts)
        (runtimePartReadStatuses branchParts)
    cancelledImplementations =
      newlyActive
        implementationActive
        (runtimePartImplementationStatuses baseParts)
        (runtimePartImplementationStatuses branchParts)

    nextExecution =
      foldl
        (\currentMap currentId ->
            Map.insert
              currentId
              (ExecutionOutcomeUnknown executionCancellationFailure)
              currentMap
        )
        (runtimePartExecutionStatuses branchParts)
        cancelledExecutions

    nextReads =
      foldl
        (\currentMap currentId ->
            Map.insert
              currentId
              (ReadFailed readCancellationFailure)
              currentMap
        )
        (runtimePartReadStatuses branchParts)
        cancelledReads

    nextImplementations =
      foldl
        (\currentMap currentId ->
            Map.insert
              currentId
              (ImplementationFailed implementationCancellationFailure)
              currentMap
        )
        (runtimePartImplementationStatuses branchParts)
        cancelledImplementations

    suspectHandles =
      Set.toAscList
        ( Set.fromList cancelledExecutions
            `Set.union` Set.fromList cancelledReads
            `Set.union` Set.fromList
              (map implementationIdTarget cancelledImplementations)
        )
    nextValidities =
      foldl
        (\currentMap currentId ->
            Map.insert currentId Suspect currentMap
        )
        (runtimePartValidities branchParts)
        suspectHandles
    nextReadValues =
      foldl
        (flip Map.delete)
        (runtimePartReadValues branchParts)
        cancelledReads

    normalizationEvents =
      map
        (\currentId ->
            ExecutionStatusChanged
              currentId
              (ExecutionOutcomeUnknown executionCancellationFailure)
        )
        cancelledExecutions
        ++ map
          (\currentId ->
              ReadStatusChanged
                currentId
                (ReadFailed readCancellationFailure)
          )
          cancelledReads
        ++ map
          (\currentId ->
              ImplementationStatusChanged
                currentId
                (ImplementationFailed implementationCancellationFailure)
          )
          cancelledImplementations
        ++ map
          (\currentId -> ValidityChanged currentId Suspect)
          suspectHandles

newlyActive ::
  (Ord key, Eq status) =>
  (status -> Bool) ->
  Map key status ->
  Map key status ->
  [key]
newlyActive isActive currentBase currentBranch =
  [ currentKey
  | (currentKey, currentStatus) <- Map.toAscList currentBranch
  , isActive currentStatus
  , Map.lookup currentKey currentBase /= Just currentStatus
  ]

executionActive :: ExecutionStatus -> Bool
executionActive ExecutionPending =
  True
executionActive ExecutionRunning =
  True
executionActive _ =
  False

readActive :: ReadStatus -> Bool
readActive ReadPending =
  True
readActive ReadRunning =
  True
readActive _ =
  False

implementationActive :: ImplementationStatus -> Bool
implementationActive ImplementationPending =
  True
implementationActive ImplementationRunning =
  True
implementationActive _ =
  False

executionCancellationFailure :: RuntimeFailure
executionCancellationFailure =
  RuntimeFailure
    { runtimeFailurePhase = HandlerPhase
    , runtimeFailureCommitState = ExternalCommitUnknown
    , runtimeFailureMessage =
        "cancelled CUDE execution did not publish a settled outcome"
    }

readCancellationFailure :: RuntimeFailure
readCancellationFailure =
  RuntimeFailure
    { runtimeFailurePhase = ReadPhase
    , runtimeFailureCommitState = NoExternalCommit
    , runtimeFailureMessage =
        "cancelled R evaluation did not publish a settled outcome"
    }

implementationCancellationFailure :: RuntimeFailure
implementationCancellationFailure =
  RuntimeFailure
    { runtimeFailurePhase = ImplementationPhase
    , runtimeFailureCommitState = ExternalCommitUnknown
    , runtimeFailureMessage =
        "cancelled implementation did not publish a settled outcome"
    }
