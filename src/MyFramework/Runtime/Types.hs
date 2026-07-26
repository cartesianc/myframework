module MyFramework.Runtime.Types
  ( CommitState (..)
  , ExecutionStatus (..)
  , FailurePhase (..)
  , HandlerInput
  , ImplementationStatus (..)
  , InputSnapshot
  , ReadStatus (..)
  , RuntimeFailure (..)
  , Validity (..)
  , executionSucceeded
  , handlerInput
  , handlerInputExecutionStatus
  , handlerInputHandleId
  , handlerInputReadStatus
  , handlerInputSnapshot
  , handlerInputValidity
  , inputSnapshotExecutionStatus
  , inputSnapshotHandleId
  , inputSnapshotReadStatus
  , inputSnapshotValidity
  , noHandlerInput
  , readAvailable
  ) where

import MyFramework.CURDE.Types
  ( HandleId )

data CommitState
  = NoExternalCommit
  | ExternalCommitUnknown
  | ExternalCommitted
  deriving (Eq, Ord, Show)

data FailurePhase
  = DependencyPhase
  | ArgumentBindingPhase
  | HandlerPhase
  | ObservationPhase
  | ReadPhase
  | ImplementationPhase
  | RuntimeInvariantPhase
  deriving (Eq, Ord, Show)

data RuntimeFailure = RuntimeFailure
  { runtimeFailurePhase :: FailurePhase
  , runtimeFailureCommitState :: CommitState
  , runtimeFailureMessage :: String
  }
  deriving (Eq, Show)

data ExecutionStatus
  = ExecutionUnused
  | ExecutionPending
  | ExecutionRunning
  | ExecutionSucceeded
  | ExecutionFailed RuntimeFailure
  | ExecutionOutcomeUnknown RuntimeFailure
  deriving (Eq, Show)

data ReadStatus
  = ReadUnused
  | ReadPending
  | ReadRunning
  | ReadAvailable
  | ReadObservationUnavailable RuntimeFailure
  | ReadFailed RuntimeFailure
  deriving (Eq, Show)

data ImplementationStatus
  = ImplementationUnused
  | ImplementationPending
  | ImplementationRunning
  | ImplementationSucceeded
  | ImplementationFailed RuntimeFailure
  deriving (Eq, Show)

data Validity
  = Trusted
  | Suspect
  | Invalidated
  | TaintedBy HandleId
  deriving (Eq, Show)

-- | A handler can see that its declared input exists and inspect only runtime
-- identity/status. Business data never enters through this channel.
data InputSnapshot = InputSnapshot
  { inputSnapshotHandleId :: HandleId
  , inputSnapshotExecutionStatus :: Maybe ExecutionStatus
  , inputSnapshotReadStatus :: Maybe ReadStatus
  , inputSnapshotValidity :: Validity
  }
  deriving (Eq, Show)

data HandlerInput
  = NoHandlerInput
  | HandlerInputSnapshot InputSnapshot
  deriving (Eq, Show)

noHandlerInput :: HandlerInput
noHandlerInput =
  NoHandlerInput

handlerInput ::
  HandleId ->
  Maybe ExecutionStatus ->
  Maybe ReadStatus ->
  Validity ->
  HandlerInput
handlerInput currentId currentExecution currentRead currentValidity =
  HandlerInputSnapshot
    InputSnapshot
      { inputSnapshotHandleId = currentId
      , inputSnapshotExecutionStatus = currentExecution
      , inputSnapshotReadStatus = currentRead
      , inputSnapshotValidity = currentValidity
      }

handlerInputSnapshot :: HandlerInput -> Maybe InputSnapshot
handlerInputSnapshot NoHandlerInput =
  Nothing
handlerInputSnapshot (HandlerInputSnapshot currentSnapshot) =
  Just currentSnapshot

handlerInputHandleId :: HandlerInput -> Maybe HandleId
handlerInputHandleId =
  fmap inputSnapshotHandleId . handlerInputSnapshot

handlerInputExecutionStatus ::
  HandlerInput ->
  Maybe ExecutionStatus
handlerInputExecutionStatus currentInput =
  handlerInputSnapshot currentInput
    >>= inputSnapshotExecutionStatus

handlerInputReadStatus :: HandlerInput -> Maybe ReadStatus
handlerInputReadStatus currentInput =
  handlerInputSnapshot currentInput
    >>= inputSnapshotReadStatus

handlerInputValidity :: HandlerInput -> Maybe Validity
handlerInputValidity =
  fmap inputSnapshotValidity . handlerInputSnapshot

executionSucceeded :: ExecutionStatus -> Bool
executionSucceeded ExecutionSucceeded =
  True
executionSucceeded _ =
  False

readAvailable :: ReadStatus -> Bool
readAvailable ReadAvailable =
  True
readAvailable _ =
  False
