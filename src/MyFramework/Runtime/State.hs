module MyFramework.Runtime.State
  ( RuntimeEvent (..)
  , RuntimeState
  , RuntimeStateParts (..)
  , emptyRuntimeState
  , executionStatusFor
  , executionStatuses
  , handlerInputFor
  , implementationStatusFor
  , implementationStatuses
  , putReadValue
  , readStatusFor
  , readStatuses
  , readValueFor
  , recordHandlerInvocation
  , recordObservationCaptured
  , recordObservationUnavailable
  , recordReadFailure
  , runtimeStateFromParts
  , runtimeEvents
  , runtimeStateParts
  , setExecutionStatus
  , setImplementationStatus
  , setReadStatus
  , setValidity
  , validityFor
  , validities
  ) where

import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map

import MyFramework.CURDE.Core
  ( DemandNodeId
  , ImplementationId
  )
import MyFramework.CURDE.Types
  ( HandleId )
import MyFramework.Runtime.Types
import MyFramework.Runtime.Value
  ( RuntimeData
  , RuntimeDataBinding (..)
  )

data RuntimeEvent
  = ExecutionStatusChanged HandleId ExecutionStatus
  | ReadStatusChanged HandleId ReadStatus
  | ImplementationStatusChanged ImplementationId ImplementationStatus
  | ValidityChanged HandleId Validity
  | ReadValueAvailable HandleId
  | RuntimeObservationCaptured HandleId
  | RuntimeObservationUnavailable HandleId RuntimeFailure
  | HandlerInvocationAuthorized
      ExecutionProvenance
      DemandNodeId
      HandleId
  deriving (Eq, Show)

data RuntimeState = RuntimeState
  { stateExecutionStatuses :: Map HandleId ExecutionStatus
  , stateReadStatuses :: Map HandleId ReadStatus
  , stateValidities :: Map HandleId Validity
  , stateImplementationStatuses ::
      Map ImplementationId ImplementationStatus
  , stateReadValues :: Map HandleId RuntimeDataBinding
  , stateRuntimeEvents :: [RuntimeEvent]
  }
  deriving (Eq, Show)

data RuntimeStateParts = RuntimeStateParts
  { runtimePartExecutionStatuses :: Map HandleId ExecutionStatus
  , runtimePartReadStatuses :: Map HandleId ReadStatus
  , runtimePartValidities :: Map HandleId Validity
  , runtimePartImplementationStatuses ::
      Map ImplementationId ImplementationStatus
  , runtimePartReadValues :: Map HandleId RuntimeDataBinding
  , runtimePartEvents :: [RuntimeEvent]
  }
  deriving (Eq, Show)

emptyRuntimeState :: RuntimeState
emptyRuntimeState =
  RuntimeState
    { stateExecutionStatuses = Map.empty
    , stateReadStatuses = Map.empty
    , stateValidities = Map.empty
    , stateImplementationStatuses = Map.empty
    , stateReadValues = Map.empty
    , stateRuntimeEvents = []
    }

runtimeStateParts :: RuntimeState -> RuntimeStateParts
runtimeStateParts currentState =
  RuntimeStateParts
    { runtimePartExecutionStatuses =
        stateExecutionStatuses currentState
    , runtimePartReadStatuses =
        stateReadStatuses currentState
    , runtimePartValidities =
        stateValidities currentState
    , runtimePartImplementationStatuses =
        stateImplementationStatuses currentState
    , runtimePartReadValues =
        stateReadValues currentState
    , runtimePartEvents =
        stateRuntimeEvents currentState
    }

runtimeStateFromParts :: RuntimeStateParts -> RuntimeState
runtimeStateFromParts currentParts =
  RuntimeState
    { stateExecutionStatuses =
        runtimePartExecutionStatuses currentParts
    , stateReadStatuses =
        runtimePartReadStatuses currentParts
    , stateValidities =
        runtimePartValidities currentParts
    , stateImplementationStatuses =
        runtimePartImplementationStatuses currentParts
    , stateReadValues =
        runtimePartReadValues currentParts
    , stateRuntimeEvents =
        runtimePartEvents currentParts
    }

executionStatuses :: RuntimeState -> Map HandleId ExecutionStatus
executionStatuses =
  stateExecutionStatuses

readStatuses :: RuntimeState -> Map HandleId ReadStatus
readStatuses =
  stateReadStatuses

validities :: RuntimeState -> Map HandleId Validity
validities =
  stateValidities

implementationStatuses ::
  RuntimeState ->
  Map ImplementationId ImplementationStatus
implementationStatuses =
  stateImplementationStatuses

runtimeEvents :: RuntimeState -> [RuntimeEvent]
runtimeEvents =
  stateRuntimeEvents

executionStatusFor :: HandleId -> RuntimeState -> ExecutionStatus
executionStatusFor currentId =
  Map.findWithDefault ExecutionUnused currentId
    . stateExecutionStatuses

readStatusFor :: HandleId -> RuntimeState -> ReadStatus
readStatusFor currentId =
  Map.findWithDefault ReadUnused currentId
    . stateReadStatuses

validityFor :: HandleId -> RuntimeState -> Validity
validityFor currentId =
  Map.findWithDefault Trusted currentId
    . stateValidities

implementationStatusFor ::
  ImplementationId ->
  RuntimeState ->
  ImplementationStatus
implementationStatusFor currentId =
  Map.findWithDefault ImplementationUnused currentId
    . stateImplementationStatuses

readValueFor :: HandleId -> RuntimeState -> Maybe RuntimeData
readValueFor currentId currentState =
  runtimeDataBindingValue
    <$> Map.lookup currentId (stateReadValues currentState)

-- | Build the only value passed through the handler dependency channel.
-- Missing map entries remain Nothing; they are not confused with Unused.
handlerInputFor :: Maybe HandleId -> RuntimeState -> HandlerInput
handlerInputFor Nothing _ =
  noHandlerInput
handlerInputFor (Just currentId) currentState =
  handlerInput
    currentId
    (Map.lookup currentId (stateExecutionStatuses currentState))
    (Map.lookup currentId (stateReadStatuses currentState))
    (validityFor currentId currentState)

setExecutionStatus ::
  HandleId ->
  ExecutionStatus ->
  RuntimeState ->
  RuntimeState
setExecutionStatus currentId currentStatus currentState =
  appendEvent
    (ExecutionStatusChanged currentId currentStatus)
    currentState
      { stateExecutionStatuses =
          Map.insert
            currentId
            currentStatus
            (stateExecutionStatuses currentState)
      }

setReadStatus ::
  HandleId ->
  ReadStatus ->
  RuntimeState ->
  RuntimeState
setReadStatus currentId currentStatus currentState =
  appendEvent
    (ReadStatusChanged currentId currentStatus)
    currentState
      { stateReadStatuses =
          Map.insert
            currentId
            currentStatus
            (stateReadStatuses currentState)
      }

setValidity ::
  HandleId ->
  Validity ->
  RuntimeState ->
  RuntimeState
setValidity currentId currentValidity currentState =
  appendEvent
    (ValidityChanged currentId currentValidity)
    currentState
      { stateValidities =
          Map.insert
            currentId
            currentValidity
            (stateValidities currentState)
      }

setImplementationStatus ::
  ImplementationId ->
  ImplementationStatus ->
  RuntimeState ->
  RuntimeState
setImplementationStatus currentId currentStatus currentState =
  appendEvent
    (ImplementationStatusChanged currentId currentStatus)
    currentState
      { stateImplementationStatuses =
          Map.insert
            currentId
            currentStatus
            (stateImplementationStatuses currentState)
      }

putReadValue ::
  HandleId ->
  RuntimeDataBinding ->
  RuntimeState ->
  Either RuntimeFailure RuntimeState
putReadValue currentId currentBinding currentState
  | runtimeDataBindingHandle currentBinding /= currentId =
      Left
        RuntimeFailure
          { runtimeFailurePhase = RuntimeInvariantPhase
          , runtimeFailureCommitState = NoExternalCommit
          , runtimeFailureMessage =
              "read value binding handle does not match target handle"
          }
  | otherwise =
      Right
        ( appendEvent
            (ReadValueAvailable currentId)
            ( setReadStatus
                currentId
                ReadAvailable
                currentState
                  { stateReadValues =
                      Map.insert
                        currentId
                        currentBinding
                        (stateReadValues currentState)
                  }
            )
        )

recordReadFailure ::
  HandleId ->
  RuntimeFailure ->
  RuntimeState ->
  RuntimeState
recordReadFailure currentId currentFailure =
  setReadStatus currentId (ReadFailed currentFailure)

recordObservationCaptured :: HandleId -> RuntimeState -> RuntimeState
recordObservationCaptured currentId =
  appendEvent (RuntimeObservationCaptured currentId)

recordHandlerInvocation ::
  ExecutionProvenance ->
  DemandNodeId ->
  HandleId ->
  RuntimeState ->
  RuntimeState
recordHandlerInvocation currentProvenance currentNode currentHandle =
  appendEvent
    (HandlerInvocationAuthorized currentProvenance currentNode currentHandle)

recordObservationUnavailable ::
  HandleId ->
  RuntimeFailure ->
  RuntimeState ->
  RuntimeState
recordObservationUnavailable currentId currentFailure =
  appendEvent
    (RuntimeObservationUnavailable currentId currentFailure)
    . setValidity currentId Suspect

appendEvent :: RuntimeEvent -> RuntimeState -> RuntimeState
appendEvent currentEvent currentState =
  currentState
    { stateRuntimeEvents =
        stateRuntimeEvents currentState ++ [currentEvent]
    }
