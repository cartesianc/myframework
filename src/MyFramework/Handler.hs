{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MyFramework.Handler
  ( CommandHandler
  , CommitState (..)
  , CudeInvocation (..)
  , ExecutionStatus (..)
  , FailurePhase (..)
  , HandlerInput
  , HandlerRegistry
  , ObservationCapture (..)
  , ReadHandler
  , ReadInvocation (..)
  , ReadStatus (..)
  , RegistryError (..)
  , RuntimeData (..)
  , RuntimeFailure (..)
  , RuntimeValueError (..)
  , SomeCommandHandler
  , SomeReadHandler
  , SomeRuntimeValue
  , Validity (..)
  , ValueCodec (..)
  , ValueTag
  , commandHandler
  , decodeWithCodec
  , discardingCommandHandler
  , emptyHandlerRegistry
  , encodeWithCodec
  , encodeRegisteredReadValue
  , executionSucceeded
  , handlerInputExecutionStatus
  , handlerInputHandleId
  , handlerInputReadStatus
  , handlerInputValidity
  , handlerRegistryIds
  , invokeCude
  , invokeRead
  , normalizeRegisteredReadData
  , readCodec
  , readHandler
  , registerC
  , registerD
  , registerE
  , registerR
  , registerU
  , readAvailable
  , someRuntimeValue
  , someRuntimeValueSchema
  , someRuntimeValueText
  , typedValueCodec
  , unitValueCodec
  , validateRuntimeData
  , valueTag
  , valueTagSchema
  ) where

import Control.Exception
  ( SomeException
  , displayException
  , evaluate
  , try
  )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map

import MyFramework.CURDE.Types
  hiding
    ( ObservationCaptured
    , ObservationDiscarded
    )
import MyFramework.Runtime.Types
  ( CommitState (..)
  , ExecutionStatus (..)
  , FailurePhase (..)
  , HandlerInput
  , ReadStatus (..)
  , RuntimeFailure (..)
  , Validity (..)
  , executionSucceeded
  , handlerInputExecutionStatus
  , handlerInputHandleId
  , handlerInputReadStatus
  , handlerInputValidity
  , readAvailable
  )
import MyFramework.Runtime.Value
  ( RuntimeData (..)
  , RuntimeValueError (..)
  , SomeRuntimeValue
  , ValueCodec (..)
  , ValueTag
  , decodeWithCodec
  , encodeWithCodec
  , opaqueValueCodec
  , someRuntimeValue
  , someRuntimeValueSchema
  , someRuntimeValueText
  , unitValueCodec
  , validateRuntimeData
  , valueTag
  , valueTagSchema
  )

data CommandHandler args observation = CommandHandler
  { commandArgumentCodec :: ValueCodec args
  , commandObservationCodec :: Maybe (ValueCodec observation)
  , runCommand ::
      HandlerInput ->
      args ->
      IO (Either RuntimeFailure (Maybe observation))
  }

data ReadHandler value = ReadHandler
  { readResultCodec :: ValueCodec value
  , readAction ::
      Maybe (HandlerInput -> IO (Either RuntimeFailure value))
  }

data SomeCommandHandler where
  SomeCommandHandler ::
    Handle name kind args observation ->
    CommandHandler args observation ->
    SomeCommandHandler

data SomeReadHandler where
  SomeReadHandler ::
    Handle name 'R () value ->
    ReadHandler value ->
    SomeReadHandler

data HandlerRegistry = HandlerRegistry
  { registryCommandHandlers :: Map HandleId SomeCommandHandler
  , registryReadHandlers :: Map HandleId SomeReadHandler
  }

data RegistryError
  = DuplicateHandler HandleId
  | HandlerSchemaMismatch HandleId SchemaIdentity SchemaIdentity
  | HandlerObservationModeMismatch HandleId Bool Bool
  | ReadSourceMissing HandleId
  | ReadHandlerRequired HandleId
  | ReadHandlerNotAllowed HandleId ReadSource
  deriving (Eq, Show)

data ObservationCapture
  = ObservationDiscarded
  | ObservationCaptured RuntimeData
  | ObservationCaptureFailed RuntimeFailure
  deriving (Eq, Show)

data CudeInvocation
  = CudeInvocationSucceeded ObservationCapture
  | CudeInvocationFailed RuntimeFailure
  deriving (Eq, Show)

data ReadInvocation
  = ReadInvocationSucceeded RuntimeData
  | ReadInvocationSourceRequired ReadSource
  | ReadInvocationFailed RuntimeFailure
  deriving (Eq, Show)

-- | Constructor for a command whose private observation is captured.
commandHandler ::
  ValueCodec args ->
  ValueCodec observation ->
  (HandlerInput ->
    args ->
    IO (Either RuntimeFailure observation)) ->
  CommandHandler args observation
commandHandler argumentCodec observationCodec currentAction =
  CommandHandler
    { commandArgumentCodec = argumentCodec
    , commandObservationCodec = Just observationCodec
    , runCommand =
        \currentInput currentArguments ->
          fmap
            (fmap Just)
            (currentAction currentInput currentArguments)
    }

-- | Constructor for DiscardObservation. The action returns no observation
-- value, which is necessary because NoObservation is intentionally opaque.
discardingCommandHandler ::
  ValueCodec args ->
  (HandlerInput ->
    args ->
    IO (Either RuntimeFailure ())) ->
  CommandHandler args NoObservation
discardingCommandHandler argumentCodec currentAction =
  CommandHandler
    { commandArgumentCodec = argumentCodec
    , commandObservationCodec = Nothing
    , runCommand =
        \currentInput currentArguments ->
          fmap
            (fmap (const Nothing))
            (currentAction currentInput currentArguments)
    }

readHandler ::
  ValueCodec value ->
  (HandlerInput -> IO (Either RuntimeFailure value)) ->
  ReadHandler value
readHandler currentCodec currentAction =
  ReadHandler
    { readResultCodec = currentCodec
    , readAction = Just currentAction
    }

readCodec :: ValueCodec value -> ReadHandler value
readCodec currentCodec =
  ReadHandler
    { readResultCodec = currentCodec
    , readAction = Nothing
    }

typedValueCodec :: ValueTag value -> ValueCodec value
typedValueCodec =
  opaqueValueCodec

emptyHandlerRegistry :: HandlerRegistry
emptyHandlerRegistry =
  HandlerRegistry
    { registryCommandHandlers = Map.empty
    , registryReadHandlers = Map.empty
    }

handlerRegistryIds :: HandlerRegistry -> [HandleId]
handlerRegistryIds currentRegistry =
  Map.keys (registryCommandHandlers currentRegistry)
    ++ Map.keys (registryReadHandlers currentRegistry)

registerC ::
  Handle name 'C args observation ->
  CommandHandler args observation ->
  HandlerRegistry ->
  Either RegistryError HandlerRegistry
registerC =
  registerCommand

registerU ::
  Handle name 'U args observation ->
  CommandHandler args observation ->
  HandlerRegistry ->
  Either RegistryError HandlerRegistry
registerU =
  registerCommand

registerD ::
  Handle name 'D args observation ->
  CommandHandler args observation ->
  HandlerRegistry ->
  Either RegistryError HandlerRegistry
registerD =
  registerCommand

registerE ::
  Handle name 'E args observation ->
  CommandHandler args observation ->
  HandlerRegistry ->
  Either RegistryError HandlerRegistry
registerE =
  registerCommand

registerR ::
  Handle name 'R () value ->
  ReadHandler value ->
  HandlerRegistry ->
  Either RegistryError HandlerRegistry
registerR currentHandle currentHandler currentRegistry
  | codecSchemaIdentity (readResultCodec currentHandler)
      /= schemaRefIdentityOf (handleResultSchemaRef currentHandle) =
      Left
        ( HandlerSchemaMismatch
            currentId
            (schemaRefIdentityOf (handleResultSchemaRef currentHandle))
            (codecSchemaIdentity (readResultCodec currentHandler))
        )
  | registryContains currentId currentRegistry =
      Left (DuplicateHandler currentId)
  | otherwise = do
      validateReadRegistration currentHandle currentHandler
      Right
        currentRegistry
          { registryReadHandlers =
              Map.insert
                currentId
                (SomeReadHandler currentHandle currentHandler)
                (registryReadHandlers currentRegistry)
          }
  where
    currentId =
      handleId currentHandle

validateReadRegistration ::
  Handle name 'R () value ->
  ReadHandler value ->
  Either RegistryError ()
validateReadRegistration currentHandle currentHandler =
  case (handleReadSource currentHandle, readAction currentHandler) of
    (Nothing, _) ->
      Left (ReadSourceMissing currentId)
    (Just ReadFromHandler, Nothing) ->
      Left (ReadHandlerRequired currentId)
    (Just ReadFromHandler, Just _) ->
      Right ()
    (Just currentSource, Just _) ->
      Left (ReadHandlerNotAllowed currentId currentSource)
    (Just _, Nothing) ->
      Right ()
  where
    currentId =
      handleId currentHandle

registerCommand ::
  Handle name kind args observation ->
  CommandHandler args observation ->
  HandlerRegistry ->
  Either RegistryError HandlerRegistry
registerCommand currentHandle currentHandler currentRegistry
  | codecSchemaIdentity (commandArgumentCodec currentHandler)
      /= schemaRefIdentityOf (handleArgumentSchemaRef currentHandle) =
      Left
        ( HandlerSchemaMismatch
            currentId
            (schemaRefIdentityOf (handleArgumentSchemaRef currentHandle))
            (codecSchemaIdentity (commandArgumentCodec currentHandler))
        )
  | registryContains currentId currentRegistry =
      Left (DuplicateHandler currentId)
  | otherwise = do
      validateObservationRegistration currentHandle currentHandler
      Right
        currentRegistry
          { registryCommandHandlers =
              Map.insert
                currentId
                (SomeCommandHandler currentHandle currentHandler)
                (registryCommandHandlers currentRegistry)
          }
  where
    currentId =
      handleId currentHandle

validateObservationRegistration ::
  Handle name kind args observation ->
  CommandHandler args observation ->
  Either RegistryError ()
validateObservationRegistration currentHandle currentHandler =
  case
    ( handleObservationSchemaRef currentHandle
    , commandObservationCodec currentHandler
    ) of
    (Nothing, Nothing) ->
      Right ()
    (Just expectedSchema, Just currentCodec)
      | schemaRefIdentityOf expectedSchema
          == codecSchemaIdentity currentCodec ->
          Right ()
      | otherwise ->
          Left
            ( HandlerSchemaMismatch
                currentId
                (schemaRefIdentityOf expectedSchema)
                (codecSchemaIdentity currentCodec)
            )
    (expectedSchema, currentCodec) ->
      Left
        ( HandlerObservationModeMismatch
            currentId
            (maybe False (const True) expectedSchema)
            (maybe False (const True) currentCodec)
        )
  where
    currentId =
      handleId currentHandle

codecSchemaIdentity :: ValueCodec value -> SchemaIdentity
codecSchemaIdentity =
  schemaRefIdentityOf . valueCodecSchema

registryContains :: HandleId -> HandlerRegistry -> Bool
registryContains currentId currentRegistry =
  Map.member currentId (registryCommandHandlers currentRegistry)
    || Map.member currentId (registryReadHandlers currentRegistry)

invokeCude ::
  HandlerRegistry ->
  HandleId ->
  HandlerInput ->
  RuntimeData ->
  IO CudeInvocation
invokeCude currentRegistry currentId currentInput currentArguments =
  case Map.lookup currentId (registryCommandHandlers currentRegistry) of
    Nothing ->
      pure (CudeInvocationFailed (missingHandlerFailure currentId))
    Just currentHandler ->
      invokeSomeCommand currentHandler currentInput currentArguments

invokeRead ::
  HandlerRegistry ->
  HandleId ->
  HandlerInput ->
  IO ReadInvocation
invokeRead currentRegistry currentId currentInput =
  case Map.lookup currentId (registryReadHandlers currentRegistry) of
    Nothing ->
      pure (ReadInvocationFailed (missingHandlerFailure currentId))
    Just currentHandler ->
      invokeSomeRead currentHandler currentInput

normalizeRegisteredReadData ::
  HandlerRegistry ->
  HandleId ->
  RuntimeData ->
  Either RuntimeFailure RuntimeData
normalizeRegisteredReadData currentRegistry currentId currentData =
  case Map.lookup currentId (registryReadHandlers currentRegistry) of
    Nothing ->
      Left (missingHandlerFailure currentId)
    Just currentHandler ->
      normalizeSomeReadData currentHandler currentData

encodeRegisteredReadValue ::
  HandlerRegistry ->
  HandleId ->
  SomeRuntimeValue ->
  Either RuntimeFailure RuntimeData
encodeRegisteredReadValue currentRegistry currentId =
  normalizeRegisteredReadData currentRegistry currentId
    . RuntimeOpaque

invokeSomeCommand ::
  SomeCommandHandler ->
  HandlerInput ->
  RuntimeData ->
  IO CudeInvocation
invokeSomeCommand
  (SomeCommandHandler currentHandle currentHandler)
  currentInput
  currentArguments =
    case validateHandlerInput currentHandle currentInput of
      Left currentFailure ->
        pure (CudeInvocationFailed currentFailure)
      Right () ->
        invokeCommand
          currentHandle
          currentHandler
          currentInput
          currentArguments

invokeCommand ::
  Handle name kind args observation ->
  CommandHandler args observation ->
  HandlerInput ->
  RuntimeData ->
  IO CudeInvocation
invokeCommand currentHandle currentHandler currentInput currentArguments = do
  decodedResult <-
    tryEvaluate
      (decodeWithCodec (commandArgumentCodec currentHandler) currentArguments)
  case decodedResult of
    Left currentException ->
      pure
        ( CudeInvocationFailed
            ( exceptionFailure
                ArgumentBindingPhase
                NoExternalCommit
                currentException
            )
        )
    Right (Left currentError) ->
      pure
        ( CudeInvocationFailed
            ( valueFailure
                ArgumentBindingPhase
                NoExternalCommit
                currentError
            )
        )
    Right (Right decodedArguments) -> do
      commandResult <-
        tryAction
          ( runCommand
              currentHandler
              currentInput
              decodedArguments
              >>= evaluate
          )
      case commandResult of
        Left currentException ->
          pure
            ( CudeInvocationFailed
                ( exceptionFailure
                    HandlerPhase
                    ExternalCommitUnknown
                    currentException
                )
            )
        Right (Left currentFailure) ->
          pure (CudeInvocationFailed currentFailure)
        Right (Right currentObservation) -> do
          capture <-
            captureCommandObservation
              currentHandle
              currentHandler
              currentObservation
          pure (CudeInvocationSucceeded capture)

captureCommandObservation ::
  Handle name kind args observation ->
  CommandHandler args observation ->
  Maybe observation ->
  IO ObservationCapture
captureCommandObservation currentHandle currentHandler currentObservation =
  case
    ( handleObservationSchemaRef currentHandle
    , commandObservationCodec currentHandler
    , currentObservation
    ) of
    (Nothing, Nothing, Nothing) ->
      pure ObservationDiscarded
    (Just _, Just currentCodec, Just currentValue) -> do
      encodedResult <-
        tryEvaluate (encodeWithCodec currentCodec currentValue)
      pure
        ( case encodedResult of
            Left currentException ->
              ObservationCaptureFailed
                ( exceptionFailure
                    ObservationPhase
                    ExternalCommitted
                    currentException
                )
            Right (Left currentError) ->
              ObservationCaptureFailed
                ( valueFailure
                    ObservationPhase
                    ExternalCommitted
                    currentError
                )
            Right (Right currentData) ->
              ObservationCaptured currentData
        )
    _ ->
      pure
        ( ObservationCaptureFailed
            RuntimeFailure
              { runtimeFailurePhase = ObservationPhase
              , runtimeFailureCommitState = ExternalCommitted
              , runtimeFailureMessage =
                  "registered observation mode and handler result diverged"
              }
        )

invokeSomeRead ::
  SomeReadHandler ->
  HandlerInput ->
  IO ReadInvocation
invokeSomeRead
  (SomeReadHandler currentHandle currentHandler)
  currentInput =
    case validateHandlerInput currentHandle currentInput of
      Left currentFailure ->
        pure (ReadInvocationFailed currentFailure)
      Right () ->
        case readAction currentHandler of
          Nothing ->
            pure
              ( ReadInvocationSourceRequired
                  (maybe ReadFromHandler id (handleReadSource currentHandle))
              )
          Just currentAction -> do
            actionResult <-
              tryAction (currentAction currentInput >>= evaluate)
            case actionResult of
              Left currentException ->
                pure
                  ( ReadInvocationFailed
                      ( exceptionFailure
                          ReadPhase
                          NoExternalCommit
                          currentException
                      )
                  )
              Right (Left currentFailure) ->
                pure (ReadInvocationFailed currentFailure)
              Right (Right currentValue) ->
                encodeReadResult currentHandler currentValue

encodeReadResult ::
  ReadHandler value ->
  value ->
  IO ReadInvocation
encodeReadResult currentHandler currentValue = do
  encodedResult <-
    tryEvaluate
      (encodeWithCodec (readResultCodec currentHandler) currentValue)
  pure
    ( case encodedResult of
        Left currentException ->
          ReadInvocationFailed
            ( exceptionFailure
                ReadPhase
                NoExternalCommit
                currentException
            )
        Right (Left currentError) ->
          ReadInvocationFailed
            ( valueFailure
                ReadPhase
                NoExternalCommit
                currentError
            )
        Right (Right currentData) ->
          ReadInvocationSucceeded currentData
    )

validateHandlerInput ::
  Handle name kind args result ->
  HandlerInput ->
  Either RuntimeFailure ()
validateHandlerInput currentHandle currentInput
  | expectedInput == actualInput =
      Right ()
  | otherwise =
      Left
        RuntimeFailure
          { runtimeFailurePhase = DependencyPhase
          , runtimeFailureCommitState = NoExternalCommit
          , runtimeFailureMessage =
              "handler input mismatch for "
                ++ renderHandleId (handleId currentHandle)
                ++ ": expected "
                ++ show expectedInput
                ++ ", observed "
                ++ show actualInput
          }
  where
    expectedInput =
      someHandleId <$> handleInput currentHandle
    actualInput =
      handlerInputHandleId currentInput

normalizeSomeReadData ::
  SomeReadHandler ->
  RuntimeData ->
  Either RuntimeFailure RuntimeData
normalizeSomeReadData
  (SomeReadHandler _ currentHandler)
  currentData = do
    currentValue <-
      mapValueFailure ReadPhase NoExternalCommit
        (decodeWithCodec (readResultCodec currentHandler) currentData)
    mapValueFailure ReadPhase NoExternalCommit
      (encodeWithCodec (readResultCodec currentHandler) currentValue)

mapValueFailure ::
  FailurePhase ->
  CommitState ->
  Either RuntimeValueError value ->
  Either RuntimeFailure value
mapValueFailure currentPhase currentCommitState =
  either
    (Left . valueFailure currentPhase currentCommitState)
    Right

valueFailure ::
  FailurePhase ->
  CommitState ->
  RuntimeValueError ->
  RuntimeFailure
valueFailure currentPhase currentCommitState currentError =
  RuntimeFailure
    { runtimeFailurePhase = currentPhase
    , runtimeFailureCommitState = currentCommitState
    , runtimeFailureMessage = show currentError
    }

missingHandlerFailure :: HandleId -> RuntimeFailure
missingHandlerFailure currentId =
  RuntimeFailure
    { runtimeFailurePhase = RuntimeInvariantPhase
    , runtimeFailureCommitState = NoExternalCommit
    , runtimeFailureMessage =
        "missing registered handler for " ++ renderHandleId currentId
    }

exceptionFailure ::
  FailurePhase ->
  CommitState ->
  SomeException ->
  RuntimeFailure
exceptionFailure currentPhase currentCommitState currentException =
  RuntimeFailure
    { runtimeFailurePhase = currentPhase
    , runtimeFailureCommitState = currentCommitState
    , runtimeFailureMessage = displayException currentException
    }

tryAction :: IO value -> IO (Either SomeException value)
tryAction =
  try

tryEvaluate :: value -> IO (Either SomeException value)
tryEvaluate =
  try . evaluate
