module MyFramework.Handler
  ( CommandHandler
  , CommitState (..)
  , ExecutionStatus (..)
  , FailurePhase (..)
  , HandlerInput
  , HandlerRegistry
  , InputUsed
  , ReadHandler
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
  , commandHandlerUsingInput
  , decodeWithCodec
  , discardingCommandHandler
  , discardingCommandHandlerUsingInput
  , emptyHandlerRegistry
  , encodeWithCodec
  , encodeRegisteredReadValue
  , executionSucceeded
  , handlerInputExecutionStatus
  , handlerInputHandleId
  , handlerInputReadStatus
  , handlerInputValidity
  , handlerRegistryIds
  , normalizeRegisteredReadData
  , readCodec
  , readHandler
  , readHandlerUsingInput
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
  , useHandlerInput
  , unitValueCodec
  , validateRuntimeData
  , valueTag
  , valueTagSchema
  ) where

import MyFramework.Handler.Internal
