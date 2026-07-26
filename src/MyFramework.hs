module MyFramework
  ( frameworkVersion
  , module MyFramework.Ast
  , module MyFramework.CURDE
  , module MyFramework.Handler
  ) where

import MyFramework.Ast
import MyFramework.CURDE
import MyFramework.Handler
  ( CommandHandler
  , CommitState (..)
  , CudeInvocation (..)
  , ExecutionStatus (..)
  , FailurePhase (..)
  , HandlerInput
  , HandlerRegistry
  , ObservationCapture
  , ReadHandler
  , ReadInvocation (..)
  , ReadStatus (..)
  , RegistryError
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
  )

frameworkVersion :: String
frameworkVersion = "0.1.0.0"
