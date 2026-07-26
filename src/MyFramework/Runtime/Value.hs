{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MyFramework.Runtime.Value
  ( BoundArgs (..)
  , RuntimeData (..)
  , RuntimeDataBinding (..)
  , RuntimeTypedValue (..)
  , RuntimeValueError (..)
  , SomeRuntimeValue (..)
  , ValueCodec (..)
  , ValueTag
  , bindArguments
  , decodeWithCodec
  , encodeWithCodec
  , lookupRuntimeData
  , opaqueValueCodec
  , runtimeTypedValue
  , sameValueTag
  , someRuntimeValue
  , someRuntimeValueSchema
  , someRuntimeValueText
  , typedValueFromSome
  , unitBoundArgs
  , unitValueCodec
  , validateRuntimeData
  , valueTag
  , valueTagSchema
  ) where

import Control.Monad
  ( foldM )
import Data.List
  ( find )
import Data.Type.Equality
  ( (:~:) (Refl) )
import Data.Typeable
  ( Typeable
  , eqT
  )

import MyFramework.CURDE.Types
  ( FieldName
  , HandleId
  , SchemaIdentity (..)
  , SchemaRef
  , SchemaShape (..)
  , schemaRefIdentityOf
  , unitSchema
  )

data ValueTag value where
  ValueTag ::
    Typeable value =>
    SchemaRef value ->
    (value -> String) ->
    ValueTag value

data RuntimeTypedValue value = RuntimeTypedValue
  { runtimeTypedValueTag :: ValueTag value
  , runtimeTypedValuePayload :: value
  }

data SomeRuntimeValue where
  SomeRuntimeValue ::
    RuntimeTypedValue value ->
    SomeRuntimeValue

instance Eq SomeRuntimeValue where
  left == right =
    someRuntimeValueSchema left == someRuntimeValueSchema right
      && someRuntimeValueText left == someRuntimeValueText right

instance Show SomeRuntimeValue where
  show currentValue =
    "SomeRuntimeValue "
      ++ show (someRuntimeValueSchema currentValue)
      ++ " "
      ++ show (someRuntimeValueText currentValue)

data RuntimeData
  = RuntimeUnit
  | RuntimeBool Bool
  | RuntimeInteger Integer
  | RuntimeDecimal String
  | RuntimeText String
  | RuntimeList [RuntimeData]
  | RuntimeRecord [(FieldName, RuntimeData)]
  | RuntimeOpaque SomeRuntimeValue
  deriving (Eq, Show)

data RuntimeDataBinding = RuntimeDataBinding
  { runtimeDataBindingHandle :: HandleId
  , runtimeDataBindingSchema :: SchemaIdentity
  , runtimeDataBindingValue :: RuntimeData
  }
  deriving (Eq, Show)

data BoundArgs = BoundArgs
  { boundArgsSchema :: SchemaIdentity
  , boundArgsValue :: RuntimeData
  }
  deriving (Eq, Show)

data RuntimeValueError
  = RuntimeDataShapeMismatch SchemaIdentity RuntimeData
  | RuntimeOpaqueSchemaMismatch SchemaIdentity SchemaIdentity
  | RuntimeCodecDecodeFailed SchemaIdentity String
  | RuntimeMissingReference HandleId
  | RuntimeDuplicateReference HandleId
  | RuntimeReferenceSchemaMismatch HandleId SchemaIdentity SchemaIdentity
  | RuntimeDuplicateField FieldName
  | RuntimeMissingField FieldName
  | RuntimeUnexpectedField FieldName
  | RuntimeProductArityMismatch Int Int
  deriving (Eq, Show)

data ValueCodec value = ValueCodec
  { valueCodecSchema :: SchemaRef value
  , encodeRuntimeData :: value -> Either RuntimeValueError RuntimeData
  , decodeRuntimeData :: RuntimeData -> Either RuntimeValueError value
  }

valueTag ::
  Typeable value =>
  SchemaRef value ->
  (value -> String) ->
  ValueTag value
valueTag =
  ValueTag

valueTagSchema :: ValueTag value -> SchemaRef value
valueTagSchema (ValueTag currentSchema _) =
  currentSchema

runtimeTypedValue :: ValueTag value -> value -> RuntimeTypedValue value
runtimeTypedValue =
  RuntimeTypedValue

someRuntimeValue :: ValueTag value -> value -> SomeRuntimeValue
someRuntimeValue currentTag =
  SomeRuntimeValue . RuntimeTypedValue currentTag

someRuntimeValueSchema :: SomeRuntimeValue -> SchemaIdentity
someRuntimeValueSchema (SomeRuntimeValue currentValue) =
  schemaRefIdentityOf (valueTagSchema (runtimeTypedValueTag currentValue))

someRuntimeValueText :: SomeRuntimeValue -> String
someRuntimeValueText (SomeRuntimeValue currentValue) =
  case runtimeTypedValueTag currentValue of
    ValueTag _ renderValue ->
      renderValue (runtimeTypedValuePayload currentValue)

sameValueTag :: ValueTag left -> ValueTag right -> Maybe (left :~: right)
sameValueTag (ValueTag leftSchema _) (ValueTag rightSchema _)
  | schemaRefIdentityOf leftSchema == schemaRefIdentityOf rightSchema =
      eqT
  | otherwise =
      Nothing

typedValueFromSome ::
  ValueTag value ->
  SomeRuntimeValue ->
  Maybe (RuntimeTypedValue value)
typedValueFromSome expectedTag (SomeRuntimeValue currentValue) =
  case sameValueTag expectedTag (runtimeTypedValueTag currentValue) of
    Just Refl ->
      Just currentValue
    Nothing ->
      Nothing

opaqueValueCodec :: ValueTag value -> ValueCodec value
opaqueValueCodec currentTag =
  ValueCodec
    { valueCodecSchema = valueTagSchema currentTag
    , encodeRuntimeData =
        Right . RuntimeOpaque . someRuntimeValue currentTag
    , decodeRuntimeData = decodeOpaque currentTag
    }

unitValueCodec :: ValueCodec ()
unitValueCodec =
  ValueCodec
    { valueCodecSchema = unitSchema
    , encodeRuntimeData = const (Right RuntimeUnit)
    , decodeRuntimeData = decodeUnit
    }
  where
    decodeUnit RuntimeUnit =
      Right ()
    decodeUnit currentData =
      Left
        ( RuntimeCodecDecodeFailed
            (schemaRefIdentityOf unitSchema)
            ("expected RuntimeUnit, observed " ++ show currentData)
        )

decodeOpaque ::
  ValueTag value ->
  RuntimeData ->
  Either RuntimeValueError value
decodeOpaque currentTag currentData =
  case currentData of
    RuntimeOpaque currentValue ->
      case typedValueFromSome currentTag currentValue of
        Just currentTypedValue ->
          Right (runtimeTypedValuePayload currentTypedValue)
        Nothing ->
          Left
            ( RuntimeCodecDecodeFailed
                (schemaRefIdentityOf (valueTagSchema currentTag))
                "opaque value does not match the registered schema and Haskell type"
            )
    _ ->
      Left
        ( RuntimeCodecDecodeFailed
            (schemaRefIdentityOf (valueTagSchema currentTag))
            "expected RuntimeOpaque"
        )

encodeWithCodec ::
  ValueCodec value ->
  value ->
  Either RuntimeValueError RuntimeData
encodeWithCodec currentCodec currentValue = do
  currentData <-
    encodeRuntimeData currentCodec currentValue
  validateRuntimeData
    (schemaRefIdentityOf (valueCodecSchema currentCodec))
    currentData
  pure currentData

decodeWithCodec ::
  ValueCodec value ->
  RuntimeData ->
  Either RuntimeValueError value
decodeWithCodec currentCodec currentData = do
  validateRuntimeData
    (schemaRefIdentityOf (valueCodecSchema currentCodec))
    currentData
  decodeRuntimeData currentCodec currentData

bindArguments ::
  ValueCodec args ->
  args ->
  Either RuntimeValueError BoundArgs
bindArguments currentCodec currentArguments = do
  currentData <-
    encodeWithCodec currentCodec currentArguments
  pure
    BoundArgs
      { boundArgsSchema =
          schemaRefIdentityOf (valueCodecSchema currentCodec)
      , boundArgsValue = currentData
      }

unitBoundArgs :: BoundArgs
unitBoundArgs =
  BoundArgs
    { boundArgsSchema = schemaRefIdentityOf unitSchema
    , boundArgsValue = RuntimeUnit
    }

lookupRuntimeData ::
  HandleId ->
  SchemaIdentity ->
  [RuntimeDataBinding] ->
  Either RuntimeValueError RuntimeData
lookupRuntimeData currentHandle expectedSchema currentBindings =
  case
    [ currentBinding
    | currentBinding <- currentBindings
    , runtimeDataBindingHandle currentBinding == currentHandle
    ] of
    [] ->
      Left (RuntimeMissingReference currentHandle)
    [currentBinding]
      | runtimeDataBindingSchema currentBinding /= expectedSchema ->
          Left
            ( RuntimeReferenceSchemaMismatch
                currentHandle
                expectedSchema
                (runtimeDataBindingSchema currentBinding)
            )
      | otherwise -> do
          validateRuntimeData
            expectedSchema
            (runtimeDataBindingValue currentBinding)
          pure (runtimeDataBindingValue currentBinding)
    _ ->
      Left (RuntimeDuplicateReference currentHandle)

validateRuntimeData ::
  SchemaIdentity ->
  RuntimeData ->
  Either RuntimeValueError ()
validateRuntimeData expectedSchema currentData =
  case (schemaIdentityShape expectedSchema, currentData) of
    (_, RuntimeOpaque currentValue)
      | someRuntimeValueSchema currentValue == expectedSchema ->
          Right ()
      | otherwise ->
          Left
            ( RuntimeOpaqueSchemaMismatch
                expectedSchema
                (someRuntimeValueSchema currentValue)
            )
    (UnitShape, RuntimeUnit) ->
      Right ()
    (ScalarShape, RuntimeBool _) ->
      Right ()
    (ScalarShape, RuntimeInteger _) ->
      Right ()
    (ScalarShape, RuntimeDecimal _) ->
      Right ()
    (ScalarShape, RuntimeText _) ->
      Right ()
    (RecordShape expectedFields, RuntimeRecord currentFields) ->
      validateRecord expectedFields currentFields
    (ProductShape expectedItems, RuntimeList currentItems) ->
      validateProduct expectedItems currentItems
    _ ->
      Left (RuntimeDataShapeMismatch expectedSchema currentData)

validateRecord ::
  [(FieldName, SchemaIdentity)] ->
  [(FieldName, RuntimeData)] ->
  Either RuntimeValueError ()
validateRecord expectedFields currentFields = do
  foldM validateExpectedField () expectedFields
  case
    find
      (\(currentName, _) -> currentName `notElem` map fst expectedFields)
      currentFields of
    Nothing ->
      Right ()
    Just (unexpectedName, _) ->
      Left (RuntimeUnexpectedField unexpectedName)
  where
    validateExpectedField () (expectedName, expectedFieldSchema) =
      case
        [ currentValue
        | (currentName, currentValue) <- currentFields
        , currentName == expectedName
        ] of
        [] ->
          Left (RuntimeMissingField expectedName)
        [currentValue] ->
          validateRuntimeData expectedFieldSchema currentValue
        _ ->
          Left (RuntimeDuplicateField expectedName)

validateProduct ::
  [SchemaIdentity] ->
  [RuntimeData] ->
  Either RuntimeValueError ()
validateProduct expectedItems currentItems
  | length expectedItems /= length currentItems =
      Left
        ( RuntimeProductArityMismatch
            (length expectedItems)
            (length currentItems)
        )
  | otherwise =
      foldM
        (\() (expectedSchema, currentData) ->
            validateRuntimeData expectedSchema currentData
        )
        ()
        (zip expectedItems currentItems)
