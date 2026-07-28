module MyFramework.Runtime.Expression
  ( RExprEvaluationError (..)
  , interpretRExprDecl
  ) where

import Data.List
  ( sort )

import MyFramework.CURDE.Expression
  ( LiteralValue (..)
  , RExprDecl (..)
  , rExprDeclSchemaIdentity
  )
import MyFramework.CURDE.Types
  ( FieldName
  , SchemaIdentity (..)
  , SchemaShape (..)
  )
import MyFramework.Runtime.Value
  ( RuntimeData (..)
  , RuntimeDataBinding
  , RuntimeValueError
  , lookupRuntimeData
  , validateRuntimeData
  )

-- | Evaluation is deliberately structural. Business transformations are R
-- Facts in the EffectSystem and therefore enter through registered R handlers.
data RExprEvaluationError
  = LiteralSchemaMismatch SchemaIdentity LiteralValue
  | ProductSchemaMismatch SchemaIdentity
  | RuntimeValueRejected RuntimeValueError
  deriving (Eq, Show)

interpretRExprDecl ::
  [RuntimeDataBinding] ->
  RExprDecl ->
  Either RExprEvaluationError RuntimeData
interpretRExprDecl currentBindings currentExpression =
  case currentExpression of
    RReferenceDecl currentHandle currentSchema ->
      mapRuntimeValueError
        ( lookupRuntimeData
            currentHandle
            currentSchema
            currentBindings
        )
    RLiteralDecl currentSchema currentLiteral ->
      literalRuntimeData currentSchema currentLiteral
    RProductDecl currentSchema currentFields -> do
      let currentFieldSchemas =
            [ ( currentName
              , rExprDeclSchemaIdentity currentField
              )
            | (currentName, currentField) <- currentFields
            ]
      case schemaIdentityShape currentSchema of
        RecordShape expectedFields
          | currentFieldSchemas == expectedFields ->
              Right ()
          | otherwise ->
              Left (ProductSchemaMismatch currentSchema)
        ProductShape expectedItems
          | map snd currentFieldSchemas == expectedItems ->
              Right ()
          | otherwise ->
              Left (ProductSchemaMismatch currentSchema)
        _ ->
          Left (ProductSchemaMismatch currentSchema)
      currentValues <-
        traverse
          ( \(_, currentField) ->
              interpretRExprDecl
                currentBindings
                currentField
          )
          currentFields
      let currentData =
            case schemaIdentityShape currentSchema of
              RecordShape _ ->
                RuntimeRecord
                  (zip (map fst currentFields) currentValues)
              ProductShape _ ->
                RuntimeList currentValues
              _ ->
                RuntimeRecord
                  (zip (map fst currentFields) currentValues)
      validateEvaluated currentSchema currentData

literalRuntimeData ::
  SchemaIdentity ->
  LiteralValue ->
  Either RExprEvaluationError RuntimeData
literalRuntimeData currentSchema currentLiteral =
  case (schemaIdentityShape currentSchema, currentLiteral) of
    (UnitShape, LiteralUnit) ->
      Right RuntimeUnit
    (ScalarShape, LiteralBool currentValue) ->
      Right (RuntimeBool currentValue)
    (ScalarShape, LiteralInteger currentValue) ->
      Right (RuntimeInteger currentValue)
    (ScalarShape, LiteralDecimal currentValue) ->
      Right (RuntimeDecimal currentValue)
    (ScalarShape, LiteralText currentValue) ->
      Right (RuntimeText currentValue)
    (RecordShape currentFields, LiteralRecord currentValues)
      | sort (map fst currentFields)
          /= sort (map fst currentValues)
          || length currentFields /= length currentValues ->
          Left (LiteralSchemaMismatch currentSchema currentLiteral)
      | otherwise -> do
          nextValues <-
            traverse
              (literalRecordField currentValues)
              currentFields
          validateEvaluated currentSchema (RuntimeRecord nextValues)
    (ProductShape currentSchemas, LiteralList currentValues)
      | length currentSchemas == length currentValues -> do
          nextValues <-
            traverse
              (uncurry literalRuntimeData)
              (zip currentSchemas currentValues)
          validateEvaluated currentSchema (RuntimeList nextValues)
    _ ->
      Left (LiteralSchemaMismatch currentSchema currentLiteral)

literalRecordField ::
  [(FieldName, LiteralValue)] ->
  (FieldName, SchemaIdentity) ->
  Either RExprEvaluationError (FieldName, RuntimeData)
literalRecordField currentValues (currentName, currentSchema) =
  case
      [ currentLiteral
      | (fieldName, currentLiteral) <- currentValues
      , fieldName == currentName
      ] of
    [currentLiteral] -> do
      currentData <-
        literalRuntimeData currentSchema currentLiteral
      Right (currentName, currentData)
    _ ->
      Left
        (LiteralSchemaMismatch currentSchema (LiteralRecord currentValues))

validateEvaluated ::
  SchemaIdentity ->
  RuntimeData ->
  Either RExprEvaluationError RuntimeData
validateEvaluated currentSchema currentData = do
  mapRuntimeValueError
    (validateRuntimeData currentSchema currentData)
  Right currentData

mapRuntimeValueError ::
  Either RuntimeValueError value ->
  Either RExprEvaluationError value
mapRuntimeValueError =
  either (Left . RuntimeValueRejected) Right