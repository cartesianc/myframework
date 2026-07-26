module MyFramework.Runtime.Expression
  ( PureOperator (..)
  , PureOperatorRegistry
  , RExprEvaluationError (..)
  , emptyPureOperatorRegistry
  , interpretRExprDecl
  , pureOperatorRegistry
  , registerPureOperator
  ) where

import Control.Monad
  ( foldM )
import Data.List
  ( sort )
import qualified Data.Map.Strict as Map

import MyFramework.CURDE.Expression
  ( LiteralValue (..)
  , OperatorRef
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

-- | A runtime-only implementation of one serializable 'OperatorRef'.
-- Functions live in the Handler/runtime face and never cross the facade.
data PureOperator = PureOperator
  { pureOperatorInputSchemas :: [SchemaIdentity]
  , pureOperatorOutputSchema :: SchemaIdentity
  , runPureOperator ::
      [RuntimeData] ->
      Either String RuntimeData
  }

newtype PureOperatorRegistry = PureOperatorRegistry
  { pureOperatorBindings :: Map.Map OperatorRef PureOperator
  }

data RExprEvaluationError
  = DuplicatePureOperator OperatorRef
  | MissingPureOperator OperatorRef
  | OperatorInputSchemasMismatch
      OperatorRef
      [SchemaIdentity]
      [SchemaIdentity]
  | OperatorOutputSchemaMismatch
      OperatorRef
      SchemaIdentity
      SchemaIdentity
  | PureOperatorFailed OperatorRef String
  | LiteralSchemaMismatch SchemaIdentity LiteralValue
  | ProductSchemaMismatch SchemaIdentity
  | RuntimeValueRejected RuntimeValueError
  deriving (Eq, Show)

emptyPureOperatorRegistry :: PureOperatorRegistry
emptyPureOperatorRegistry =
  PureOperatorRegistry Map.empty

pureOperatorRegistry ::
  [(OperatorRef, PureOperator)] ->
  Either RExprEvaluationError PureOperatorRegistry
pureOperatorRegistry =
  foldM
    (\currentRegistry (currentRef, currentOperator) ->
        registerPureOperator
          currentRef
          currentOperator
          currentRegistry
    )
    emptyPureOperatorRegistry

registerPureOperator ::
  OperatorRef ->
  PureOperator ->
  PureOperatorRegistry ->
  Either RExprEvaluationError PureOperatorRegistry
registerPureOperator currentRef currentOperator currentRegistry
  | Map.member currentRef (pureOperatorBindings currentRegistry) =
      Left (DuplicatePureOperator currentRef)
  | otherwise =
      Right
        ( PureOperatorRegistry
            ( Map.insert
                currentRef
                currentOperator
                (pureOperatorBindings currentRegistry)
            )
        )

interpretRExprDecl ::
  PureOperatorRegistry ->
  [RuntimeDataBinding] ->
  RExprDecl ->
  Either RExprEvaluationError RuntimeData
interpretRExprDecl currentRegistry currentBindings currentExpression =
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
                currentRegistry
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
    RProjectionDecl currentRef currentSchema currentSource ->
      interpretOperator
        currentRef
        currentSchema
        [currentSource]
    ROperatorDecl currentRef currentSchema currentArguments ->
      interpretOperator
        currentRef
        currentSchema
        currentArguments
  where
    interpretOperator currentRef currentSchema currentArguments = do
      currentOperator <-
        case
            Map.lookup
              currentRef
              (pureOperatorBindings currentRegistry) of
          Nothing ->
            Left (MissingPureOperator currentRef)
          Just currentBinding ->
            Right currentBinding
      let currentInputSchemas =
            map rExprDeclSchemaIdentity currentArguments
      if pureOperatorInputSchemas currentOperator
          == currentInputSchemas
        then Right ()
        else
          Left
            ( OperatorInputSchemasMismatch
                currentRef
                (pureOperatorInputSchemas currentOperator)
                currentInputSchemas
            )
      if pureOperatorOutputSchema currentOperator
          == currentSchema
        then Right ()
        else
          Left
            ( OperatorOutputSchemaMismatch
                currentRef
                (pureOperatorOutputSchema currentOperator)
                currentSchema
            )
      currentValues <-
        traverse
          ( interpretRExprDecl
              currentRegistry
              currentBindings
          )
          currentArguments
      currentValue <-
        case runPureOperator currentOperator currentValues of
          Left currentMessage ->
            Left
              (PureOperatorFailed currentRef currentMessage)
          Right nextValue ->
            Right nextValue
      validateEvaluated currentSchema currentValue

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
