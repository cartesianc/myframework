{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module MyFramework.CURDE.Expression
  ( OperatorRef (..)
  , LiteralValue (..)
  , FieldExpr (..)
  , RExpr
  , RExprDecl (..)
  , SomeRExpr (..)
  , rRef
  , literal
  , productRecord
  , project
  , applyOperator
  , rExprSchemaRef
  , rExprSchemaIdentity
  , rExprHandleReferences
  , eraseRExpr
  , eraseSomeRExpr
  , rExprDeclSchemaIdentity
  , rExprDeclHandleReferences
  , Implementation
  , ImplementationDecl (..)
  , SomeImplementation (..)
  , implC
  , implU
  , implD
  , implE
  , eraseImplementation
  , eraseSomeImplementation
  , implementationDeclTargetId
  , implementationDeclRReferences
  ) where

import MyFramework.CURDE.Types

-- | Stable registry identity for a serializable R algebra operator.
newtype OperatorRef = OperatorRef
  { operatorRefName :: String
  }
  deriving (Eq, Ord, Read, Show)

-- | Closed literal syntax. There is deliberately no host-language escape
-- hatch, function value, runtime resource, or opaque dynamic payload here.
data LiteralValue
  = LiteralUnit
  | LiteralBool Bool
  | LiteralInteger Integer
  | LiteralDecimal String
  | LiteralText String
  | LiteralList [LiteralValue]
  | LiteralRecord [(FieldName, LiteralValue)]
  deriving (Eq, Ord, Read, Show)

-- | Typed, closed, serializable R expression.
--
-- A reference can only name an R handle. CUDE observations are therefore not
-- public expression values: the declaring effect system must first expose an
-- R handle whose source is that private observation.
data RExpr value where
  RReference :: Handle name 'R () value -> RExpr value
  RLiteral :: SchemaRef value -> LiteralValue -> RExpr value
  RProduct :: SchemaRef value -> [FieldExpr] -> RExpr value
  RProjection :: OperatorRef -> SchemaRef value -> SomeRExpr -> RExpr value
  ROperator :: OperatorRef -> SchemaRef value -> [SomeRExpr] -> RExpr value

data FieldExpr where
  FieldExpr :: FieldName -> RExpr value -> FieldExpr

data SomeRExpr where
  SomeRExpr :: RExpr value -> SomeRExpr

-- | Fully erased serialization form. Deserialization may construct malformed
-- declarations, so Validate checks every reference and schema again.
data RExprDecl
  = RReferenceDecl HandleId SchemaIdentity
  | RLiteralDecl SchemaIdentity LiteralValue
  | RProductDecl SchemaIdentity [(FieldName, RExprDecl)]
  | RProjectionDecl OperatorRef SchemaIdentity RExprDecl
  | ROperatorDecl OperatorRef SchemaIdentity [RExprDecl]
  deriving (Eq, Ord, Read, Show)

instance Eq SomeRExpr where
  left == right =
    eraseSomeRExpr left == eraseSomeRExpr right

instance Ord SomeRExpr where
  compare left right =
    compare (eraseSomeRExpr left) (eraseSomeRExpr right)

instance Show SomeRExpr where
  show =
    show . eraseSomeRExpr

instance Show FieldExpr where
  show =
    show . eraseFieldExpr

rRef :: Handle name 'R () value -> RExpr value
rRef =
  RReference

literal :: SchemaRef value -> LiteralValue -> RExpr value
literal =
  RLiteral

productRecord :: SchemaRef value -> [FieldExpr] -> RExpr value
productRecord =
  RProduct

project :: OperatorRef -> SchemaRef value -> SomeRExpr -> RExpr value
project =
  RProjection

applyOperator :: OperatorRef -> SchemaRef value -> [SomeRExpr] -> RExpr value
applyOperator =
  ROperator

rExprSchemaRef :: RExpr value -> SchemaRef value
rExprSchemaRef currentExpr =
  case currentExpr of
    RReference currentHandle ->
      handleResultSchemaRef currentHandle
    RLiteral currentSchema _ ->
      currentSchema
    RProduct currentSchema _ ->
      currentSchema
    RProjection _ currentSchema _ ->
      currentSchema
    ROperator _ currentSchema _ ->
      currentSchema

rExprSchemaIdentity :: RExpr value -> SchemaIdentity
rExprSchemaIdentity =
  schemaRefIdentityOf . rExprSchemaRef

rExprHandleReferences :: RExpr value -> [HandleId]
rExprHandleReferences currentExpr =
  case currentExpr of
    RReference currentHandle ->
      [handleId currentHandle]
    RLiteral _ _ ->
      []
    RProduct _ currentFields ->
      concatMap fieldExprHandleReferences currentFields
    RProjection _ _ currentSource ->
      someRExprHandleReferences currentSource
    ROperator _ _ currentArguments ->
      concatMap someRExprHandleReferences currentArguments

eraseRExpr :: RExpr value -> RExprDecl
eraseRExpr currentExpr =
  case currentExpr of
    RReference currentHandle ->
      RReferenceDecl
        (handleId currentHandle)
        (schemaRefIdentityOf (handleResultSchemaRef currentHandle))
    RLiteral currentSchema currentValue ->
      RLiteralDecl
        (schemaRefIdentityOf currentSchema)
        currentValue
    RProduct currentSchema currentFields ->
      RProductDecl
        (schemaRefIdentityOf currentSchema)
        (map eraseFieldExpr currentFields)
    RProjection currentOperator currentSchema currentSource ->
      RProjectionDecl
        currentOperator
        (schemaRefIdentityOf currentSchema)
        (eraseSomeRExpr currentSource)
    ROperator currentOperator currentSchema currentArguments ->
      ROperatorDecl
        currentOperator
        (schemaRefIdentityOf currentSchema)
        (map eraseSomeRExpr currentArguments)

eraseSomeRExpr :: SomeRExpr -> RExprDecl
eraseSomeRExpr (SomeRExpr currentExpr) =
  eraseRExpr currentExpr

rExprDeclSchemaIdentity :: RExprDecl -> SchemaIdentity
rExprDeclSchemaIdentity currentExpr =
  case currentExpr of
    RReferenceDecl _ currentSchema ->
      currentSchema
    RLiteralDecl currentSchema _ ->
      currentSchema
    RProductDecl currentSchema _ ->
      currentSchema
    RProjectionDecl _ currentSchema _ ->
      currentSchema
    ROperatorDecl _ currentSchema _ ->
      currentSchema

rExprDeclHandleReferences :: RExprDecl -> [HandleId]
rExprDeclHandleReferences currentExpr =
  case currentExpr of
    RReferenceDecl currentHandle _ ->
      [currentHandle]
    RLiteralDecl _ _ ->
      []
    RProductDecl _ currentFields ->
      concatMap (rExprDeclHandleReferences . snd) currentFields
    RProjectionDecl _ _ currentSource ->
      rExprDeclHandleReferences currentSource
    ROperatorDecl _ _ currentArguments ->
      concatMap rExprDeclHandleReferences currentArguments

eraseFieldExpr :: FieldExpr -> (FieldName, RExprDecl)
eraseFieldExpr (FieldExpr currentName currentExpr) =
  (currentName, eraseRExpr currentExpr)

fieldExprHandleReferences :: FieldExpr -> [HandleId]
fieldExprHandleReferences (FieldExpr _ currentExpr) =
  rExprHandleReferences currentExpr

someRExprHandleReferences :: SomeRExpr -> [HandleId]
someRExprHandleReferences (SomeRExpr currentExpr) =
  rExprHandleReferences currentExpr

-- | A serializable binding of a CUDE handle to a closed argument expression.
-- It is not a runtime executable closure. The CommandKind witness makes an R
-- target unrepresentable.
data Implementation
  (kind :: CURDE)
  args
  observation where
  Implementation ::
    CommandKind kind ->
    Handle name kind args observation ->
    RExpr args ->
    Implementation kind args observation

instance Show (Implementation kind args observation) where
  show =
    show . eraseImplementation

data ImplementationDecl = ImplementationDecl
  { implementationDeclaredKind :: CURDE
  , implementationTarget :: HandleId
  , implementationArguments :: RExprDecl
  }
  deriving (Eq, Ord, Read, Show)

data SomeImplementation where
  SomeImplementation ::
    Implementation kind args observation ->
    SomeImplementation

instance Eq SomeImplementation where
  left == right =
    eraseSomeImplementation left == eraseSomeImplementation right

instance Ord SomeImplementation where
  compare left right =
    compare (eraseSomeImplementation left) (eraseSomeImplementation right)

instance Show SomeImplementation where
  show =
    show . eraseSomeImplementation

implC ::
  Handle name 'C args observation ->
  RExpr args ->
  Implementation 'C args observation
implC =
  Implementation CCommand

implU ::
  Handle name 'U args observation ->
  RExpr args ->
  Implementation 'U args observation
implU =
  Implementation UCommand

implD ::
  Handle name 'D args observation ->
  RExpr args ->
  Implementation 'D args observation
implD =
  Implementation DCommand

implE ::
  Handle name 'E args observation ->
  RExpr args ->
  Implementation 'E args observation
implE =
  Implementation ECommand

eraseImplementation ::
  Implementation kind args observation ->
  ImplementationDecl
eraseImplementation
  (Implementation currentKind currentHandle currentArguments) =
    ImplementationDecl
      { implementationDeclaredKind =
          case currentKind of
            CCommand -> C
            UCommand -> U
            DCommand -> D
            ECommand -> E
      , implementationTarget = handleId currentHandle
      , implementationArguments = eraseRExpr currentArguments
      }

eraseSomeImplementation :: SomeImplementation -> ImplementationDecl
eraseSomeImplementation (SomeImplementation currentImplementation) =
  eraseImplementation currentImplementation

implementationDeclTargetId :: ImplementationDecl -> HandleId
implementationDeclTargetId =
  implementationTarget

implementationDeclRReferences :: ImplementationDecl -> [HandleId]
implementationDeclRReferences =
  rExprDeclHandleReferences . implementationArguments
