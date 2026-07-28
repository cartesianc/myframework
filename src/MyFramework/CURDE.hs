module MyFramework.CURDE
  ( CURDE (..)
  , SCURDE (..)
  , curdeValue
  , EffectSystemName (..)
  , HandleId (..)
  , renderHandleId
  , FieldName (..)
  , SchemaIdentity (..)
  , SchemaShape (..)
  , SchemaRef
  , schemaRef
  , scalarSchema
  , recordSchema
  , productSchema
  , schemaRefName
  , schemaRefShape
  , schemaRefIdentityOf
  , unitSchema
  , NoObservation
  , noObservationSchema
  , ObservationSpec (..)
  , ObservationContract (..)
  , ReadSource (..)
  , CommandKind (..)
  , CommandSpec (..)
  , ReadSpec (..)
  , Handle
  , SomeHandleRef (..)
  , SomeCommandHandleRef (..)
  , HandleDecl (..)
  , eraseHandle
  , eraseSomeHandle
  , c
  , u
  , r
  , d
  , e
  , handleId
  , handleKind
  , handleArgumentSchemaRef
  , handleResultSchemaRef
  , handleInput
  , handleReadSource
  , handleCommandKind
  , handleObservationSchemaRef
  , handlePublicValueSchemaRef
  , someHandleId
  , someHandleKind
  , someHandleArgumentSchemaIdentity
  , someHandleObservationContract
  , someHandlePublicValueSchemaIdentity
  , someHandleInput
  , someHandleReadSource
  , someCommandHandleId
  , someCommandHandleKind
  , someCommandHandleArgumentSchemaIdentity
  , EffectSystem (..)
  , effectSystem
  , effectSystemHandleIds
  , EffectSystemDecl (..)
  , eraseEffectSystem
  , effectSystemDeclHandleIds
  , LiteralValue (..)
  , FieldExpr (..)
  , RExpr
  , RExprDecl (..)
  , rRef
  , literal
  , productRecord
  , rExprSchemaRef
  , rExprSchemaIdentity
  , rExprHandleReferences
  , eraseRExpr
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
  , FieldPath (..)
  , renderFieldPath
  , ImplementationId (..)
  , implementationIdFor
  , renderImplementationId
  , DemandNodeId (..)
  , LoweringResult
  , loweringPassed
  , loweringValidationErrors
  , handleRefFor
  , handleDeclRefFor
  , lowerCURDE
  , lowerCURDEDecl
  , ReferenceSite (..)
  , ValidationError (..)
  , sortValidationErrors
  , validationPassed
  , validateEffectSystems
  , validateImplementationCatalog
  , RecordError (..)
  , RecordHandles (recordHandles)
  , effectSystemFromRecord
  , sortRecordErrors
  ) where

import MyFramework.CURDE.Core
  ( DemandNodeId (..)
  , FieldPath (..)
  , ImplementationId (..)
  , implementationIdFor
  , renderFieldPath
  , renderImplementationId
  )
import MyFramework.CURDE.Expression
  ( FieldExpr (..)
  , Implementation
  , ImplementationDecl (..)
  , LiteralValue (..)
  , RExpr
  , RExprDecl (..)
  , SomeImplementation (..)
  , eraseImplementation
  , eraseRExpr
  , eraseSomeImplementation
  , implC
  , implD
  , implE
  , implU
  , implementationDeclRReferences
  , implementationDeclTargetId
  , literal
  , productRecord
  , rExprDeclHandleReferences
  , rExprDeclSchemaIdentity
  , rExprHandleReferences
  , rExprSchemaIdentity
  , rExprSchemaRef
  , rRef
  )
import MyFramework.CURDE.Lowering
  ( LoweringResult
  , handleDeclRefFor
  , handleRefFor
  , lowerCURDE
  , lowerCURDEDecl
  , loweringErrors
  , loweringPassed
  )
import MyFramework.CURDE.Record
  ( RecordError (..)
  , RecordHandles (recordHandles)
  , effectSystemFromRecord
  , sortRecordErrors
  )
import MyFramework.CURDE.Types
  ( CURDE (..)
  , CommandKind (..)
  , CommandSpec (..)
  , EffectSystem (..)
  , EffectSystemDecl (..)
  , EffectSystemName (..)
  , FieldName (..)
  , Handle
  , HandleDecl (..)
  , HandleId (..)
  , NoObservation
  , ObservationContract (..)
  , ObservationSpec (..)
  , ReadSource (..)
  , ReadSpec (..)
  , SCURDE (..)
  , SchemaIdentity (..)
  , SchemaRef
  , SchemaShape (..)
  , SomeCommandHandleRef (..)
  , SomeHandleRef (..)
  , c
  , curdeValue
  , d
  , e
  , effectSystem
  , effectSystemDeclHandleIds
  , effectSystemHandleIds
  , eraseEffectSystem
  , eraseHandle
  , eraseSomeHandle
  , handleArgumentSchemaRef
  , handleCommandKind
  , handleId
  , handleInput
  , handleKind
  , handleObservationSchemaRef
  , handlePublicValueSchemaRef
  , handleReadSource
  , handleResultSchemaRef
  , noObservationSchema
  , productSchema
  , r
  , recordSchema
  , renderHandleId
  , scalarSchema
  , schemaRef
  , schemaRefIdentityOf
  , schemaRefName
  , schemaRefShape
  , someCommandHandleArgumentSchemaIdentity
  , someCommandHandleId
  , someCommandHandleKind
  , someHandleArgumentSchemaIdentity
  , someHandleId
  , someHandleInput
  , someHandleKind
  , someHandleObservationContract
  , someHandlePublicValueSchemaIdentity
  , someHandleReadSource
  , u
  , unitSchema
  )
import MyFramework.CURDE.Validate
  ( ReferenceSite (..)
  , ValidationError (..)
  , sortValidationErrors
  , validateEffectSystems
  , validateImplementationCatalog
  , validationPassed
  )

-- | Stable public projection of lowering diagnostics. The lowered demand
-- graph remains an internal compiler value.
loweringValidationErrors :: LoweringResult -> [ValidationError]
loweringValidationErrors =
  loweringErrors
