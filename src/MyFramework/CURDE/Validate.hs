module MyFramework.CURDE.Validate
  ( ReferenceSite (..)
  , ValidationError (..)
  , sortValidationErrors
  , validationPassed
  , validateHandleReference
  , validateEffectSystems
  , validateImplementationCatalog
  , validateImplementationContract
  , validateDemandGraph
  , validateReadConsumption
  ) where

import Data.Graph
  ( SCC (..)
  , stronglyConnComp
  )
import Data.List
  ( group
  , sort
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.CURDE.Core
import MyFramework.CURDE.Expression
import MyFramework.CURDE.Types

data ReferenceSite
  = HandleInputSite HandleId
  | EffectSystemPrivateSite EffectSystemName
  | EffectSystemExportSite EffectSystemName
  | AstHandleSite AstPath
  | AstStatusSite AstPath
  | AstCallbackSite AstPath
  | AstSuspenseSite AstPath
  | ImplementationTargetSite AstPath
  | ImplementationArgumentSite AstPath FieldPath
  deriving (Eq, Ord, Show)

-- | Validation errors are values rather than early exits. Lowering accumulates
-- every independent error and returns a normalized list.
data ValidationError
  = EmptyEffectSystemName
  | EmptyHandleLocalName HandleId
  | EmptySchemaName SchemaIdentity
  | DuplicateEffectSystem EffectSystemName
  | UnknownEffectSystemImport EffectSystemName EffectSystemName
  | DuplicateEffectSystemImport EffectSystemName EffectSystemName
  | DuplicateHandle HandleId
  | HandleDeclaredInWrongSystem EffectSystemName HandleId
  | PrivateHandleOutsideSystem EffectSystemName HandleId
  | ExportedHandleOutsideSystem EffectSystemName HandleId
  | PrivateExportHandleOverlap EffectSystemName HandleId
  | DuplicatePrivateHandle EffectSystemName HandleId
  | DuplicateExportedHandle EffectSystemName HandleId
  | UnknownHandleReference ReferenceSite HandleId
  | HandleInputSystemNotImported HandleId EffectSystemName HandleId
  | HandleInputNotExported HandleId HandleId
  | InputCycle [HandleId]
  | ReadArgumentSchemaNotUnit HandleId SchemaIdentity
  | CommandObservationMissing HandleId
  | CommandPublicValueExposed HandleId SchemaIdentity
  | CommandReadSourcePresent HandleId ReadSource
  | ReadObservationExposed HandleId ObservationContract
  | ReadPublicValueMissing HandleId
  | ReadSourceMissing HandleId
  | ObservationInputMissing HandleId
  | ValueInputMissing HandleId
  | ObservationInputKindMismatch HandleId HandleId CURDE
  | ValueInputKindMismatch HandleId HandleId CURDE
  | ObservationNotCaptured HandleId HandleId
  | ObservationSchemaMismatch HandleId HandleId SchemaIdentity SchemaIdentity
  | ValueInputSchemaMismatch HandleId HandleId SchemaIdentity SchemaIdentity
  | PrivateObservationCrossSystem HandleId HandleId
  | CommandInputCannotBeReadValue HandleId HandleId
  | ConflictingImplementation HandleId
  | InvalidImplementationTarget AstPath HandleId CURDE
  | ImplementationKindMismatch AstPath HandleId CURDE CURDE
  | ImplementationSchemaMismatch AstPath HandleId SchemaIdentity SchemaIdentity
  | ImplementationReadNotVisible AstPath HandleId HandleId
  | DuplicateExpressionField AstPath FieldPath FieldName
  | ExpressionReferenceSchemaMismatch
      AstPath
      FieldPath
      HandleId
      SchemaIdentity
      SchemaIdentity
  | PublicStatusUsedAsValue AstPath FieldPath HandleId CURDE
  | UnknownAstHandleReference AstPath String
  | InvalidRootKind AstPath HandleId CURDE
  | ParameterizedHandleLeaf AstPath HandleId SchemaIdentity
  | MissingImplementation AstPath HandleId SchemaIdentity
  | ImplementationNotInScope AstPath HandleId
  | DemandGraphUnknownNode DemandNodeId
  | DemandCycle [DemandNodeId]
  | UnconsumedRead HandleId
  | UnsupportedContextNode AstPath
  | UnsupportedHangingRoot AstPath
  deriving (Eq, Ord, Show)

sortValidationErrors :: [ValidationError] -> [ValidationError]
sortValidationErrors =
  Set.toAscList . Set.fromList

validationPassed :: [ValidationError] -> Bool
validationPassed =
  null

validateHandleReference ::
  Map.Map HandleId HandleDecl ->
  ReferenceSite ->
  HandleId ->
  [ValidationError]
validateHandleReference handleIndex currentSite currentReference =
  [ UnknownHandleReference currentSite currentReference
  | Map.notMember currentReference handleIndex
  ]

validateEffectSystems :: [EffectSystemDecl] -> [ValidationError]
validateEffectSystems currentSystems =
  sortValidationErrors
    ( identityErrors
        ++ duplicateSystemErrors
        ++ importErrors
        ++ duplicateHandleErrors
        ++ wrongSystemErrors
        ++ visibilityDeclarationErrors
        ++ referenceErrors
        ++ inputVisibilityErrors
        ++ inputCycleErrors
        ++ readSourceErrors
    )
  where
    currentHandles =
      concatMap effectSystemDeclHandles currentSystems
    handleIndex =
      Map.fromList
        [ (handleDeclId currentHandle, currentHandle)
        | currentHandle <- currentHandles
        ]
    knownSystemNames =
      Set.fromList (map effectSystemDeclName currentSystems)
    exportsBySystem =
      Map.fromListWith
        Set.union
        [ (effectSystemDeclName currentSystem, visibleExportHandleIds currentSystem)
        | currentSystem <- currentSystems
        ]
    identityErrors =
      [ EmptyEffectSystemName
      | any (null . effectSystemNameText . effectSystemDeclName) currentSystems
      ]
        ++ [ EmptyHandleLocalName (handleDeclId currentHandle)
           | currentHandle <- currentHandles
           , null (handleIdLocalName (handleDeclId currentHandle))
           ]
        ++ concatMap handleSchemaIdentityErrors currentHandles
    duplicateSystemErrors =
      [ DuplicateEffectSystem currentName
      | currentName <- duplicates (map effectSystemDeclName currentSystems)
      ]
    importErrors =
      concatMap
        (effectSystemImportErrors knownSystemNames)
        currentSystems
    duplicateHandleErrors =
      [ DuplicateHandle currentId
      | currentId <- duplicates (map handleDeclId currentHandles)
      ]
    wrongSystemErrors =
      [ HandleDeclaredInWrongSystem
          (effectSystemDeclName currentSystem)
          (handleDeclId currentHandle)
      | currentSystem <- currentSystems
      , currentHandle <- effectSystemDeclHandles currentSystem
      , handleIdEffectSystem (handleDeclId currentHandle)
          /= effectSystemDeclName currentSystem
      ]
    visibilityDeclarationErrors =
      concatMap effectSystemVisibilityDeclarationErrors currentSystems
    referenceErrors =
      concatMap (effectSystemReferenceErrors handleIndex) currentSystems
        ++ concatMap (handleInputReferenceErrors handleIndex) currentHandles
    inputVisibilityErrors =
      concatMap
        (effectSystemInputVisibilityErrors handleIndex exportsBySystem)
        currentSystems
    inputCycleErrors =
      map InputCycle (handleCycles currentHandles)
    readSourceErrors =
      concatMap handleChannelContractErrors currentHandles
        ++ concatMap (readSourceContractErrors handleIndex) currentHandles

visibleExportHandleIds :: EffectSystemDecl -> Set.Set HandleId
visibleExportHandleIds currentSystem =
  (exportedHandleIds `Set.intersection` ownedHandleIds)
    `Set.difference` privateHandleIds
  where
    ownedHandleIds =
      Set.fromList (effectSystemDeclHandleIds currentSystem)
    privateHandleIds =
      Set.fromList (effectSystemDeclPrivate currentSystem)
    exportedHandleIds =
      Set.fromList (effectSystemDeclExports currentSystem)

handleSchemaIdentityErrors :: HandleDecl -> [ValidationError]
handleSchemaIdentityErrors currentHandle =
  [ EmptySchemaName currentSchema
  | currentSchema <-
      handleDeclArgumentSchema currentHandle
        : ( maybeToList
              (handleDeclPublicValueSchema currentHandle)
              ++ observationSchemas
          )
  , null (schemaIdentityName currentSchema)
  ]
  where
    observationSchemas =
      case handleDeclObservation currentHandle of
        Just (ObservationCaptured currentSchema) ->
          [currentSchema]
        _ ->
          []

effectSystemImportErrors ::
  Set.Set EffectSystemName ->
  EffectSystemDecl ->
  [ValidationError]
effectSystemImportErrors knownSystemNames currentSystem =
  unknownImportErrors ++ duplicateImportErrors
  where
    currentName =
      effectSystemDeclName currentSystem
    currentImports =
      effectSystemDeclImports currentSystem
    unknownImportErrors =
      [ UnknownEffectSystemImport currentName currentImport
      | currentImport <- currentImports
      , currentImport `Set.notMember` knownSystemNames
      ]
    duplicateImportErrors =
      [ DuplicateEffectSystemImport currentName currentImport
      | currentImport <- duplicates currentImports
      ]

effectSystemVisibilityDeclarationErrors ::
  EffectSystemDecl ->
  [ValidationError]
effectSystemVisibilityDeclarationErrors currentSystem =
  privateOutsideErrors
    ++ exportedOutsideErrors
    ++ overlapErrors
    ++ duplicatePrivateErrors
    ++ duplicateExportErrors
  where
    currentName =
      effectSystemDeclName currentSystem
    ownedHandleIds =
      Set.fromList (effectSystemDeclHandleIds currentSystem)
    currentPrivateIds =
      effectSystemDeclPrivate currentSystem
    currentExportIds =
      effectSystemDeclExports currentSystem
    privateHandleIds =
      Set.fromList currentPrivateIds
    exportedHandleIds =
      Set.fromList currentExportIds
    privateOutsideErrors =
      [ PrivateHandleOutsideSystem currentName currentId
      | currentId <-
          Set.toAscList (privateHandleIds `Set.difference` ownedHandleIds)
      ]
    exportedOutsideErrors =
      [ ExportedHandleOutsideSystem currentName currentId
      | currentId <-
          Set.toAscList (exportedHandleIds `Set.difference` ownedHandleIds)
      ]
    overlapErrors =
      [ PrivateExportHandleOverlap currentName currentId
      | currentId <-
          Set.toAscList (privateHandleIds `Set.intersection` exportedHandleIds)
      ]
    duplicatePrivateErrors =
      [ DuplicatePrivateHandle currentName currentId
      | currentId <- duplicates currentPrivateIds
      ]
    duplicateExportErrors =
      [ DuplicateExportedHandle currentName currentId
      | currentId <- duplicates currentExportIds
      ]

effectSystemReferenceErrors ::
  Map.Map HandleId HandleDecl ->
  EffectSystemDecl ->
  [ValidationError]
effectSystemReferenceErrors handleIndex currentSystem =
  concatMap
    ( validateHandleReference
        handleIndex
        (EffectSystemPrivateSite (effectSystemDeclName currentSystem))
    )
    (effectSystemDeclPrivate currentSystem)
    ++ concatMap
      ( validateHandleReference
          handleIndex
          (EffectSystemExportSite (effectSystemDeclName currentSystem))
      )
      (effectSystemDeclExports currentSystem)

handleInputReferenceErrors ::
  Map.Map HandleId HandleDecl ->
  HandleDecl ->
  [ValidationError]
handleInputReferenceErrors handleIndex currentHandle =
  case handleDeclInput currentHandle of
    Nothing ->
      []
    Just currentInput ->
      validateHandleReference
        handleIndex
        (HandleInputSite (handleDeclId currentHandle))
        currentInput

effectSystemInputVisibilityErrors ::
  Map.Map HandleId HandleDecl ->
  Map.Map EffectSystemName (Set.Set HandleId) ->
  EffectSystemDecl ->
  [ValidationError]
effectSystemInputVisibilityErrors handleIndex exportsBySystem currentSystem =
  concatMap inputVisibilityErrors (effectSystemDeclHandles currentSystem)
  where
    currentSystemName =
      effectSystemDeclName currentSystem
    importedSystems =
      Set.fromList (effectSystemDeclImports currentSystem)
    inputVisibilityErrors currentHandle =
      case handleDeclInput currentHandle of
        Nothing ->
          []
        Just currentInput
          | inputSystem == currentSystemName ->
              []
          | inputSystem `Set.notMember` importedSystems ->
              [ HandleInputSystemNotImported
                  currentHandleId
                  inputSystem
                  inputId
              ]
          | Map.notMember inputId handleIndex ->
              []
          | inputId
              `Set.member` Map.findWithDefault
                Set.empty
                inputSystem
                exportsBySystem ->
              []
          | otherwise ->
              [HandleInputNotExported currentHandleId inputId]
          where
            currentHandleId =
              handleDeclId currentHandle
            inputId =
              currentInput
            inputSystem =
              handleIdEffectSystem inputId

handleChannelContractErrors :: HandleDecl -> [ValidationError]
handleChannelContractErrors currentHandle =
  case handleDeclKind currentHandle of
    R ->
      [ ReadArgumentSchemaNotUnit currentId argumentSchema
      | argumentSchema /= schemaRefIdentityOf unitSchema
      ]
        ++ [ ReadObservationExposed currentId currentObservation
           | Just currentObservation <- [handleDeclObservation currentHandle]
           ]
        ++ [ ReadPublicValueMissing currentId
           | handleDeclPublicValueSchema currentHandle == Nothing
           ]
        ++ [ ReadSourceMissing currentId
           | handleDeclReadSource currentHandle == Nothing
           ]
    _ ->
      [ CommandObservationMissing currentId
      | handleDeclObservation currentHandle == Nothing
      ]
        ++ [ CommandPublicValueExposed currentId currentSchema
           | Just currentSchema <- [handleDeclPublicValueSchema currentHandle]
           ]
        ++ [ CommandReadSourcePresent currentId currentSource
           | Just currentSource <- [handleDeclReadSource currentHandle]
           ]
  where
    currentId =
      handleDeclId currentHandle
    argumentSchema =
      handleDeclArgumentSchema currentHandle

readSourceContractErrors ::
  Map.Map HandleId HandleDecl ->
  HandleDecl ->
  [ValidationError]
readSourceContractErrors handleIndex currentHandle =
  commandInputErrors
    ++ readInputErrors
  where
    currentId =
      handleDeclId currentHandle
    commandInputErrors =
      case handleDeclInput currentHandle >>= (`Map.lookup` handleIndex) of
        Just currentInput
          | handleDeclKind currentHandle /= R
          , handleDeclKind currentInput == R ->
              [ CommandInputCannotBeReadValue
                  currentId
                  (handleDeclId currentInput)
              ]
        _ ->
          []
    readInputErrors
      | handleDeclKind currentHandle /= R =
          []
      | otherwise =
          case handleDeclReadSource currentHandle of
            Just ReadFromInputObservation ->
              case handleDeclInput currentHandle of
                Nothing ->
                  [ObservationInputMissing currentId]
                Just inputReference ->
                  case Map.lookup inputReference handleIndex of
                    Nothing ->
                      []
                    Just inputHandle ->
                      validateObservationInput currentHandle inputHandle
            Just ReadFromInputValue ->
              case handleDeclInput currentHandle of
                Nothing ->
                  [ValueInputMissing currentId]
                Just inputReference ->
                  case Map.lookup inputReference handleIndex of
                    Nothing ->
                      []
                    Just inputHandle ->
                      validateValueInput currentHandle inputHandle
            _ ->
              []

validateObservationInput ::
  HandleDecl ->
  HandleDecl ->
  [ValidationError]
validateObservationInput currentRead currentInput =
  kindErrors
    ++ privacyErrors
    ++ captureErrors
    ++ schemaErrors
  where
    readId =
      handleDeclId currentRead
    inputId =
      handleDeclId currentInput
    inputKind =
      handleDeclKind currentInput
    readSchema =
      handleDeclPublicValueSchema currentRead
    inputObservation =
      handleDeclObservation currentInput
    kindErrors =
      [ ObservationInputKindMismatch readId inputId inputKind
      | inputKind == R
      ]
    privacyErrors =
      [ PrivateObservationCrossSystem readId inputId
      | handleIdEffectSystem readId /= handleIdEffectSystem inputId
      ]
    captureErrors =
      [ ObservationNotCaptured readId inputId
      | inputKind /= R
      , case inputObservation of
          Just (ObservationCaptured _) -> False
          _ -> True
      ]
    schemaErrors =
      case (readSchema, inputObservation) of
        (Just expectedSchema, Just (ObservationCaptured actualSchema))
          | expectedSchema /= actualSchema ->
              [ ObservationSchemaMismatch
                  readId
                  inputId
                  expectedSchema
                  actualSchema
              ]
        _ ->
          []

validateValueInput ::
  HandleDecl ->
  HandleDecl ->
  [ValidationError]
validateValueInput currentRead currentInput =
  kindErrors ++ schemaErrors
  where
    readId =
      handleDeclId currentRead
    inputId =
      handleDeclId currentInput
    inputKind =
      handleDeclKind currentInput
    kindErrors =
      [ ValueInputKindMismatch readId inputId inputKind
      | inputKind /= R
      ]
    schemaErrors =
      case
        ( handleDeclPublicValueSchema currentRead
        , handleDeclPublicValueSchema currentInput
        ) of
        (Just expectedSchema, Just actualSchema)
          | expectedSchema /= actualSchema ->
              [ ValueInputSchemaMismatch
                  readId
                  inputId
                  expectedSchema
                  actualSchema
              ]
        _ ->
          []

validateImplementationCatalog ::
  [ImplementationDecl] ->
  [ValidationError]
validateImplementationCatalog currentImplementations =
  sortValidationErrors conflictErrors
  where
    grouped =
      Map.fromListWith
        (++)
        [ (implementationDeclTargetId currentImplementation, [currentImplementation])
        | currentImplementation <- currentImplementations
        ]
    -- Repeated identical declarations are normal when the same serializable
    -- implementation value occurs in more than one lexical AST position.
    conflictErrors =
      [ ConflictingImplementation currentTarget
      | (currentTarget, currentGroup) <- Map.toAscList grouped
      , length (Set.fromList currentGroup) > 1
      ]

validateImplementationContract ::
  Map.Map HandleId HandleDecl ->
  Map.Map EffectSystemName EffectSystemDecl ->
  AstPath ->
  ImplementationDecl ->
  [ValidationError]
validateImplementationContract
  handleIndex systemIndex currentPath currentImplementation =
    sortValidationErrors
      ( targetErrors
          ++ kindErrors
          ++ schemaErrors
          ++ expressionErrors
      )
  where
    currentTargetId =
      implementationDeclTargetId currentImplementation
    targetErrors =
      [ UnknownHandleReference
          (ImplementationTargetSite currentPath)
          currentTargetId
      | Map.notMember currentTargetId handleIndex
      ]
    registeredTarget =
      Map.lookup currentTargetId handleIndex
    actualKind =
      maybe
        (implementationDeclaredKind currentImplementation)
        handleDeclKind
        registeredTarget
    kindErrors =
      [ InvalidImplementationTarget currentPath currentTargetId actualKind
      | actualKind == R
      ]
        ++ [ ImplementationKindMismatch
               currentPath
               currentTargetId
               (implementationDeclaredKind currentImplementation)
               actualKind
           | implementationDeclaredKind currentImplementation /= actualKind
           ]
    actualSchema =
      rExprDeclSchemaIdentity
        (implementationArguments currentImplementation)
    expectedSchema =
      maybe
        actualSchema
        handleDeclArgumentSchema
        registeredTarget
    schemaErrors =
      [ ImplementationSchemaMismatch
          currentPath
          currentTargetId
          expectedSchema
          actualSchema
      | expectedSchema /= actualSchema
      ]
    expressionErrors =
      validateRExprDecl
        handleIndex
        systemIndex
        currentTargetId
        currentPath
        (FieldPath ["arguments"])
        (implementationArguments currentImplementation)

validateRExprDecl ::
  Map.Map HandleId HandleDecl ->
  Map.Map EffectSystemName EffectSystemDecl ->
  HandleId ->
  AstPath ->
  FieldPath ->
  RExprDecl ->
  [ValidationError]
validateRExprDecl
  handleIndex systemIndex targetId currentPath currentFieldPath currentExpr =
    case currentExpr of
      RReferenceDecl currentHandle declaredSchema ->
        validateReference currentHandle declaredSchema
      RLiteralDecl currentSchema _ ->
        validateSchema currentSchema
      RProductDecl currentSchema currentFields ->
        validateSchema currentSchema
          ++ [ DuplicateExpressionField
                 currentPath
                 currentFieldPath
                 currentName
             | currentName <- duplicates (map fst currentFields)
             ]
          ++ concat
            [ validateRExprDecl
                handleIndex
                systemIndex
                targetId
                currentPath
                (appendFieldPath currentFieldPath (fieldNameText currentName))
                currentValue
            | (currentName, currentValue) <- currentFields
            ]
  where
    validateSchema currentSchema =
      [EmptySchemaName currentSchema | null (schemaIdentityName currentSchema)]
    validateReference currentHandle declaredSchema =
      case Map.lookup currentHandle handleIndex of
        Nothing ->
          [ UnknownHandleReference
              (ImplementationArgumentSite currentPath currentFieldPath)
              currentHandle
          ]
        Just registeredHandle ->
          kindErrors
            ++ schemaErrors
            ++ visibilityErrors registeredHandle
          where
            currentKind =
              handleDeclKind registeredHandle
            kindErrors =
              [ PublicStatusUsedAsValue
                  currentPath
                  currentFieldPath
                  currentHandle
                  currentKind
              | currentKind /= R
              ]
            schemaErrors =
              case handleDeclPublicValueSchema registeredHandle of
                Just registeredSchema
                  | registeredSchema /= declaredSchema ->
                      [ ExpressionReferenceSchemaMismatch
                          currentPath
                          currentFieldPath
                          currentHandle
                          registeredSchema
                          declaredSchema
                      ]
                _ ->
                  []
    visibilityErrors registeredHandle
      | referenceSystem == targetSystem =
          []
      | otherwise =
          case Map.lookup targetSystem systemIndex of
            Nothing ->
              [ImplementationReadNotVisible currentPath targetId referenceId]
            Just targetEffectSystem
              | referenceSystem
                  `notElem` effectSystemDeclImports targetEffectSystem ->
                  [ImplementationReadNotVisible currentPath targetId referenceId]
              | not (referenceIsExported referenceSystem referenceId) ->
                  [ImplementationReadNotVisible currentPath targetId referenceId]
              | otherwise ->
                  []
      where
        referenceId =
          handleDeclId registeredHandle
        referenceSystem =
          handleIdEffectSystem referenceId
        targetSystem =
          handleIdEffectSystem targetId
    referenceIsExported referenceSystem referenceId =
      case Map.lookup referenceSystem systemIndex of
        Nothing ->
          False
        Just referenceEffectSystem ->
          referenceId `Set.member` visibleExportHandleIds referenceEffectSystem

validateDemandGraph :: DemandGraph -> [ValidationError]
validateDemandGraph currentGraph =
  sortValidationErrors (unknownNodeErrors ++ cycleErrors)
  where
    nodeIds =
      Map.keysSet (demandGraphNodes currentGraph)
    referencedNodeIds =
      concat
        [ map demandEdgeDependent (demandGraphEdges currentGraph)
        , map demandEdgePrerequisite (demandGraphEdges currentGraph)
        , map rootDemandNode (demandGraphRoots currentGraph)
        ]
    unknownNodeErrors =
      [ DemandGraphUnknownNode currentNode
      | currentNode <-
          Set.toAscList
            (Set.fromList referencedNodeIds `Set.difference` nodeIds)
      ]
    cycleErrors =
      map DemandCycle (demandCycles currentGraph)

validateReadConsumption ::
  [EffectSystemDecl] ->
  DemandGraph ->
  [ValidationError]
validateReadConsumption currentSystems currentGraph =
  [ UnconsumedRead currentId
  | currentId <- registeredReads
  , currentId `Set.notMember` consumedReads
  ]
  where
    registeredReads =
      sort
        [ handleDeclId currentHandle
        | currentSystem <- currentSystems
        , currentHandle <- effectSystemDeclHandles currentSystem
        , handleDeclKind currentHandle == R
        ]
    consumedReads =
      Set.fromList
        [ currentId
        | currentEdge <- demandGraphEdges currentGraph
        , edgeConsumesValue (demandEdgeKind currentEdge)
        , HandleNode currentId <- [demandEdgePrerequisite currentEdge]
        ]
    edgeConsumesValue currentKind =
      case currentKind of
        ArgumentUse _ -> True
        InputDependency -> True
        _ -> False


handleCycles :: [HandleDecl] -> [[HandleId]]
handleCycles currentHandles =
  sort
    [ sort currentCycle
    | CyclicSCC currentCycle <- stronglyConnComp vertices
    ]
  where
    vertices =
      [ (currentId, currentId, inputIds currentHandle)
      | currentHandle <- sort currentHandles
      , let currentId = handleDeclId currentHandle
      ]
    inputIds currentHandle =
      case handleDeclInput currentHandle of
        Nothing ->
          []
        Just currentInput ->
          [currentInput]

demandCycles :: DemandGraph -> [[DemandNodeId]]
demandCycles currentGraph =
  sort
    [ sort currentCycle
    | CyclicSCC currentCycle <- stronglyConnComp vertices
    ]
  where
    edgesByDependent =
      Map.fromListWith
        (++)
        [ (demandEdgeDependent currentEdge, [demandEdgePrerequisite currentEdge])
        | currentEdge <- demandGraphEdges currentGraph
        ]
    vertices =
      [ ( currentNode
        , currentNode
        , Map.findWithDefault [] currentNode edgesByDependent
        )
      | currentNode <- Map.keys (demandGraphNodes currentGraph)
      ]

appendFieldPath :: FieldPath -> String -> FieldPath
appendFieldPath (FieldPath currentPath) currentSegment =
  FieldPath (currentPath ++ [currentSegment])

duplicates :: Ord item => [item] -> [item]
duplicates currentItems =
  [ currentItem
  | currentItem : _ : _ <- group (sort currentItems)
  ]


maybeToList :: Maybe item -> [item]
maybeToList currentValue =
  case currentValue of
    Nothing -> []
    Just currentItem -> [currentItem]
