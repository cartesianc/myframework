module MyFramework.CURDE.Lowering
  ( LoweringResult (..)
  , loweringPassed
  , handleRefFor
  , handleDeclRefFor
  , lowerCURDE
  , lowerCURDEDecl
  ) where

import Data.List
  ( sort
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.Ast
  ( Ast
  , AstBlueprint (..)
  , AstBlueprintSeed
  , AstF (..)
  , AstTarget (..)
  , ChoiceKey (..)
  , HandleRef (..)
  , StatusExpr (..)
  , lowerAstBlueprintSeed
  )
import MyFramework.CURDE.Core
import MyFramework.CURDE.Expression
import MyFramework.CURDE.Types
import MyFramework.CURDE.Validate
import MyFramework.Recursion
  ( cata )

-- Minimal AST adapter contract:
--
-- * HandleRef contains stable handle identity only.
-- * Serializable ImplementationDecl values live directly at AST binding sites.
-- * Ast is Fix AstF and is consumed only with MyFramework.Recursion.cata.
-- * AST contains no handler body, closure, registry lookup, or runtime resource.

data LoweringResult = LoweringResult
  { loweringCore :: CURDECore
  , loweringErrors :: [ValidationError]
  }

loweringPassed :: LoweringResult -> Bool
loweringPassed =
  validationPassed . loweringErrors

handleRefFor :: Handle name kind args result -> HandleRef
handleRefFor =
  HandleRef . renderHandleId . handleId

handleDeclRefFor :: HandleDecl -> HandleRef
handleDeclRefFor =
  HandleRef . renderHandleId . handleDeclId


-- | Typed authoring convenience. Implementations already live in the AST, so
-- the only extra action here is erasing typed effect-system declarations.
lowerCURDE ::
  [EffectSystem] ->
  AstBlueprintSeed ->
  LoweringResult
lowerCURDE currentSystems =
  lowerCURDEDecl (map eraseEffectSystem currentSystems)

-- | The complete serialized frontend is EffectSystemDecl plus AstBlueprintSeed.
-- Every explicit ImplementationDecl is collected from Leaf or
-- WithImplementation nodes by the catamorphism below.
lowerCURDEDecl ::
  [EffectSystemDecl] ->
  AstBlueprintSeed ->
  LoweringResult
lowerCURDEDecl currentSystems =
  lowerCURDECore currentSystems . lowerAstBlueprintSeed

lowerCURDECore ::
  [EffectSystemDecl] ->
  AstBlueprint ->
  LoweringResult
lowerCURDECore currentSystems currentBlueprint =
  LoweringResult
    { loweringCore =
        CURDECore
          { curdeCoreEffectSystems = canonicalSystems
          , curdeCoreImplementations =
              catalogDeclarations finalImplementationCatalog
          , curdeCoreAst = currentBlueprint
          , curdeCoreDemandGraph = finalGraph
          }
    , loweringErrors = finalErrors
    }
  where
    currentHandles =
      concatMap effectSystemDeclHandles canonicalSystems
    handleIndex =
      firstBy handleDeclId currentHandles
    handleRefIndex =
      firstBy (renderHandleId . handleDeclId) currentHandles
    loweringEnvironment =
      LoweringEnvironment
        { loweringHandleIndex = handleIndex
        , loweringHandleRefIndex = handleRefIndex
        }
    astSummary =
      lowerAstBlueprint loweringEnvironment currentBlueprint
    explicitImplementations =
      map snd (astSummaryImplementations astSummary)
    explicitImplementationCatalog =
      catalogImplementations explicitImplementations
    preliminaryGraph =
      normalizeDemandGraph
        ( buildDemandGraph
            handleIndex
            explicitImplementationCatalog
            (astSummaryRoots astSummary)
            (astSummaryImplementationOccurrences astSummary)
        )
    implicitImplementations =
      normalizeImplicitUnitImplementations
        handleIndex
        preliminaryGraph
    -- Canonical Unit bindings precede explicit occurrences. A matching
    -- explicit Unit declaration deduplicates; a non-canonical one conflicts.
    allImplementations =
      implicitImplementations ++ explicitImplementations
    finalImplementationCatalog =
      catalogImplementations allImplementations
    finalGraph =
      normalizeDemandGraph
        ( buildDemandGraph
            handleIndex
            finalImplementationCatalog
            (astSummaryRoots astSummary)
            (astSummaryImplementationOccurrences astSummary)
        )
    systemIndex =
      firstBy effectSystemDeclName canonicalSystems
    locatedImplementations =
      astSummaryImplementations astSummary
        ++ [ ( implicitImplementationPath currentImplementation
             , currentImplementation
             )
           | currentImplementation <- implicitImplementations
           ]
    finalErrors =
      sortValidationErrors
        ( validateEffectSystems currentSystems
            ++ validateImplementationCatalog allImplementations
            ++ concat
              [ validateLocatedImplementation
                  handleIndex
                  systemIndex
                  currentPath
                  currentImplementation
              | (currentPath, currentImplementation) <-
                  locatedImplementations
              ]
            ++ astSummaryErrors astSummary
            ++ validateRootSeeds
              handleIndex
              finalImplementationCatalog
              (astSummaryRoots astSummary)
            ++ validateDemandGraph finalGraph
            ++ validateReadConsumption currentSystems finalGraph
        )

    canonicalSystems =
      sort
        (map canonicalEffectSystemDecl currentSystems)

newtype ImplementationCatalog = ImplementationCatalog
  { catalogById :: Map.Map ImplementationId ImplementationDecl
  }

catalogDeclarations :: ImplementationCatalog -> [ImplementationDecl]
catalogDeclarations =
  Map.elems . catalogById

catalogImplementations :: [ImplementationDecl] -> ImplementationCatalog
catalogImplementations currentImplementations =
  ImplementationCatalog
    { catalogById =
        firstBy implementationIdFor currentImplementations
    }

data LoweringEnvironment = LoweringEnvironment
  { loweringHandleIndex :: Map.Map HandleId HandleDecl
  , loweringHandleRefIndex :: Map.Map String HandleDecl
  }

type BindingEnvironment = Map.Map HandleId ImplementationDecl

data RootSeed = RootSeed
  { rootSeedKind :: RootDemandKind
  , rootSeedPath :: AstPath
  , rootSeedNode :: DemandNodeId
  , rootSeedBindings :: BindingEnvironment
  }
  deriving (Eq, Show)

data AstSummary = AstSummary
  { astSummaryRoots :: [RootSeed]
  , astSummaryImplementations :: [(AstPath, ImplementationDecl)]
  , astSummaryImplementationOccurrences ::
      Map.Map ImplementationId [AstPath]
  , astSummaryErrors :: [ValidationError]
  }

emptyAstSummary :: AstSummary
emptyAstSummary =
  AstSummary
    { astSummaryRoots = []
    , astSummaryImplementations = []
    , astSummaryImplementationOccurrences = Map.empty
    , astSummaryErrors = []
    }

appendAstSummary :: AstSummary -> AstSummary -> AstSummary
appendAstSummary left right =
  AstSummary
    { astSummaryRoots =
        astSummaryRoots left ++ astSummaryRoots right
    , astSummaryImplementations =
        astSummaryImplementations left
          ++ astSummaryImplementations right
    , astSummaryImplementationOccurrences =
        Map.unionWith
          (++)
          (astSummaryImplementationOccurrences left)
          (astSummaryImplementationOccurrences right)
    , astSummaryErrors =
        astSummaryErrors left ++ astSummaryErrors right
    }

combineAstSummaries :: [AstSummary] -> AstSummary
combineAstSummaries =
  foldr appendAstSummary emptyAstSummary

type AstFold =
  RootDemandKind ->
  BindingEnvironment ->
  AstPath ->
  AstSummary

lowerAstBlueprint ::
  LoweringEnvironment ->
  AstBlueprint ->
  AstSummary
lowerAstBlueprint currentEnvironment currentBlueprint =
  addErrors hangingErrors bootSummary
  where
    bootSummary =
      lowerOneAst
        currentEnvironment
        BootRoot
        Map.empty
        (AstPath ["blueprint", "boot"])
        (astBlueprintBoot currentBlueprint)
    hangingErrors =
      [ UnsupportedHangingRoot
          (AstPath ["blueprint", "hanging", "item:" ++ show currentIndex])
      | (currentIndex, _) <-
          indexedItems (astBlueprintHanging currentBlueprint)
      ]

lowerOneAst ::
  LoweringEnvironment ->
  RootDemandKind ->
  BindingEnvironment ->
  AstPath ->
  Ast ->
  AstSummary
lowerOneAst currentEnvironment currentKind currentBindings currentPath currentAst =
  cata (astLoweringAlgebra currentEnvironment) currentAst
    currentKind
    currentBindings
    currentPath

astLoweringAlgebra ::
  LoweringEnvironment ->
  AstF AstFold ->
  AstFold
astLoweringAlgebra
  currentEnvironment currentLayer currentKind currentBindings currentPath =
    case currentLayer of
      Leaf currentTarget ->
        lowerLeaf
          currentEnvironment
          currentKind
          currentBindings
          currentPath
          currentTarget
      WithImplementation currentImplementation child ->
        addImplementationOccurrence
          currentPath
          currentImplementation
          ( addErrors
              bindingErrors
              ( child
                  currentKind
                  nextBindings
                  (appendAstPath currentPath "body")
              )
          )
        where
          (nextBindings, bindingErrors) =
            bindImplementation currentImplementation currentBindings
      Chain children ->
        lowerIndexedChildren "step" children
      Parallel children ->
        lowerIndexedChildren "branch" children
      Fallback children ->
        lowerIndexedChildren "branch" children
      Race children ->
        lowerIndexedChildren "branch" children
      Choice _ children ->
        combineAstSummaries
          [ child
              currentKind
              currentBindings
              ( appendAstPath
                  currentPath
                  ( "branch:" ++ show currentIndex
                      ++ ":" ++ choiceKeyText currentKey
                  )
              )
          | (currentIndex, (currentKey, child)) <- indexedItems children
          ]
      Wait currentStatus child ->
        addErrors currentStatusErrors childSummary
        where
          childSummary =
            child
              currentKind
              currentBindings
              (appendAstPath currentPath "body")
          currentStatusErrors =
            validateStatusReferences currentEnvironment currentPath currentStatus
      Loop child ->
        child currentKind currentBindings (appendAstPath currentPath "body")
      Middleware _ child ->
        child currentKind currentBindings (appendAstPath currentPath "body")
      Callback currentReference child ->
        case resolveHandle
          currentEnvironment
          (AstCallbackSite currentPath)
          currentReference of
          Left currentError ->
            addErrors [currentError] childSummary
          Right _ ->
            childSummary
        where
          childSummary =
            child currentKind currentBindings (appendAstPath currentPath "body")
      Suspense currentReference ->
        case resolveHandle
          currentEnvironment
          (AstSuspenseSite currentPath)
          currentReference of
          Left currentError ->
            addErrors [currentError] emptyAstSummary
          Right _ ->
            emptyAstSummary
      Context _ _ ->
        addErrors
          [UnsupportedContextNode currentPath]
          emptyAstSummary
  where
    lowerIndexedChildren currentPrefix children =
      combineAstSummaries
        [ child
            currentKind
            currentBindings
            (appendAstPath currentPath (currentPrefix ++ ":" ++ show currentIndex))
        | (currentIndex, child) <- indexedItems children
        ]

lowerLeaf ::
  LoweringEnvironment ->
  RootDemandKind ->
  BindingEnvironment ->
  AstPath ->
  AstTarget ->
  AstSummary
lowerLeaf
  currentEnvironment currentKind currentBindings currentPath currentTarget =
    case currentTarget of
      HandleTarget currentReference ->
        case resolveHandle
          currentEnvironment
          (AstHandleSite currentPath)
          currentReference of
          Left currentError ->
            addErrors [currentError] emptyAstSummary
          Right currentHandle ->
            emptyAstSummary
              { astSummaryRoots =
                  [ RootSeed
                      { rootSeedKind = currentKind
                      , rootSeedPath = currentPath
                      , rootSeedNode =
                          HandleNode (handleDeclId currentHandle)
                      , rootSeedBindings = currentBindings
                      }
                  ]
              }
      ImplementationTarget currentImplementation ->
        addImplementationOccurrence
          currentPath
          currentImplementation
          ( addErrors
              bindingErrors
              ( emptyAstSummary
                  { astSummaryRoots =
                      [ RootSeed
                          { rootSeedKind = currentKind
                          , rootSeedPath = currentPath
                          , rootSeedNode =
                              ImplementationNode
                                (implementationIdFor currentImplementation)
                          , rootSeedBindings = nextBindings
                          }
                      ]
                  }
              )
          )
        where
          (nextBindings, bindingErrors) =
            bindImplementation currentImplementation currentBindings

resolveHandle ::
  LoweringEnvironment ->
  ReferenceSite ->
  HandleRef ->
  Either ValidationError HandleDecl
resolveHandle currentEnvironment currentSite currentReference =
  case
    Map.lookup
      (handleRefText currentReference)
      (loweringHandleRefIndex currentEnvironment) of
    Nothing ->
      Left
        ( UnknownAstHandleReference
            (referenceSitePath currentSite)
            (handleRefText currentReference)
        )
    Just currentHandle ->
      Right currentHandle


referenceSitePath :: ReferenceSite -> AstPath
referenceSitePath currentSite =
  case currentSite of
    AstHandleSite currentPath -> currentPath
    AstStatusSite currentPath -> currentPath
    AstCallbackSite currentPath -> currentPath
    AstSuspenseSite currentPath -> currentPath
    _ -> AstPath []

validateStatusReferences ::
  LoweringEnvironment ->
  AstPath ->
  StatusExpr ->
  [ValidationError]
validateStatusReferences currentEnvironment currentPath currentStatus =
  case currentStatus of
    StatusOf currentReference ->
      case
        resolveHandle
          currentEnvironment
          (AstStatusSite currentPath)
          currentReference of
        Left currentError ->
          [currentError]
        Right _ ->
          []
    StatusAll currentExpressions ->
      concatMap
        (validateStatusReferences currentEnvironment currentPath)
        currentExpressions
    StatusAny currentExpressions ->
      concatMap
        (validateStatusReferences currentEnvironment currentPath)
        currentExpressions

bindImplementation ::
  ImplementationDecl ->
  BindingEnvironment ->
  (BindingEnvironment, [ValidationError])
bindImplementation currentImplementation currentBindings =
  case Map.lookup currentTarget currentBindings of
    Nothing ->
      (Map.insert currentTarget currentImplementation currentBindings, [])
    Just previousImplementation
      | previousImplementation == currentImplementation ->
          (currentBindings, [])
      | otherwise ->
          (currentBindings, [ConflictingImplementation currentTarget])
  where
    currentTarget =
      implementationDeclTargetId currentImplementation

addImplementationOccurrence ::
  AstPath ->
  ImplementationDecl ->
  AstSummary ->
  AstSummary
addImplementationOccurrence currentPath currentImplementation currentSummary =
  currentSummary
    { astSummaryImplementations =
        (currentPath, currentImplementation)
          : astSummaryImplementations currentSummary
    , astSummaryImplementationOccurrences =
        Map.insertWith
          (++)
          (implementationIdFor currentImplementation)
          [currentPath]
          (astSummaryImplementationOccurrences currentSummary)
    }

addErrors :: [ValidationError] -> AstSummary -> AstSummary
addErrors currentErrors currentSummary =
  currentSummary
    { astSummaryErrors =
        currentErrors ++ astSummaryErrors currentSummary
    }

buildDemandGraph ::
  Map.Map HandleId HandleDecl ->
  ImplementationCatalog ->
  [RootSeed] ->
  Map.Map ImplementationId [AstPath] ->
  DemandGraph
buildDemandGraph
  handleIndex
  currentCatalog
  currentRoots
  currentImplementationOccurrences =
    emptyDemandGraph
      { demandGraphNodes =
          Map.union handleNodes implementationNodes
      , demandGraphEdges =
          handleEdges
            ++ implementationEdges
            ++ invocationEdges
      , demandGraphRoots =
          map rootDemandFromSeed currentRoots
      , demandGraphOccurrences =
          Map.unionWith
            (++)
            implementationOccurrences
            (rootOccurrences currentRoots)
      }
  where
    handleNodes =
      Map.fromList
        [ (HandleNode currentId, DemandHandleNode currentHandle)
        | (currentId, currentHandle) <- Map.toAscList handleIndex
        ]
    implementationNodes =
      Map.fromList
        [ ( ImplementationNode currentId
          , DemandImplementationNode currentId currentImplementation
          )
        | (currentId, currentImplementation) <-
            Map.toAscList (catalogById currentCatalog)
        ]
    handleEdges =
      concatMap handleInputEdges (Map.elems handleIndex)
    implementationEdges =
      concatMap
        (implementationDependencyEdges handleIndex)
        (catalogDeclarations currentCatalog)
    invocationEdges =
      [ DemandEdge
          { demandEdgeDependent = HandleNode currentTarget
          , demandEdgePrerequisite = ImplementationNode currentId
          , demandEdgeKind = InvokeImplementation
          , demandEdgeSourcePath =
              firstOccurrence currentId currentImplementationOccurrences
          }
      | (currentId, currentImplementation) <-
          Map.toAscList (catalogById currentCatalog)
      , let currentTarget =
              implementationDeclTargetId currentImplementation
      , Map.member currentTarget handleIndex
      ]
    implementationOccurrences =
      Map.fromList
        [ (ImplementationNode currentId, currentPaths)
        | (currentId, currentPaths) <-
            Map.toAscList currentImplementationOccurrences
        ]

handleInputEdges :: HandleDecl -> [DemandEdge]
handleInputEdges currentHandle =
  case handleDeclInput currentHandle of
    Nothing ->
      []
    Just currentInput ->
      [ DemandEdge
          { demandEdgeDependent =
              HandleNode (handleDeclId currentHandle)
          , demandEdgePrerequisite =
              HandleNode currentInput
          , demandEdgeKind = InputDependency
          , demandEdgeSourcePath = Nothing
          }
      ]

implementationDependencyEdges ::
  Map.Map HandleId HandleDecl ->
  ImplementationDecl ->
  [DemandEdge]
implementationDependencyEdges handleIndex currentImplementation =
  inputEdges ++ argumentEdges
  where
    currentTarget =
      implementationDeclTargetId currentImplementation
    currentImplementationNode =
      ImplementationNode (implementationIdFor currentImplementation)
    inputEdges =
      case Map.lookup currentTarget handleIndex >>= handleDeclInput of
        Nothing ->
          []
        Just currentInput ->
          [ DemandEdge
              { demandEdgeDependent = currentImplementationNode
              , demandEdgePrerequisite =
                  HandleNode currentInput
              , demandEdgeKind = InputDependency
              , demandEdgeSourcePath = Nothing
              }
          ]
    argumentEdges =
      [ DemandEdge
          { demandEdgeDependent = currentImplementationNode
          , demandEdgePrerequisite = HandleNode currentRead
          , demandEdgeKind =
              ArgumentUse (FieldPath ["reference:" ++ show currentIndex])
          , demandEdgeSourcePath = Nothing
          }
      | (currentIndex, currentRead) <-
          indexedItems
            (implementationDeclRReferences currentImplementation)
      ]

rootDemandFromSeed :: RootSeed -> RootDemand
rootDemandFromSeed currentSeed =
  RootDemand
    { rootDemandKind = rootSeedKind currentSeed
    , rootDemandPath = rootSeedPath currentSeed
    , rootDemandNode = rootSeedNode currentSeed
    }

rootOccurrences ::
  [RootSeed] ->
  Map.Map DemandNodeId [AstPath]
rootOccurrences currentRoots =
  Map.fromListWith
    (++)
    [ (rootSeedNode currentRoot, [rootSeedPath currentRoot])
    | currentRoot <- currentRoots
    ]

normalizeImplicitUnitImplementations ::
  Map.Map HandleId HandleDecl ->
  DemandGraph ->
  [ImplementationDecl]
normalizeImplicitUnitImplementations handleIndex currentGraph =
  [ implicitUnitImplementation currentHandle
  | currentId <- Set.toAscList demandedHandleIds
  , Just currentHandle <- [Map.lookup currentId handleIndex]
  , handleDeclKind currentHandle /= R
  , isUnitHandle currentHandle
  ]
  where
    demandedHandleIds =
      Set.fromList
        (concatMap demandedHandleId (demandGraphClosure currentGraph))
    demandedHandleId currentNode =
      case currentNode of
        HandleNode currentId ->
          [currentId]
        ImplementationNode currentImplementation ->
          [implementationIdTarget currentImplementation]

implicitUnitImplementation :: HandleDecl -> ImplementationDecl
implicitUnitImplementation currentHandle =
  ImplementationDecl
    { implementationDeclaredKind = handleDeclKind currentHandle
    , implementationTarget = handleDeclId currentHandle
    , implementationArguments =
        RLiteralDecl
          (handleDeclArgumentSchema currentHandle)
          LiteralUnit
    }

implicitImplementationPath :: ImplementationDecl -> AstPath
implicitImplementationPath currentImplementation =
  AstPath
    [ "lowering"
    , "implicit-unit"
    , renderImplementationId (implementationIdFor currentImplementation)
    ]

validateLocatedImplementation ::
  Map.Map HandleId HandleDecl ->
  Map.Map EffectSystemName EffectSystemDecl ->
  AstPath ->
  ImplementationDecl ->
  [ValidationError]
validateLocatedImplementation
  handleIndex systemIndex currentPath currentImplementation =
    validateImplementationContract
      handleIndex
      systemIndex
      currentPath
      currentImplementation

validateRootSeeds ::
  Map.Map HandleId HandleDecl ->
  ImplementationCatalog ->
  [RootSeed] ->
  [ValidationError]
validateRootSeeds handleIndex currentCatalog =
  concatMap validateOne
  where
    validateOne currentRoot =
      rootKindErrors
        ++ validateDemandBindings
          handleIndex
          currentCatalog
          (rootSeedBindings currentRoot)
          (rootSeedPath currentRoot)
          (rootSeedNode currentRoot)
      where
        rootKindErrors =
          case rootSeedNode currentRoot of
            HandleNode currentHandleId ->
              case Map.lookup currentHandleId handleIndex of
                Just currentHandle
                  | handleDeclKind currentHandle == R ->
                      [ InvalidRootKind
                          (rootSeedPath currentRoot)
                          currentHandleId
                          R
                      ]
                  | not (isUnitHandle currentHandle) ->
                      [ ParameterizedHandleLeaf
                          (rootSeedPath currentRoot)
                          currentHandleId
                          (handleDeclArgumentSchema currentHandle)
                      ]
                _ ->
                  []
            ImplementationNode _ ->
              []

validateDemandBindings ::
  Map.Map HandleId HandleDecl ->
  ImplementationCatalog ->
  BindingEnvironment ->
  AstPath ->
  DemandNodeId ->
  [ValidationError]
validateDemandBindings
  handleIndex currentCatalog currentBindings currentPath =
    go Set.empty
  where
    go visited currentNode
      | currentNode `Set.member` visited =
          []
      | otherwise =
          case currentNode of
            HandleNode currentHandleId ->
              case Map.lookup currentHandleId handleIndex of
                Nothing ->
                  [ UnknownHandleReference
                      (AstHandleSite currentPath)
                      currentHandleId
                  ]
                Just currentHandle ->
                  validateHandleNode
                    (Set.insert currentNode visited)
                    currentHandle
            ImplementationNode currentImplementationId ->
              case Map.lookup currentImplementationId (catalogById currentCatalog) of
                Nothing ->
                  [DemandGraphUnknownNode currentNode]
                Just currentImplementation ->
                  concatMap
                    (go (Set.insert currentNode visited))
                    ( implementationPrerequisites
                        handleIndex
                        currentImplementation
                    )
    validateHandleNode visited currentHandle
      | handleDeclKind currentHandle == R =
          walkHandleInput visited currentHandle
      | isUnitHandle currentHandle =
          walkHandleInput visited currentHandle
      | otherwise =
          bindingErrors ++ bindingDependencies ++ walkHandleInput visited currentHandle
      where
        currentId =
          handleDeclId currentHandle
        bindingErrors =
          case Map.lookup currentId currentBindings of
            Just _ ->
              []
            Nothing
              | Map.member
                  (ImplementationId currentId)
                  (catalogById currentCatalog) ->
                  [ImplementationNotInScope currentPath currentId]
              | otherwise ->
                  [ MissingImplementation
                      currentPath
                      currentId
                      (handleDeclArgumentSchema currentHandle)
                  ]
        bindingDependencies =
          case Map.lookup currentId currentBindings of
            Nothing ->
              []
            Just currentImplementation ->
              go
                visited
                (ImplementationNode (implementationIdFor currentImplementation))
    walkHandleInput visited currentHandle =
      case handleDeclInput currentHandle of
        Nothing ->
          []
        Just currentInput ->
          go visited (HandleNode currentInput)

implementationPrerequisites ::
  Map.Map HandleId HandleDecl ->
  ImplementationDecl ->
  [DemandNodeId]
implementationPrerequisites handleIndex currentImplementation =
  targetInput
    ++ map
      HandleNode
      (implementationDeclRReferences currentImplementation)
  where
    targetInput =
      case
        Map.lookup
          (implementationDeclTargetId currentImplementation)
          handleIndex
          >>= handleDeclInput of
        Nothing ->
          []
        Just currentInput ->
          [HandleNode currentInput]

isUnitHandle :: HandleDecl -> Bool
isUnitHandle currentHandle =
  handleDeclArgumentSchema currentHandle
    == schemaRefIdentityOf unitSchema

canonicalEffectSystemDecl :: EffectSystemDecl -> EffectSystemDecl
canonicalEffectSystemDecl currentSystem =
  currentSystem
    { effectSystemDeclImports =
        sort (effectSystemDeclImports currentSystem)
    , effectSystemDeclHandles =
        sort (effectSystemDeclHandles currentSystem)
    , effectSystemDeclPrivate =
        sort (effectSystemDeclPrivate currentSystem)
    , effectSystemDeclExports =
        sort (effectSystemDeclExports currentSystem)
    }

firstOccurrence ::
  Ord key =>
  key ->
  Map.Map key [value] ->
  Maybe value
firstOccurrence currentKey currentOccurrences =
  case Map.lookup currentKey currentOccurrences of
    Just (currentValue : _) -> Just currentValue
    _ -> Nothing

firstBy :: Ord key => (value -> key) -> [value] -> Map.Map key value
firstBy keyOf =
  foldl insertOne Map.empty
  where
    insertOne currentMap currentValue =
      Map.insertWith
        (\_ previousValue -> previousValue)
        (keyOf currentValue)
        currentValue
        currentMap


indexedItems :: [item] -> [(Int, item)]
indexedItems =
  zip [0 ..]
