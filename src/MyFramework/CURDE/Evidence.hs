module MyFramework.CURDE.Evidence
  ( CURDEClaimEvidence (..)
  , CURDEDemandObservation (..)
  , CURDEEvidenceBoundary (..)
  , CURDEEvidenceStatus (..)
  , CURDESemanticsReport (..)
  , curdeSemanticsClaimCatalog
  , curdeSemanticsEvidenceSchemaV1
  , curdeSemanticsReport
  , CURDESemanticsSummary (..)
  , curdeSemanticsEvidenceSchemaString
  , summarizeCURDESemanticsReport
  , curdeSemanticsReportPassed
  , renderCURDESemanticsReportJSON
  ) where

import Data.Char
  ( ord )
import Data.List
  ( intercalate
  , sort
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric
  ( showHex )
import Text.Read
  ( readMaybe )

import MyFramework.Ast
  ( AstBlueprintSeed (..)
  , AstPath (..)
  , AstSeed (..)
  )
import MyFramework.Ast.Layout
  ( AstBlueprintLayout (..)
  , AstLayoutEdge (..)
  , AstLayoutModel (..)
  , AstLayoutNode (..)
  , AstNodeKind (..)
  , AstSummary (..)
  , layoutAstBlueprint
  , summarizeAstBlueprint
  )
import MyFramework.Control
  ( ControlNode (..)
  , ControlPlan (..)
  , ControlTree (..)
  , ControlValidationError
  , compileControlPlan
  )
import MyFramework.CURDE.Core
  ( CURDECore (..)
  , DemandEdge (..)
  , DemandEdgeKind (..)
  , DemandGraph (..)
  , DemandNodeId (..)
  , RootDemand (..)
  , demandClosure
  , implementationIdFor
  )
import MyFramework.CURDE.Expression
  ( ImplementationDecl
  , implementationDeclRReferences
  )
import MyFramework.CURDE.Lowering
  ( LoweringResult (..)
  , lowerCURDEDecl
  , loweringPassed
  )
import MyFramework.CURDE.Types
  ( CURDE (..)
  , EffectSystemDecl (..)
  , HandleDecl (..)
  , ReadSource (..)
  )
import MyFramework.CURDE.Validate
  ( ValidationError (..)
  , sortValidationErrors
  , validateDemandGraph
  , validateEffectSystems
  , validateImplementationCatalog
  , validateReadConsumption
  , validationPassed
  )
import MyFramework.TrustBase.Evidence
  ( ClaimCatalog (..)
  )
import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  , renderSchemaId
  )

data CURDEEvidenceBoundary
  = GenericRecordCompileTimeWitness
  | PublicFacadeCompileTimeWitness
  | RuntimeObservationWitness
  | RuntimeControlWitness
  deriving (Eq, Ord, Show)

data CURDEEvidenceStatus
  = EvidenceEstablished
  | EvidenceViolated
  | EvidenceBlocked [String]
  | EvidenceDeferredTo CURDEEvidenceBoundary
  deriving (Eq, Ord, Show)

data CURDEClaimEvidence = CURDEClaimEvidence
  { curdeClaimEvidenceName :: ClaimName
  , curdeClaimEvidenceStatus :: CURDEEvidenceStatus
  , curdeClaimEvidenceExpected :: String
  , curdeClaimEvidenceObserved :: String
  }
  deriving (Eq, Show)

data CURDEDemandObservation = CURDEDemandObservation
  { curdeObservationLoweringErrors :: [ValidationError]
  , curdeObservationDirectValidationErrors :: [ValidationError]
  , curdeObservationControlErrors :: [ControlValidationError]
  , curdeObservationLayoutPaths :: [AstPath]
  , curdeObservationControlPaths :: [AstPath]
  , curdeObservationDemandRoots :: [RootDemand]
  , curdeObservationDemandOccurrences :: [(AstPath, DemandNodeId)]
  , curdeObservationDemandEdges :: [DemandEdge]
  , curdeObservationDemandClosure :: [DemandNodeId]
  }
  deriving (Eq, Show)

data CURDESemanticsReport = CURDESemanticsReport
  { curdeSemanticsReportSchema :: SchemaId
  , curdeSemanticsReportCatalog :: ClaimCatalog
  , curdeSemanticsReportObservation :: CURDEDemandObservation
  , curdeSemanticsReportEvidence :: [CURDEClaimEvidence]
  }
  deriving (Eq, Show)

curdeSemanticsEvidenceSchemaV1 :: SchemaId
curdeSemanticsEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "curde-semantics-evidence"
    , schemaIdVersion = SchemaVersion 1
    }

curdeSemanticsClaimCatalog :: ClaimCatalog
curdeSemanticsClaimCatalog =
  ClaimCatalog
    { claimCatalogName = "curde-semantics"
    , claimCatalogCoreClaims =
        map
          ClaimName
          [ "curde-declaration-round-trip"
          , "curde-canonical-lowering"
          , "curde-cumulative-validation"
          , "curde-single-input"
          , "curde-input-cycle-free"
          , "curde-implementation-kind"
          , "curde-implementation-schema"
          , "curde-implementation-complete"
          , "curde-read-consumption"
          , "curde-observation-read-implementation-dag"
          , "curde-leaf-only-demand-roots"
          , "curde-cata-layout-paths"
          , "curde-control-occurrence-alignment"
          , "curde-control-root-alignment"
          , "curde-demand-closure-three-edge-kinds"
          , "curde-closed-frontend-no-legacy-control"
          , "curde-generic-record-identity"
          , "curde-public-facade-boundary"
          , "curde-runtime-observation-read-history"
          , "curde-runtime-control-parity"
          ]
    , claimCatalogManifestClaim =
        ClaimName "curde-semantics-claim-manifest"
    }

-- | The only evidence entry point. It accepts the real serializable frontend,
-- performs lowering itself, and derives every static observation from the
-- lowered core. There is no API that accepts caller-authored claim booleans.
curdeSemanticsReport ::
  [EffectSystemDecl] ->
  AstBlueprintSeed ->
  CURDESemanticsReport
curdeSemanticsReport currentSystems currentSeed =
  CURDESemanticsReport
    { curdeSemanticsReportSchema =
        curdeSemanticsEvidenceSchemaV1
    , curdeSemanticsReportCatalog =
        curdeSemanticsClaimCatalog
    , curdeSemanticsReportObservation =
        CURDEDemandObservation
          { curdeObservationLoweringErrors =
              currentLoweringErrors
          , curdeObservationDirectValidationErrors =
              currentDirectValidationErrors
          , curdeObservationControlErrors =
              currentControlErrors
          , curdeObservationLayoutPaths =
              currentLayoutPaths
          , curdeObservationControlPaths =
              currentControlPaths
          , curdeObservationDemandRoots =
              demandGraphRoots currentGraph
          , curdeObservationDemandOccurrences =
              currentGraphOccurrences
          , curdeObservationDemandEdges =
              demandGraphEdges currentGraph
          , curdeObservationDemandClosure =
              demandGraphClosure currentGraph
          }
    , curdeSemanticsReportEvidence =
        completeClaimEvidence currentCoreEvidence
    }
  where
    currentLowering =
      lowerCURDEDecl currentSystems currentSeed
    currentCore =
      loweringCore currentLowering
    currentGraph =
      curdeCoreDemandGraph currentCore
    currentLoweringErrors =
      loweringErrors currentLowering
    currentDirectValidationErrors =
      sortValidationErrors
        ( validateEffectSystems currentSystems
            ++ validateImplementationCatalog
              (curdeCoreImplementations currentCore)
            ++ validateDemandGraph currentGraph
            ++ validateReadConsumption currentSystems currentGraph
        )
    currentLayout =
      layoutAstBlueprint (curdeCoreAst currentCore)
    currentLayoutModels =
      blueprintLayoutModels currentLayout
    currentLayoutNodes =
      concatMap astLayoutNodes currentLayoutModels
    currentLayoutPaths =
      map astLayoutNodePath currentLayoutNodes
    currentLayoutSummary =
      summarizeAstBlueprint (curdeCoreAst currentCore)
    currentControl =
      compileControlPlan currentCore
    currentControlErrors =
      case currentControl of
        Left currentErrors ->
          currentErrors
        Right _ ->
          []
    currentControlTrees =
      case currentControl of
        Left _ ->
          []
        Right currentPlan ->
          controlPlanTrees currentPlan
    currentControlPaths =
      concatMap controlTreePaths currentControlTrees
    currentControlOccurrences =
      uniqueSorted
        (concatMap controlTreeOccurrences currentControlTrees)
    currentControlRoots =
      uniqueSorted
        (concatMap controlTreeDemandRoots currentControlTrees)
    currentGraphOccurrences =
      uniqueSorted
        [ (currentPath, currentNode)
        | (currentNode, currentPaths) <-
            Map.toAscList (demandGraphOccurrences currentGraph)
        , currentPath <- currentPaths
        ]
    currentGraphRoots =
      uniqueSorted
        [ (rootDemandPath currentRoot, rootDemandNode currentRoot)
        | currentRoot <- demandGraphRoots currentGraph
        ]
    currentLeafPaths =
      sort
        [ astLayoutNodePath currentNode
        | currentNode <- currentLayoutNodes
        , astLayoutNodeKind currentNode == AstLeaf
        ]
    currentRootPaths =
      sort (map rootDemandPath (demandGraphRoots currentGraph))
    currentCanonicalLowering =
      lowerCURDEDecl
        (canonicalEffectSystems currentSystems)
        currentSeed
    currentCanonicalCore =
      loweringCore currentCanonicalLowering
    currentCanonicalPassed =
      canonicalProjection currentCore
        == canonicalProjection currentCanonicalCore
        && currentLoweringErrors
          == loweringErrors currentCanonicalLowering
    currentValidationIncluded =
      all
        (`elem` currentLoweringErrors)
        currentDirectValidationErrors
        && currentLoweringErrors
          == sortValidationErrors currentLoweringErrors
        && loweringPassed currentLowering
          == validationPassed currentLoweringErrors
    currentHandles =
      concatMap effectSystemDeclHandles currentSystems
    currentInputWidth =
      maximumOrZero
        [ inputWidth currentHandle
        | currentHandle <- currentHandles
        ]
    currentInputCycleErrors =
      filter isInputCycleError currentLoweringErrors
    currentImplementationKindErrors =
      filter isImplementationKindError currentLoweringErrors
    currentImplementationSchemaErrors =
      filter isImplementationSchemaError currentLoweringErrors
    currentImplementationCompletenessErrors =
      filter isImplementationCompletenessError currentLoweringErrors
    currentReadConsumptionErrors =
      filter isReadConsumptionError currentLoweringErrors
    currentExpectedArgumentUses =
      expectedArgumentUses (curdeCoreImplementations currentCore)
    currentActualArgumentUses =
      actualArgumentUses currentGraph
    currentObservationErrors =
      filter isObservationDagError currentLoweringErrors
    currentExpectedObservationEdges =
      expectedObservationInputEdges currentHandles
    currentActualInputEdges =
      actualInputEdges currentGraph
    currentLayoutPathSet =
      Set.fromList currentLayoutPaths
    currentLayoutEdges =
      concatMap astLayoutEdges currentLayoutModels
    currentLayoutRoots =
      map astLayoutRootPath currentLayoutModels
    currentExpectedLayoutRoots =
      AstPath ["blueprint", "boot"]
        : [ AstPath
              [ "blueprint"
              , "hanging"
              , "item:" ++ show currentIndex
              ]
          | currentIndex <-
              [0 .. length (astBlueprintSeedHanging currentSeed) - 1]
          ]
    currentLayoutPassed =
      length currentLayoutPaths
        == Set.size currentLayoutPathSet
        && all
          (layoutEdgeClosed currentLayoutPathSet)
          currentLayoutEdges
        && currentLayoutRoots == currentExpectedLayoutRoots
        && astSummaryNodeCount currentLayoutSummary
          == length currentLayoutNodes
    currentControlOccurrencePassed =
      case currentControl of
        Left _ ->
          False
        Right _ ->
          uniqueSorted currentControlPaths
            == uniqueSorted currentLayoutPaths
            && currentControlOccurrences
              == currentGraphOccurrences
    currentControlRootPassed =
      case currentControl of
        Left _ ->
          False
        Right _ ->
          currentControlRoots == currentGraphRoots
    currentDemandClosurePassed =
      all demandEdgeShapeAllowed (demandGraphEdges currentGraph)
        && demandGraphClosure currentGraph == demandClosure currentGraph
        && all
          (`Map.member` demandGraphNodes currentGraph)
          (demandGraphClosure currentGraph)
        && all
          ((`elem` demandGraphClosure currentGraph) . rootDemandNode)
          (demandGraphRoots currentGraph)
        && null (validateDemandGraph currentGraph)
    currentReferenceBlockers =
      map show (filter isReferenceBlocker currentLoweringErrors)
    currentRootBlockers =
      map show (filter isRootSemanticError currentLoweringErrors)
    currentControlBlockers =
      map show currentControlErrors
    currentGraphBlockers =
      map show (validateDemandGraph currentGraph)
    currentBlockersForClaim =
      [ (ClaimName "curde-input-cycle-free", currentReferenceBlockers)
      , (ClaimName "curde-implementation-kind", currentReferenceBlockers)
      , (ClaimName "curde-implementation-schema", currentReferenceBlockers)
      , (ClaimName "curde-implementation-complete", currentReferenceBlockers)
      , (ClaimName "curde-read-consumption", currentReferenceBlockers)
      , (ClaimName "curde-observation-read-implementation-dag", currentReferenceBlockers)
      , (ClaimName "curde-leaf-only-demand-roots", currentRootBlockers)
      , (ClaimName "curde-control-occurrence-alignment", currentControlBlockers)
      , (ClaimName "curde-control-root-alignment", currentControlBlockers)
      , (ClaimName "curde-demand-closure-three-edge-kinds", currentGraphBlockers)
      ]
    applyCurrentBlockers currentEvidence =
      case lookup (curdeClaimEvidenceName currentEvidence) currentBlockersForClaim of
        Just currentBlockers
          | not (null currentBlockers)
              && curdeClaimEvidenceStatus currentEvidence
                == EvidenceEstablished ->
              currentEvidence
                { curdeClaimEvidenceStatus = EvidenceBlocked currentBlockers
                , curdeClaimEvidenceObserved =
                    curdeClaimEvidenceObserved currentEvidence
                      ++ "; blockers="
                      ++ show currentBlockers
                }
        _ ->
          currentEvidence
    currentCoreEvidence =
      map applyCurrentBlockers
        [ establishedClaim
          "curde-declaration-round-trip"
          (roundTrip currentSystems && roundTrip currentSeed)
          "EffectSystemDecl and AstBlueprintSeed round-trip through their closed codec"
          ( "systems="
              ++ show (length currentSystems)
              ++ "; ast-seed-nodes="
              ++ show (astBlueprintSeedNodeCount currentSeed)
          )
      , establishedClaim
          "curde-canonical-lowering"
          currentCanonicalPassed
          "lowering is invariant under canonical EffectSystem declaration ordering"
          ( "lowering-errors="
              ++ show (length currentLoweringErrors)
              ++ "; canonical-errors="
              ++ show
                (length (loweringErrors currentCanonicalLowering))
          )
      , establishedClaim
          "curde-cumulative-validation"
          currentValidationIncluded
          "lowering includes every normalized error returned by the public aggregate validators and derives pass/fail from the complete lowering error set"
          ( "lowering-errors="
              ++ show currentLoweringErrors
              ++ "; direct-errors="
              ++ show currentDirectValidationErrors
          )
      , establishedClaim
          "curde-single-input"
          (currentInputWidth <= 1)
          "every HandleDecl has at most one explicit input"
          ("maximum-input-width=" ++ show currentInputWidth)
      , establishedClaim
          "curde-input-cycle-free"
          (null currentInputCycleErrors)
          "the explicit input relation is acyclic"
          (show currentInputCycleErrors)
      , establishedClaim
          "curde-implementation-kind"
          (null currentImplementationKindErrors)
          "every Implementation targets a non-R handle of the declared CURDE kind"
          (show currentImplementationKindErrors)
      , establishedClaim
          "curde-implementation-schema"
          (null currentImplementationSchemaErrors)
          "Implementation argument and referenced R schemas match registered contracts"
          (show currentImplementationSchemaErrors)
      , establishedClaim
          "curde-implementation-complete"
          (null currentImplementationCompletenessErrors)
          "every parameterized demanded CUDE has one non-conflicting lexical Implementation"
          (show currentImplementationCompletenessErrors)
      , establishedClaim
          "curde-read-consumption"
          ( null currentReadConsumptionErrors
              && currentExpectedArgumentUses == currentActualArgumentUses
          )
          "registered R handles are consumed and every Implementation R reference is an ArgumentUse edge"
          ( "errors="
              ++ show currentReadConsumptionErrors
              ++ "; expected="
              ++ show currentExpectedArgumentUses
              ++ "; actual="
              ++ show currentActualArgumentUses
          )
      , establishedClaim
          "curde-observation-read-implementation-dag"
          ( null currentObservationErrors
              && all
                (`elem` currentActualInputEdges)
                currentExpectedObservationEdges
              && currentExpectedArgumentUses == currentActualArgumentUses
          )
          "observation R inputs and R-to-Implementation value use are explicit graph edges"
          ( "errors="
              ++ show currentObservationErrors
              ++ "; observation-edges="
              ++ show currentExpectedObservationEdges
          )
      , establishedClaim
          "curde-leaf-only-demand-roots"
          (currentLeafPaths == currentRootPaths)
          "AST Leaf paths are exactly the initial Demand root paths"
          ( "leaf-paths="
              ++ show currentLeafPaths
              ++ "; root-paths="
              ++ show currentRootPaths
          )
      , establishedClaim
          "curde-cata-layout-paths"
          currentLayoutPassed
          "the cata layout has unique closed paths and canonical blueprint roots"
          ( "paths="
              ++ show currentLayoutPaths
              ++ "; edges="
              ++ show currentLayoutEdges
          )
      , establishedClaim
          "curde-control-occurrence-alignment"
          currentControlOccurrencePassed
          "ControlTree paths and occurrence identities align with cata layout and Demand occurrences"
          ( "control-errors="
              ++ show currentControlErrors
              ++ "; control-occurrences="
              ++ show currentControlOccurrences
              ++ "; graph-occurrences="
              ++ show currentGraphOccurrences
          )
      , establishedClaim
          "curde-control-root-alignment"
          currentControlRootPassed
          "ControlDemand leaves align with DemandGraph root path/node pairs"
          ( "control-roots="
              ++ show currentControlRoots
              ++ "; graph-roots="
              ++ show currentGraphRoots
          )
      , establishedClaim
          "curde-demand-closure-three-edge-kinds"
          currentDemandClosurePassed
          "Demand closure is recomputed from InvokeImplementation, InputDependency, and ArgumentUse only"
          ( "edges="
              ++ show (demandGraphEdges currentGraph)
              ++ "; closure="
              ++ show (demandGraphClosure currentGraph)
          )
      , establishedClaim
          "curde-closed-frontend-no-legacy-control"
          (closedAstBlueprintSeed currentSeed)
          "the serializable frontend is EffectSystemDecl plus the closed AstBlueprintSeed vocabulary, with no pipeline, Boot.targets, or retry policy field"
          ( "ast-seed-nodes="
              ++ show (astBlueprintSeedNodeCount currentSeed)
              ++ "; frontend-surfaces=[EffectSystemDecl,AstBlueprintSeed]"
          )
      , deferredClaim
          "curde-generic-record-identity"
          GenericRecordCompileTimeWitness
          "Generic record selector/Symbol identity and typed reuse require a compile-time witness"
      , deferredClaim
          "curde-public-facade-boundary"
          PublicFacadeCompileTimeWitness
          "module export inspection must prove DemandGraph and runtime internals remain absent from the public CURDE facade"
      , deferredClaim
          "curde-runtime-observation-read-history"
          RuntimeObservationWitness
          "runtime evidence must prove CUDE observation to R to Implementation flow and that R failure preserves CUDE success history"
      , deferredClaim
          "curde-runtime-control-parity"
          RuntimeControlWitness
          "runtime evidence must prove control-node behavior, cancellation settlement, shared identity/single-flight, and no automatic retry"
      ]

completeClaimEvidence ::
  [CURDEClaimEvidence] ->
  [CURDEClaimEvidence]
completeClaimEvidence currentEvidence =
  currentEvidence
    ++
    [ establishedClaim
        "curde-semantics-claim-manifest"
        manifestMatches
        "the evidence payload contains each catalog claim exactly once and in catalog order"
        ( "expected="
            ++ show expectedClaims
            ++ "; observed="
            ++ show observedClaims
        )
    ]
  where
    expectedClaims =
      claimCatalogCoreClaims curdeSemanticsClaimCatalog
    observedClaims =
      map curdeClaimEvidenceName currentEvidence
    manifestMatches =
      observedClaims == expectedClaims
        && length observedClaims
          == Set.size (Set.fromList observedClaims)

establishedClaim ::
  String ->
  Bool ->
  String ->
  String ->
  CURDEClaimEvidence
establishedClaim currentName currentPassed currentExpected currentObserved =
  CURDEClaimEvidence
    { curdeClaimEvidenceName = ClaimName currentName
    , curdeClaimEvidenceStatus =
        if currentPassed
          then EvidenceEstablished
          else EvidenceViolated
    , curdeClaimEvidenceExpected = currentExpected
    , curdeClaimEvidenceObserved = currentObserved
    }

deferredClaim ::
  String ->
  CURDEEvidenceBoundary ->
  String ->
  CURDEClaimEvidence
deferredClaim currentName currentBoundary currentExpected =
  CURDEClaimEvidence
    { curdeClaimEvidenceName = ClaimName currentName
    , curdeClaimEvidenceStatus =
        EvidenceDeferredTo currentBoundary
    , curdeClaimEvidenceExpected = currentExpected
    , curdeClaimEvidenceObserved =
        "deferred to " ++ show currentBoundary
    }
canonicalEffectSystems :: [EffectSystemDecl] -> [EffectSystemDecl]
canonicalEffectSystems =
  sort . map canonicalEffectSystem
  where
    canonicalEffectSystem currentSystem =
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

canonicalProjection ::
  CURDECore ->
  ([EffectSystemDecl], [ImplementationDecl], DemandGraph)
canonicalProjection currentCore =
  ( curdeCoreEffectSystems currentCore
  , curdeCoreImplementations currentCore
  , curdeCoreDemandGraph currentCore
  )

roundTrip :: (Eq value, Read value, Show value) => value -> Bool
roundTrip currentValue =
  readMaybe (show currentValue) == Just currentValue

blueprintLayoutModels :: AstBlueprintLayout -> [AstLayoutModel]
blueprintLayoutModels currentLayout =
  astBlueprintBootLayout currentLayout
    : astBlueprintHangingLayouts currentLayout

controlPlanTrees :: ControlPlan -> [ControlTree]
controlPlanTrees currentPlan =
  controlPlanBoot currentPlan : controlPlanHanging currentPlan

controlTreePaths :: ControlTree -> [AstPath]
controlTreePaths currentTree =
  controlTreePath currentTree
    : concatMap controlTreePaths (controlChildren (controlTreeNode currentTree))

controlTreeOccurrences :: ControlTree -> [(AstPath, DemandNodeId)]
controlTreeOccurrences currentTree =
  currentOccurrence
    ++ concatMap
      controlTreeOccurrences
      (controlChildren currentNode)
  where
    currentPath =
      controlTreePath currentTree
    currentNode =
      controlTreeNode currentTree
    currentOccurrence =
      case currentNode of
        ControlDemand currentDemand ->
          [(currentPath, currentDemand)]
        ControlWithImplementation currentImplementation _ ->
          [(currentPath, ImplementationNode currentImplementation)]
        _ ->
          []

controlTreeDemandRoots :: ControlTree -> [(AstPath, DemandNodeId)]
controlTreeDemandRoots currentTree =
  currentRoot
    ++ concatMap
      controlTreeDemandRoots
      (controlChildren currentNode)
  where
    currentPath =
      controlTreePath currentTree
    currentNode =
      controlTreeNode currentTree
    currentRoot =
      case currentNode of
        ControlDemand currentDemand ->
          [(currentPath, currentDemand)]
        _ ->
          []

controlChildren :: ControlNode -> [ControlTree]
controlChildren currentNode =
  case currentNode of
    ControlDemand _ -> []
    ControlWithImplementation _ currentChild -> [currentChild]
    ControlSequence currentChildren -> currentChildren
    ControlParallel currentChildren -> currentChildren
    ControlFallback currentChildren -> currentChildren
    ControlRace currentChildren -> currentChildren
    ControlChoice _ currentBranches -> map snd currentBranches
    ControlWait _ currentChild -> [currentChild]
    ControlLoop currentChild -> [currentChild]
    ControlMiddleware _ currentChild -> [currentChild]
    ControlCallback _ currentChild -> [currentChild]
    ControlSuspense _ -> []
    ControlContext _ currentChild -> [currentChild]

expectedArgumentUses ::
  [ImplementationDecl] ->
  [(DemandNodeId, DemandNodeId)]
expectedArgumentUses currentImplementations =
  uniqueSorted
    [ ( ImplementationNode (implementationIdFor currentImplementation)
      , HandleNode currentRead
      )
    | currentImplementation <- currentImplementations
    , currentRead <- implementationDeclRReferences currentImplementation
    ]

actualArgumentUses :: DemandGraph -> [(DemandNodeId, DemandNodeId)]
actualArgumentUses currentGraph =
  uniqueSorted
    [ ( demandEdgeDependent currentEdge
      , demandEdgePrerequisite currentEdge
      )
    | currentEdge <- demandGraphEdges currentGraph
    , ArgumentUse _ <- [demandEdgeKind currentEdge]
    ]

expectedObservationInputEdges ::
  [HandleDecl] ->
  [(DemandNodeId, DemandNodeId)]
expectedObservationInputEdges currentHandles =
  uniqueSorted
    [ (HandleNode (handleDeclId currentHandle), HandleNode currentInput)
    | currentHandle <- currentHandles
    , handleDeclKind currentHandle == R
    , handleDeclReadSource currentHandle == Just ReadFromInputObservation
    , Just currentInput <- [handleDeclInput currentHandle]
    ]

actualInputEdges :: DemandGraph -> [(DemandNodeId, DemandNodeId)]
actualInputEdges currentGraph =
  uniqueSorted
    [ ( demandEdgeDependent currentEdge
      , demandEdgePrerequisite currentEdge
      )
    | currentEdge <- demandGraphEdges currentGraph
    , demandEdgeKind currentEdge == InputDependency
    ]

inputWidth :: HandleDecl -> Int
inputWidth currentHandle =
  case handleDeclInput currentHandle of
    Nothing -> 0
    Just _ -> 1

maximumOrZero :: [Int] -> Int
maximumOrZero [] = 0
maximumOrZero currentValues = maximum currentValues

layoutEdgeClosed :: Set.Set AstPath -> AstLayoutEdge -> Bool
layoutEdgeClosed currentPaths currentEdge =
  astLayoutEdgeFrom currentEdge `Set.member` currentPaths
    && astLayoutEdgeTo currentEdge `Set.member` currentPaths

demandEdgeShapeAllowed :: DemandEdge -> Bool
demandEdgeShapeAllowed currentEdge =
  case demandEdgeKind currentEdge of
    InvokeImplementation -> True
    InputDependency -> True
    ArgumentUse _ -> True

isReferenceBlocker :: ValidationError -> Bool
isReferenceBlocker currentError =
  case currentError of
    DuplicateEffectSystem _ -> True
    UnknownEffectSystemImport _ _ -> True
    DuplicateHandle _ -> True
    HandleDeclaredInWrongSystem _ _ -> True
    PrivateHandleOutsideSystem _ _ -> True
    ExportedHandleOutsideSystem _ _ -> True
    PrivateExportHandleOverlap _ _ -> True
    UnknownHandleReference _ _ -> True
    HandleInputSystemNotImported _ _ _ -> True
    HandleInputNotExported _ _ -> True
    UnknownAstHandleReference _ _ -> True
    InvalidImplementationTarget _ _ _ -> True
    ImplementationNotInScope _ _ -> True
    DemandGraphUnknownNode _ -> True
    _ -> False

isRootSemanticError :: ValidationError -> Bool
isRootSemanticError currentError =
  case currentError of
    UnknownAstHandleReference _ _ -> True
    InvalidRootKind _ _ _ -> True
    ParameterizedHandleLeaf _ _ _ -> True
    MissingImplementation _ _ _ -> True
    ImplementationNotInScope _ _ -> True
    _ -> False

isInputCycleError :: ValidationError -> Bool
isInputCycleError currentError =
  case currentError of
    InputCycle _ -> True
    _ -> False

isImplementationKindError :: ValidationError -> Bool
isImplementationKindError currentError =
  case currentError of
    InvalidImplementationTarget _ _ _ -> True
    ImplementationKindMismatch _ _ _ _ -> True
    _ -> False

isImplementationSchemaError :: ValidationError -> Bool
isImplementationSchemaError currentError =
  case currentError of
    ImplementationSchemaMismatch _ _ _ _ -> True
    ExpressionReferenceSchemaMismatch _ _ _ _ _ -> True
    PublicStatusUsedAsValue _ _ _ _ -> True
    EmptyOperatorReference _ _ -> True
    DuplicateExpressionField _ _ _ -> True
    _ -> False

isImplementationCompletenessError :: ValidationError -> Bool
isImplementationCompletenessError currentError =
  case currentError of
    ConflictingImplementation _ -> True
    ParameterizedHandleLeaf _ _ _ -> True
    MissingImplementation _ _ _ -> True
    ImplementationNotInScope _ _ -> True
    _ -> False

isReadConsumptionError :: ValidationError -> Bool
isReadConsumptionError currentError =
  case currentError of
    UnconsumedRead _ -> True
    _ -> False

isObservationDagError :: ValidationError -> Bool
isObservationDagError currentError =
  case currentError of
    ObservationInputMissing _ -> True
    ObservationInputKindMismatch _ _ _ -> True
    ObservationNotCaptured _ _ -> True
    ObservationSchemaMismatch _ _ _ _ -> True
    PrivateObservationCrossSystem _ _ -> True
    _ -> False

astBlueprintSeedNodeCount :: AstBlueprintSeed -> Int
astBlueprintSeedNodeCount currentSeed =
  astSeedNodeCount (astBlueprintSeedBoot currentSeed)
    + sum (map astSeedNodeCount (astBlueprintSeedHanging currentSeed))

astSeedNodeCount :: AstSeed -> Int
astSeedNodeCount currentSeed =
  1 + sum (map astSeedNodeCount (astSeedChildren currentSeed))

closedAstBlueprintSeed :: AstBlueprintSeed -> Bool
closedAstBlueprintSeed currentSeed =
  all closedAstSeed
    (astBlueprintSeedBoot currentSeed : astBlueprintSeedHanging currentSeed)

closedAstSeed :: AstSeed -> Bool
closedAstSeed currentSeed =
  all closedAstSeed (astSeedChildren currentSeed)

astSeedChildren :: AstSeed -> [AstSeed]
astSeedChildren currentSeed =
  case currentSeed of
    SeedLeaf _ -> []
    SeedWithImplementation _ currentChild -> [currentChild]
    SeedChain currentChildren -> currentChildren
    SeedParallel currentChildren -> currentChildren
    SeedFallback currentChildren -> currentChildren
    SeedRace currentChildren -> currentChildren
    SeedChoice _ currentBranches -> map snd currentBranches
    SeedWait _ currentChild -> [currentChild]
    SeedLoop currentChild -> [currentChild]
    SeedMiddleware _ currentChild -> [currentChild]
    SeedCallback _ currentChild -> [currentChild]
    SeedSuspense _ -> []
    SeedContext _ currentChild -> [currentChild]

uniqueSorted :: Ord item => [item] -> [item]
uniqueSorted =
  Set.toAscList . Set.fromList

data CURDESemanticsSummary = CURDESemanticsSummary
  { curdeSummarySchemaExact :: Bool
  , curdeSummaryCatalogExact :: Bool
  , curdeSummaryEvidenceOrderExact :: Bool
  , curdeSummaryManifestExact :: Bool
  , curdeSummaryEstablishedCount :: Int
  , curdeSummaryViolatedCount :: Int
  , curdeSummaryBlockedCount :: Int
  , curdeSummaryDeferredCount :: Int
  , curdeSummaryPassed :: Bool
  }
  deriving (Eq, Show)

data SlotClass
  = SlotEstablished
  | SlotViolated
  | SlotBlocked
  | SlotDeferred
  deriving (Eq)

data ClaimSlot = ClaimSlot
  { slotName :: ClaimName
  , slotCardinality :: Int
  , slotEvidence :: Maybe CURDEClaimEvidence
  , slotClass :: SlotClass
  }

curdeSemanticsEvidenceSchemaString :: String
curdeSemanticsEvidenceSchemaString =
  renderSchemaId curdeSemanticsEvidenceSchemaV1

canonicalClaimNames :: [ClaimName]
canonicalClaimNames =
  claimCatalogCoreClaims curdeSemanticsClaimCatalog
    ++ [claimCatalogManifestClaim curdeSemanticsClaimCatalog]

allowedDeferredNames :: [ClaimName]
allowedDeferredNames =
  map
    ClaimName
    [ "curde-generic-record-identity"
    , "curde-public-facade-boundary"
    , "curde-runtime-observation-read-history"
    , "curde-runtime-control-parity"
    ]

claimSlots :: CURDESemanticsReport -> [ClaimSlot]
claimSlots currentReport =
  map makeSlot canonicalClaimNames
  where
    currentEvidence =
      curdeSemanticsReportEvidence currentReport
    makeSlot currentName =
      case
          filter
            ((== currentName) . curdeClaimEvidenceName)
            currentEvidence of
        [currentItem] ->
          ClaimSlot
            { slotName = currentName
            , slotCardinality = 1
            , slotEvidence = Just currentItem
            , slotClass =
                classifyStatus (curdeClaimEvidenceStatus currentItem)
            }
        currentItems ->
          ClaimSlot
            { slotName = currentName
            , slotCardinality = length currentItems
            , slotEvidence = Nothing
            , slotClass = SlotViolated
            }

classifyStatus :: CURDEEvidenceStatus -> SlotClass
classifyStatus currentStatus =
  case currentStatus of
    EvidenceEstablished -> SlotEstablished
    EvidenceViolated -> SlotViolated
    EvidenceBlocked _ -> SlotBlocked
    EvidenceDeferredTo _ -> SlotDeferred

summarizeCURDESemanticsReport ::
  CURDESemanticsReport ->
  CURDESemanticsSummary
summarizeCURDESemanticsReport currentReport =
  CURDESemanticsSummary
    { curdeSummarySchemaExact = schemaExact
    , curdeSummaryCatalogExact = catalogExact
    , curdeSummaryEvidenceOrderExact = orderExact
    , curdeSummaryManifestExact = manifestExact
    , curdeSummaryEstablishedCount = count SlotEstablished
    , curdeSummaryViolatedCount = count SlotViolated
    , curdeSummaryBlockedCount = count SlotBlocked
    , curdeSummaryDeferredCount = count SlotDeferred
    , curdeSummaryPassed = currentPassed
    }
  where
    currentSlots =
      claimSlots currentReport
    currentEvidence =
      curdeSemanticsReportEvidence currentReport
    currentNames =
      map curdeClaimEvidenceName currentEvidence
    schemaExact =
      curdeSemanticsReportSchema currentReport
        == curdeSemanticsEvidenceSchemaV1
    catalogExact =
      curdeSemanticsReportCatalog currentReport
        == curdeSemanticsClaimCatalog
    orderExact =
      currentNames == canonicalClaimNames
    manifestName =
      claimCatalogManifestClaim curdeSemanticsClaimCatalog
    manifestExact =
      orderExact
        && case reverse currentEvidence of
          currentItem : _ ->
            curdeClaimEvidenceName currentItem == manifestName
              && curdeClaimEvidenceStatus currentItem
                == EvidenceEstablished
          [] -> False
    count currentClass =
      length
        [ ()
        | currentSlot <- currentSlots
        , slotClass currentSlot == currentClass
        ]
    slotAccepted currentSlot
      | slotName currentSlot `elem` allowedDeferredNames =
          slotClass currentSlot
            `elem` [SlotEstablished, SlotDeferred]
      | otherwise =
          slotClass currentSlot == SlotEstablished
    currentPassed =
      schemaExact
        && catalogExact
        && orderExact
        && manifestExact
        && all slotAccepted currentSlots

curdeSemanticsReportPassed :: CURDESemanticsReport -> Bool
curdeSemanticsReportPassed =
  curdeSummaryPassed . summarizeCURDESemanticsReport

renderCURDESemanticsReportJSON :: CURDESemanticsReport -> String
renderCURDESemanticsReportJSON currentReport =
  jsonObject
    [ ( "schema"
      , jsonString (renderSchemaId (curdeSemanticsReportSchema currentReport))
      )
    , ("catalog", renderCatalog (curdeSemanticsReportCatalog currentReport))
    , ("summary", renderSummary (summarizeCURDESemanticsReport currentReport))
    , ( "observation"
      , renderObservation (curdeSemanticsReportObservation currentReport)
      )
    , ("claims", jsonArray (map renderSlot (claimSlots currentReport)))
    ]

renderCatalog :: ClaimCatalog -> String
renderCatalog currentCatalog =
  jsonObject
    [ ("name", jsonString (claimCatalogName currentCatalog))
    , ( "coreClaims"
      , jsonArray
          ( map
              (jsonString . claimNameString)
              (claimCatalogCoreClaims currentCatalog)
          )
      )
    , ( "manifestClaim"
      , jsonString
          (claimNameString (claimCatalogManifestClaim currentCatalog))
      )
    ]

renderSummary :: CURDESemanticsSummary -> String
renderSummary currentSummary =
  jsonObject
    [ ("schemaExact", jsonBool (curdeSummarySchemaExact currentSummary))
    , ("catalogExact", jsonBool (curdeSummaryCatalogExact currentSummary))
    , ( "evidenceOrderExact"
      , jsonBool (curdeSummaryEvidenceOrderExact currentSummary)
      )
    , ("manifestExact", jsonBool (curdeSummaryManifestExact currentSummary))
    , ( "establishedCount"
      , jsonInt (curdeSummaryEstablishedCount currentSummary)
      )
    , ("violatedCount", jsonInt (curdeSummaryViolatedCount currentSummary))
    , ("blockedCount", jsonInt (curdeSummaryBlockedCount currentSummary))
    , ("deferredCount", jsonInt (curdeSummaryDeferredCount currentSummary))
    , ("passed", jsonBool (curdeSummaryPassed currentSummary))
    ]

renderObservation :: CURDEDemandObservation -> String
renderObservation currentObservation =
  jsonObject
    [ ( "loweringErrorCount"
      , jsonInt (length (curdeObservationLoweringErrors currentObservation))
      )
    , ( "directValidationErrorCount"
      , jsonInt
          ( length
              (curdeObservationDirectValidationErrors currentObservation)
          )
      )
    , ( "controlErrorCount"
      , jsonInt (length (curdeObservationControlErrors currentObservation))
      )
    , ( "layoutPathCount"
      , jsonInt (length (curdeObservationLayoutPaths currentObservation))
      )
    , ( "controlPathCount"
      , jsonInt (length (curdeObservationControlPaths currentObservation))
      )
    , ( "demandRootCount"
      , jsonInt (length (curdeObservationDemandRoots currentObservation))
      )
    , ( "demandOccurrenceCount"
      , jsonInt
          (length (curdeObservationDemandOccurrences currentObservation))
      )
    , ( "demandEdgeCount"
      , jsonInt (length (curdeObservationDemandEdges currentObservation))
      )
    , ( "demandClosureNodeCount"
      , jsonInt (length (curdeObservationDemandClosure currentObservation))
      )
    ]

renderSlot :: ClaimSlot -> String
renderSlot currentSlot =
  jsonObject
    ( [ ("name", jsonString (claimNameString (slotName currentSlot)))
      , ("evidenceCardinality", jsonInt (slotCardinality currentSlot))
      , ("status", renderSlotStatus currentSlot)
      ]
        ++ case slotEvidence currentSlot of
          Just currentItem ->
            [ ( "expected"
              , jsonString (curdeClaimEvidenceExpected currentItem)
              )
            ]
          Nothing -> []
    )

renderSlotStatus :: ClaimSlot -> String
renderSlotStatus currentSlot =
  case slotEvidence currentSlot of
    Nothing ->
      jsonObject [("kind", jsonString "violated")]
    Just currentItem ->
      renderEvidenceStatus (curdeClaimEvidenceStatus currentItem)

renderEvidenceStatus :: CURDEEvidenceStatus -> String
renderEvidenceStatus currentStatus =
  case currentStatus of
    EvidenceEstablished ->
      jsonObject [("kind", jsonString "established")]
    EvidenceViolated ->
      jsonObject [("kind", jsonString "violated")]
    EvidenceBlocked currentReasons ->
      jsonObject
        [ ("kind", jsonString "blocked")
        , ("reasonCount", jsonInt (length currentReasons))
        ]
    EvidenceDeferredTo currentBoundary ->
      jsonObject
        [ ("kind", jsonString "deferred")
        , ("boundary", jsonString (renderBoundary currentBoundary))
        ]

renderBoundary :: CURDEEvidenceBoundary -> String
renderBoundary currentBoundary =
  case currentBoundary of
    GenericRecordCompileTimeWitness ->
      "generic-record-compile-time-witness"
    PublicFacadeCompileTimeWitness ->
      "public-facade-compile-time-witness"
    RuntimeObservationWitness ->
      "runtime-observation-witness"
    RuntimeControlWitness ->
      "runtime-control-witness"

claimNameString :: ClaimName -> String
claimNameString (ClaimName currentName) =
  currentName

jsonObject :: [(String, String)] -> String
jsonObject currentFields =
  "{"
    ++ intercalate
      ","
      [ jsonString currentName ++ ":" ++ currentValue
      | (currentName, currentValue) <- currentFields
      ]
    ++ "}"

jsonArray :: [String] -> String
jsonArray currentValues =
  "[" ++ intercalate "," currentValues ++ "]"

jsonBool :: Bool -> String
jsonBool currentValue =
  if currentValue then "true" else "false"

jsonInt :: Int -> String
jsonInt =
  show

jsonString :: String -> String
jsonString currentValue =
  '"' : concatMap escapeJson currentValue ++ "\""

escapeJson :: Char -> String
escapeJson currentChar =
  case currentChar of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _
      | ord currentChar < 32 ->
          "\\u"
            ++ replicate (4 - length currentHex) '0'
            ++ currentHex
      | otherwise -> [currentChar]
      where
        currentHex =
          showHex (ord currentChar) ""