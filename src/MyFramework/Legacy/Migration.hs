module MyFramework.Legacy.Migration
  ( LegacyEffectDescriptor (..)
  , LegacyFactDescriptor (..)
  , LegacySendDescriptor (..)
  , LegacyHandlerDescriptor (..)
  , LegacyPolicyDescriptor (..)
  , LegacyTransformDescriptor (..)
  , LegacyVisibilityDescriptor (..)
  , LegacyPipelineDescriptor (..)
  , LegacyExternalTakeDescriptor (..)
  , MigrationSeverity (..)
  , MigrationIssueCode (..)
  , MigrationIssue (..)
  , PlannedCommandKind (..)
  , PlannedObservation (..)
  , PlannedCommand (..)
  , PlannedReadSource (..)
  , PlannedRead (..)
  , PlannedRExpr (..)
  , MigrationPlanStep (..)
  , ManualMigrationAction (..)
  , MigrationPlan (..)
  , LegacyMigrationReport (..)
  , analyzeLegacyEffect
  , planLegacyMigration
  , migrationReportIssues
  , migrationReportHasBlockers
  , migrationPlanCanAutoMaterialize
  , migrationIssueCodeText
  , migrationSeverityText
  , normalizeLegacySchema
  , sortMigrationIssues
  , canonicalizeLegacyEffectDescriptor
  , migrationReportCanonical
  , migrationPreservesSemanticOrder
  , encodeLegacyEffectDescriptor
  , decodeLegacyEffectDescriptor
  , encodeLegacyMigrationReport
  , decodeLegacyMigrationReport
  , legacyDescriptorRoundTrip
  , legacyMigrationReportRoundTrip
  ) where

import Data.List
  ( group
  , sort
  , sortOn
  )
import qualified Data.Set as Set
import Text.Read
  ( readMaybe
  )

-- This is a one-shot compatibility input, not a fourth authoring facade.
-- It deliberately contains no handler closure, runtime state, gate, or
-- executable policy.
data LegacyEffectDescriptor = LegacyEffectDescriptor
  { legacyEffectName :: String
  , legacyEffectFacts :: [LegacyFactDescriptor]
  , legacyEffectSends :: [LegacySendDescriptor]
  , legacyEffectHandlers :: [LegacyHandlerDescriptor]
  , legacyEffectPolicies :: [LegacyPolicyDescriptor]
  , legacyEffectTransforms :: [LegacyTransformDescriptor]
  , legacyEffectVisibility :: LegacyVisibilityDescriptor
  , legacyEffectPipelines :: [LegacyPipelineDescriptor]
  , legacyEffectExternalTakes :: [LegacyExternalTakeDescriptor]
  }
  deriving (Eq, Ord, Read, Show)

-- | Every list in a fact preserves legacy declaration order. Canonicalization
-- may reorder top-level facts, but it never rewrites a fact-internal sequence.
data LegacyFactDescriptor = LegacyFactDescriptor
  { legacyFactName :: String
  -- | Order is semantic: only the first need is a possible CURDE input.
  , legacyFactNeeds :: [String]
  , legacyFactUses :: [String]
  , legacyFactTakes :: [String]
  , legacyFactMakes :: [String]
  -- | Order is semantic and becomes an RExpr operator chain when valid.
  , legacyFactTransforms :: [String]
  , legacyFactExternal :: Bool
  , legacyFactOnFailure :: [String]
  , legacyFactErrorDispatch :: [String]
  }
  deriving (Eq, Ord, Read, Show)

data LegacySendDescriptor = LegacySendDescriptor
  { legacySendName :: String
  , legacySendInputSchema :: String
  , legacySendOutputSchema :: String
  }
  deriving (Eq, Ord, Read, Show)

data LegacyHandlerDescriptor = LegacyHandlerDescriptor
  { legacyHandlerSend :: String
  , legacyHandlerName :: String
  }
  deriving (Eq, Ord, Read, Show)

data LegacyPolicyDescriptor = LegacyPolicyDescriptor
  { legacyPolicySend :: String
  , legacyPolicyIdempotency :: Maybe String
  , legacyPolicyRetry :: Maybe String
  }
  deriving (Eq, Ord, Read, Show)

data LegacyTransformDescriptor = LegacyTransformDescriptor
  { legacyTransformName :: String
  , legacyTransformInputSchema :: String
  , legacyTransformOutputSchema :: String
  }
  deriving (Eq, Ord, Read, Show)

data LegacyVisibilityDescriptor = LegacyVisibilityDescriptor
  { legacyVisibilityImports :: [String]
  , legacyVisibilityPrivateFacts :: [String]
  , legacyVisibilityExports :: [String]
  }
  deriving (Eq, Ord, Read, Show)

data LegacyPipelineDescriptor = LegacyPipelineDescriptor
  { legacyPipelineName :: String
  -- | Pipeline order is evidence only. It is never converted to input edges.
  , legacyPipelineEntries :: [String]
  }
  deriving (Eq, Ord, Read, Show)

data LegacyExternalTakeDescriptor = LegacyExternalTakeDescriptor
  { legacyExternalTakeFact :: String
  , legacyExternalTakeOutputSchema :: Maybe String
  }
  deriving (Eq, Ord, Read, Show)

data MigrationSeverity
  = MigrationWarning
  | MigrationBlocker
  deriving (Eq, Ord, Read, Show)

data MigrationIssueCode
  = EmptyLegacyEffectName
  | EmptyLegacyFactName
  | EmptyLegacySendName
  | EmptyLegacyHandlerName
  | EmptyLegacyTransformName
  | EmptyLegacyPipelineName
  | DuplicateFactProducer
  | DuplicateSendSignature
  | DuplicateArtifactProducer
  | AmbiguousArtifactConsumer
  | ArtifactProducerNotReadable
  | MissingArtifactProducer
  | UnknownPrimaryInput
  | SelfPrimaryInput
  | ExtraNeedsRequireExplicitAstControl
  | MultipleUsesRequireAtomicSplit
  | MissingSendSignature
  | AmbiguousSendSignature
  | MissingHandlerResolution
  | AmbiguousHandlerResolution
  | MultipleTakesRequireClosedRecord
  | MissingImplementationArgument
  | ImplementationArgumentSchemaMismatch
  | MultipleMakesAmbiguous
  | ObservationSourceAmbiguous
  | ObservationSchemaMismatch
  | MissingTransformSignature
  | AmbiguousTransformSignature
  | TransformChainSchemaMismatch
  | TransformRequiresReadFact
  | CommandKindDefaultedToE
  | PolicyRequiresHandlerDecision
  | AmbiguousPolicyDeclaration
  | PipelineRequiresManualAst
  | FailurePathRequiresManualAst
  | ErrorDispatchRequiresHandlerDecision
  | VisibilityRequiresManualMapping
  | VisibilityConflict
  | ExternalFactRequiresReadContract
  | ExternalTakeRequiresReadContract
  deriving (Eq, Ord, Read, Show)

data MigrationIssue = MigrationIssue
  { migrationIssueCode :: MigrationIssueCode
  , migrationIssueSeverity :: MigrationSeverity
  , migrationIssueSubject :: String
  , migrationIssueDetail :: String
  }
  deriving (Eq, Ord, Read, Show)

-- | Legacy facts never justify guessing C, U, or D. The only mechanical
-- command classification is E.
data PlannedCommandKind
  = MechanicalE
  deriving (Eq, Ord, Read, Show)

data PlannedReadSource
  = PlannedCommandObservation String
  | PlannedExternalTake String
  deriving (Eq, Ord, Read, Show)

data PlannedRead = PlannedRead
  { plannedReadName :: String
  , plannedReadSchema :: String
  , plannedReadSource :: PlannedReadSource
  }
  deriving (Eq, Ord, Read, Show)

-- | A closed audit form corresponding to the future RExpr declaration.
-- It contains references, Unit, and registered unary operators only.
data PlannedRExpr
  = PlannedUnitLiteral String
  | PlannedRReference PlannedRead
  | PlannedOperatorApplication String String PlannedRExpr
  deriving (Eq, Ord, Read, Show)

data PlannedObservation
  = PlannedDiscardObservation
  | PlannedCaptureObservation String
  deriving (Eq, Ord, Read, Show)

data PlannedCommand = PlannedCommand
  { plannedCommandName :: String
  , plannedCommandKind :: PlannedCommandKind
  , plannedCommandSend :: Maybe String
  , plannedCommandArgumentSchema :: String
  , plannedCommandObservation :: PlannedObservation
  , plannedCommandInput :: Maybe String
  }
  deriving (Eq, Ord, Read, Show)

data MigrationPlanStep
  = PlanCommandHandle PlannedCommand
  -- | One occurrence per legacy use, in exact declaration order. The
  -- occurrence index keeps repeated uses distinct under stable de-duplication.
  | PlanAtomicEHandle String Int PlannedCommand
  | PlanRHandle PlannedRead
  | PlanReadFact LegacyTransformDescriptor
  | PlanImplementation String String String PlannedRExpr
  | PlanHandlerBinding String String
  deriving (Eq, Ord, Read, Show)

-- | Manual actions identify the boundary where the analyzer stops. They are
-- evidence, not AST nodes, runtime behavior, or facade declarations.
data ManualMigrationAction
  = RequiresExplicitAstControl String [String]
  | RequiresAtomicESplit String [String]
  | RequiresSendResolution String
  | RequiresHandlerRegistration String
  | RequiresHandlerPolicyDecision
      String
      (Maybe String)
      (Maybe String)
  | RequiresAstFailureControl String [String]
  | RequiresHandlerErrorDecision String [String]
  | RequiresPipelineAstRewrite String [String]
  | RequiresVisibilityMapping LegacyVisibilityDescriptor
  | RequiresReadSourceContract String (Maybe String)
  | RequiresArtifactProducerResolution String String [String]
  | RequiresClosedArgumentRecord String [String]
  | RequiresArgumentExpression String String
  | RequiresTransformResolution String [String]
  | RequiresObservationResolution String [String]
  deriving (Eq, Ord, Read, Show)

data MigrationPlan = MigrationPlan
  { migrationPlanEffectName :: String
  , migrationPlanSteps :: [MigrationPlanStep]
  , migrationPlanManualActions :: [ManualMigrationAction]
  }
  deriving (Eq, Ord, Read, Show)

data LegacyMigrationReport = LegacyMigrationReport
  { legacyMigrationDescriptor :: LegacyEffectDescriptor
  , legacyMigrationPlan :: MigrationPlan
  , legacyMigrationIssues :: [MigrationIssue]
  }
  deriving (Eq, Ord, Read, Show)

analyzeLegacyEffect :: LegacyEffectDescriptor -> LegacyMigrationReport
analyzeLegacyEffect originalDescriptor =
  LegacyMigrationReport
    { legacyMigrationDescriptor = currentDescriptor
    , legacyMigrationPlan = buildPlan currentDescriptor
    , legacyMigrationIssues = buildIssues currentDescriptor
    }
  where
    currentDescriptor =
      canonicalizeLegacyEffectDescriptor originalDescriptor

buildPlan :: LegacyEffectDescriptor -> MigrationPlan
buildPlan currentDescriptor =
  MigrationPlan
    { migrationPlanEffectName = legacyEffectName currentDescriptor
    , migrationPlanSteps =
        stableUnique
          ( globalPlanSteps currentDescriptor
              ++ concatMap
                (factPlanSteps currentDescriptor)
                (legacyEffectFacts currentDescriptor)
          )
    , migrationPlanManualActions =
        stableUnique
          ( globalManualActions currentDescriptor
              ++ concatMap
                (factManualActions currentDescriptor)
                (legacyEffectFacts currentDescriptor)
          )
    }

buildIssues :: LegacyEffectDescriptor -> [MigrationIssue]
buildIssues currentDescriptor =
  sortMigrationIssues
    ( globalIssues currentDescriptor
        ++ concatMap
          (factIssues currentDescriptor)
          (legacyEffectFacts currentDescriptor)
    )

planLegacyMigration :: LegacyEffectDescriptor -> MigrationPlan
planLegacyMigration =
  legacyMigrationPlan . analyzeLegacyEffect

migrationReportIssues :: LegacyMigrationReport -> [MigrationIssue]
migrationReportIssues =
  legacyMigrationIssues

migrationReportHasBlockers :: LegacyMigrationReport -> Bool
migrationReportHasBlockers =
  any ((== MigrationBlocker) . migrationIssueSeverity)
    . migrationReportIssues

migrationPlanCanAutoMaterialize :: LegacyMigrationReport -> Bool
migrationPlanCanAutoMaterialize currentReport =
  not (migrationReportHasBlockers currentReport)
    && null
      (migrationPlanManualActions (legacyMigrationPlan currentReport))

migrationIssueCodeText :: MigrationIssueCode -> String
migrationIssueCodeText currentCode =
  case currentCode of
    EmptyLegacyEffectName -> "empty-legacy-effect-name"
    EmptyLegacyFactName -> "empty-legacy-fact-name"
    EmptyLegacySendName -> "empty-legacy-send-name"
    EmptyLegacyHandlerName -> "empty-legacy-handler-name"
    EmptyLegacyTransformName -> "empty-legacy-transform-name"
    EmptyLegacyPipelineName -> "empty-legacy-pipeline-name"
    DuplicateFactProducer -> "duplicate-fact-producer"
    DuplicateSendSignature -> "duplicate-send-signature"
    DuplicateArtifactProducer -> "duplicate-artifact-producer"
    AmbiguousArtifactConsumer -> "ambiguous-artifact-consumer"
    ArtifactProducerNotReadable -> "artifact-producer-not-readable"
    MissingArtifactProducer -> "missing-artifact-producer"
    UnknownPrimaryInput -> "unknown-primary-input"
    SelfPrimaryInput -> "self-primary-input"
    ExtraNeedsRequireExplicitAstControl ->
      "requires-explicit-ast-control"
    MultipleUsesRequireAtomicSplit ->
      "multiple-uses-require-atomic-split"
    MissingSendSignature -> "missing-send-signature"
    AmbiguousSendSignature -> "ambiguous-send-signature"
    MissingHandlerResolution -> "missing-handler-resolution"
    AmbiguousHandlerResolution -> "ambiguous-handler-resolution"
    MultipleTakesRequireClosedRecord ->
      "multiple-takes-require-closed-record"
    MissingImplementationArgument -> "missing-implementation-argument"
    ImplementationArgumentSchemaMismatch ->
      "implementation-argument-schema-mismatch"
    MultipleMakesAmbiguous -> "multiple-makes-ambiguous"
    ObservationSourceAmbiguous -> "observation-source-ambiguous"
    ObservationSchemaMismatch -> "observation-schema-mismatch"
    MissingTransformSignature -> "missing-transform-signature"
    AmbiguousTransformSignature -> "ambiguous-transform-signature"
    TransformChainSchemaMismatch -> "transform-chain-schema-mismatch"
    TransformRequiresReadFact -> "transform-requires-read-fact"
    CommandKindDefaultedToE -> "command-kind-defaulted-to-e"
    PolicyRequiresHandlerDecision -> "policy-requires-handler-decision"
    AmbiguousPolicyDeclaration -> "ambiguous-policy-declaration"
    PipelineRequiresManualAst -> "pipeline-requires-manual-ast"
    FailurePathRequiresManualAst -> "failure-path-requires-manual-ast"
    ErrorDispatchRequiresHandlerDecision ->
      "error-dispatch-requires-handler-decision"
    VisibilityRequiresManualMapping -> "visibility-requires-manual-mapping"
    VisibilityConflict -> "visibility-conflict"
    ExternalFactRequiresReadContract ->
      "external-fact-requires-read-contract"
    ExternalTakeRequiresReadContract ->
      "external-take-requires-read-contract"

migrationSeverityText :: MigrationSeverity -> String
migrationSeverityText currentSeverity =
  case currentSeverity of
    MigrationWarning -> "warning"
    MigrationBlocker -> "blocker"

sortMigrationIssues :: [MigrationIssue] -> [MigrationIssue]
sortMigrationIssues =
  uniqueSorted

canonicalizeLegacyEffectDescriptor ::
  LegacyEffectDescriptor ->
  LegacyEffectDescriptor
canonicalizeLegacyEffectDescriptor currentDescriptor =
  currentDescriptor
    { legacyEffectFacts =
        canonicalFacts (legacyEffectFacts currentDescriptor)
    , legacyEffectSends =
        canonicalSends (legacyEffectSends currentDescriptor)
    , legacyEffectHandlers =
        canonicalHandlers (legacyEffectHandlers currentDescriptor)
    , legacyEffectPolicies =
        canonicalPolicies (legacyEffectPolicies currentDescriptor)
    , legacyEffectTransforms =
        canonicalTransforms (legacyEffectTransforms currentDescriptor)
    , legacyEffectVisibility =
        canonicalVisibility (legacyEffectVisibility currentDescriptor)
    , legacyEffectPipelines =
        canonicalPipelines (legacyEffectPipelines currentDescriptor)
    , legacyEffectExternalTakes =
        canonicalExternalTakes (legacyEffectExternalTakes currentDescriptor)
    }

canonicalFacts :: [LegacyFactDescriptor] -> [LegacyFactDescriptor]
canonicalFacts =
  sortOn (\currentFact -> (legacyFactName currentFact, currentFact))

canonicalSends :: [LegacySendDescriptor] -> [LegacySendDescriptor]
canonicalSends =
  sortOn (\currentSend -> (legacySendName currentSend, currentSend))

canonicalHandlers :: [LegacyHandlerDescriptor] -> [LegacyHandlerDescriptor]
canonicalHandlers =
  sortOn
    ( \currentHandler ->
        ( legacyHandlerSend currentHandler
        , legacyHandlerName currentHandler
        , currentHandler
        )
    )

canonicalPolicies :: [LegacyPolicyDescriptor] -> [LegacyPolicyDescriptor]
canonicalPolicies =
  sortOn (\currentPolicy -> (legacyPolicySend currentPolicy, currentPolicy))

canonicalTransforms ::
  [LegacyTransformDescriptor] ->
  [LegacyTransformDescriptor]
canonicalTransforms =
  sortOn
    ( \currentTransform ->
        (legacyTransformName currentTransform, currentTransform)
    )

canonicalPipelines ::
  [LegacyPipelineDescriptor] ->
  [LegacyPipelineDescriptor]
canonicalPipelines =
  sortOn
    ( \currentPipeline ->
        (legacyPipelineName currentPipeline, currentPipeline)
    )

canonicalExternalTakes ::
  [LegacyExternalTakeDescriptor] ->
  [LegacyExternalTakeDescriptor]
canonicalExternalTakes =
  sortOn
    ( \currentTake ->
        ( legacyExternalTakeFact currentTake
        , legacyExternalTakeOutputSchema currentTake
        , currentTake
        )
    )

canonicalVisibility ::
  LegacyVisibilityDescriptor ->
  LegacyVisibilityDescriptor
canonicalVisibility currentVisibility =
  currentVisibility
    { legacyVisibilityImports =
        sort (legacyVisibilityImports currentVisibility)
    , legacyVisibilityPrivateFacts =
        sort (legacyVisibilityPrivateFacts currentVisibility)
    , legacyVisibilityExports =
        sort (legacyVisibilityExports currentVisibility)
    }

migrationReportCanonical :: LegacyMigrationReport -> Bool
migrationReportCanonical currentReport =
  currentDescriptor
    == canonicalizeLegacyEffectDescriptor currentDescriptor
    && legacyMigrationIssues currentReport
      == sortMigrationIssues (legacyMigrationIssues currentReport)
    && migrationPlanSteps currentPlan
      == stableUnique (migrationPlanSteps currentPlan)
    && migrationPlanManualActions currentPlan
      == stableUnique (migrationPlanManualActions currentPlan)
    && currentPlan == buildPlan currentDescriptor
  where
    currentDescriptor =
      legacyMigrationDescriptor currentReport
    currentPlan =
      legacyMigrationPlan currentReport

-- | Pure witness that canonicalization preserves every declaration sequence
-- inside a fact and every pipeline entry sequence. It also proves that atomic
-- E split steps follow the original uses order, including repeated sends.
migrationPreservesSemanticOrder ::
  LegacyEffectDescriptor ->
  LegacyMigrationReport ->
  Bool
migrationPreservesSemanticOrder originalDescriptor currentReport =
  currentDescriptor == expectedDescriptor
    && actualFactOrders == expectedFactOrders
    && actualPipelineOrders == expectedPipelineOrders
    && actualAtomicOrder == expectedAtomicOrder
    && legacyMigrationPlan currentReport == buildPlan expectedDescriptor
  where
    currentDescriptor =
      legacyMigrationDescriptor currentReport
    expectedDescriptor =
      canonicalizeLegacyEffectDescriptor originalDescriptor
    expectedFactOrders =
      map semanticFactOrder
        (canonicalFacts (legacyEffectFacts originalDescriptor))
    actualFactOrders =
      map semanticFactOrder (legacyEffectFacts currentDescriptor)
    expectedPipelineOrders =
      [ (legacyPipelineName currentPipeline, legacyPipelineEntries currentPipeline)
      | currentPipeline <-
          canonicalPipelines (legacyEffectPipelines originalDescriptor)
      ]
    actualPipelineOrders =
      [ (legacyPipelineName currentPipeline, legacyPipelineEntries currentPipeline)
      | currentPipeline <- legacyEffectPipelines currentDescriptor
      ]
    expectedAtomicOrder =
      atomicOrderFromDescriptor expectedDescriptor
    actualAtomicOrder =
      atomicOrderFromPlan (legacyMigrationPlan currentReport)

data SemanticFactOrder = SemanticFactOrder
  { semanticFactName :: String
  , semanticFactNeeds :: [String]
  , semanticFactUses :: [String]
  , semanticFactTakes :: [String]
  , semanticFactMakes :: [String]
  , semanticFactTransforms :: [String]
  , semanticFactOnFailure :: [String]
  , semanticFactErrorDispatch :: [String]
  }
  deriving (Eq)

semanticFactOrder :: LegacyFactDescriptor -> SemanticFactOrder
semanticFactOrder currentFact =
  SemanticFactOrder
    { semanticFactName = legacyFactName currentFact
    , semanticFactNeeds = legacyFactNeeds currentFact
    , semanticFactUses = legacyFactUses currentFact
    , semanticFactTakes = legacyFactTakes currentFact
    , semanticFactMakes = legacyFactMakes currentFact
    , semanticFactTransforms = legacyFactTransforms currentFact
    , semanticFactOnFailure = legacyFactOnFailure currentFact
    , semanticFactErrorDispatch = legacyFactErrorDispatch currentFact
    }

atomicOrderFromDescriptor ::
  LegacyEffectDescriptor ->
  [(String, Int, String)]
atomicOrderFromDescriptor currentDescriptor =
  [ (legacyFactName currentFact, currentOccurrence, currentSend)
  | currentFact <- legacyEffectFacts currentDescriptor
  , length (legacyFactUses currentFact) > 1
  , (currentOccurrence, currentSend) <-
      indexedItems (legacyFactUses currentFact)
  , length (sendsNamed currentDescriptor currentSend) == 1
  ]

atomicOrderFromPlan :: MigrationPlan -> [(String, Int, String)]
atomicOrderFromPlan currentPlan =
  [ (currentFact, currentOccurrence, currentSend)
  | PlanAtomicEHandle
      currentFact
      currentOccurrence
      currentCommand <- migrationPlanSteps currentPlan
  , Just currentSend <- [plannedCommandSend currentCommand]
  ]

encodeLegacyEffectDescriptor :: LegacyEffectDescriptor -> String
encodeLegacyEffectDescriptor =
  show

decodeLegacyEffectDescriptor ::
  String ->
  Either String LegacyEffectDescriptor
decodeLegacyEffectDescriptor currentText =
  case readMaybe currentText of
    Just currentDescriptor ->
      Right currentDescriptor
    Nothing ->
      Left "invalid LegacyEffectDescriptor"

encodeLegacyMigrationReport :: LegacyMigrationReport -> String
encodeLegacyMigrationReport =
  show

decodeLegacyMigrationReport ::
  String ->
  Either String LegacyMigrationReport
decodeLegacyMigrationReport currentText =
  case readMaybe currentText of
    Just currentReport ->
      Right currentReport
    Nothing ->
      Left "invalid LegacyMigrationReport"

legacyDescriptorRoundTrip :: LegacyEffectDescriptor -> Bool
legacyDescriptorRoundTrip currentDescriptor =
  decodeLegacyEffectDescriptor (encodeLegacyEffectDescriptor currentDescriptor)
    == Right currentDescriptor

legacyMigrationReportRoundTrip :: LegacyMigrationReport -> Bool
legacyMigrationReportRoundTrip currentReport =
  decodeLegacyMigrationReport (encodeLegacyMigrationReport currentReport)
    == Right currentReport

globalPlanSteps :: LegacyEffectDescriptor -> [MigrationPlanStep]
globalPlanSteps currentDescriptor =
  handlerSteps ++ operatorSteps ++ externalReadSteps
  where
    handlerSteps =
      [ PlanHandlerBinding
          (legacyHandlerSend currentHandler)
          (legacyHandlerName currentHandler)
      | currentHandler <- legacyEffectHandlers currentDescriptor
      , length
          (handlersNamed currentDescriptor (legacyHandlerSend currentHandler))
          == 1
      , length
          (sendsNamed currentDescriptor (legacyHandlerSend currentHandler))
          == 1
      ]
    operatorSteps =
      [ PlanReadFact (normalizeTransformForPlan currentTransform)
      | currentTransform <- legacyEffectTransforms currentDescriptor
      , length
          ( transformsNamed
              currentDescriptor
              (legacyTransformName currentTransform)
          )
          == 1
      ]
    externalReadSteps =
      [ PlanRHandle (plannedExternalRead currentTake currentSchema)
      | currentTake <- legacyEffectExternalTakes currentDescriptor
      , Just currentSchema <-
          [legacyExternalTakeOutputSchema currentTake]
      , length (externalTakesForSchema currentDescriptor currentSchema) == 1
      ]

normalizeTransformForPlan ::
  LegacyTransformDescriptor ->
  LegacyTransformDescriptor
normalizeTransformForPlan currentTransform =
  currentTransform
    { legacyTransformInputSchema =
        normalizeLegacySchema (legacyTransformInputSchema currentTransform)
    , legacyTransformOutputSchema =
        normalizeLegacySchema (legacyTransformOutputSchema currentTransform)
    }
factPlanSteps ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  [MigrationPlanStep]
factPlanSteps currentDescriptor currentFact =
  commandSteps ++ observationSteps ++ implementationSteps
  where
    currentName =
      legacyFactName currentFact
    currentPrimaryInput =
      firstItem (legacyFactNeeds currentFact)
    commandSteps =
      case legacyFactUses currentFact of
        [] ->
          [ PlanCommandHandle
              PlannedCommand
                { plannedCommandName = currentName
                , plannedCommandKind = MechanicalE
                , plannedCommandSend = Nothing
                , plannedCommandArgumentSchema = "Unit"
                , plannedCommandObservation = PlannedDiscardObservation
                , plannedCommandInput = currentPrimaryInput
                }
          ]
        [currentSend] ->
          case sendsNamed currentDescriptor currentSend of
            [currentSignature] ->
              [ PlanCommandHandle
                  ( plannedCommandFor
                      currentDescriptor
                      currentFact
                      0
                      currentName
                      currentSend
                      currentSignature
                      currentPrimaryInput
                  )
              ]
            _ ->
              []
        currentUses ->
          [ PlanAtomicEHandle
              currentName
              currentOccurrence
              ( plannedCommandFor
                  currentDescriptor
                  currentFact
                  currentOccurrence
                  (atomicHandleName currentName currentOccurrence currentSend)
                  currentSend
                  currentSignature
                  currentPrimaryInput
              )
          | (currentOccurrence, currentSend) <- indexedItems currentUses
          , [currentSignature] <-
              [sendsNamed currentDescriptor currentSend]
          ]
    observationSteps =
      case plannedObservationRead currentDescriptor currentFact of
        Nothing -> []
        Just currentRead -> [PlanRHandle currentRead]
    implementationSteps =
      case legacyFactUses currentFact of
        [currentSend] ->
          case sendsNamed currentDescriptor currentSend of
            [currentSignature]
              | isUnitSchema (legacySendInputSchema currentSignature) ->
                  []
              | otherwise ->
                  case
                    resolveImplementationArgument
                      currentDescriptor
                      currentFact
                      currentSignature of
                    Just (currentExpression, _) ->
                      [ PlanImplementation
                          currentName
                          currentSend
                          ( normalizeLegacySchema
                              (legacySendInputSchema currentSignature)
                          )
                          currentExpression
                      ]
                    Nothing ->
                      []
            _ ->
              []
        _ ->
          []

plannedCommandFor ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  Int ->
  String ->
  String ->
  LegacySendDescriptor ->
  Maybe String ->
  PlannedCommand
plannedCommandFor
  currentDescriptor
  currentFact
  currentOccurrence
  currentCommandName
  currentSend
  currentSignature
  currentInput =
    PlannedCommand
      { plannedCommandName = currentCommandName
      , plannedCommandKind = MechanicalE
      , plannedCommandSend = Just currentSend
      , plannedCommandArgumentSchema =
          normalizeLegacySchema
            (legacySendInputSchema currentSignature)
      , plannedCommandObservation =
          plannedObservationFor
            currentDescriptor
            currentFact
            currentOccurrence
            currentSignature
      , plannedCommandInput = currentInput
      }

globalManualActions ::
  LegacyEffectDescriptor ->
  [ManualMigrationAction]
globalManualActions currentDescriptor =
  policyActions
    ++ pipelineActions
    ++ visibilityActions
    ++ externalTakeActions
    ++ duplicateSendActions
    ++ duplicateHandlerActions
    ++ duplicateTransformActions
  where
    policyActions =
      [ RequiresHandlerPolicyDecision
          (legacyPolicySend currentPolicy)
          (legacyPolicyIdempotency currentPolicy)
          (legacyPolicyRetry currentPolicy)
      | currentPolicy <- legacyEffectPolicies currentDescriptor
      ]
    pipelineActions =
      [ RequiresPipelineAstRewrite
          (legacyPipelineName currentPipeline)
          (legacyPipelineEntries currentPipeline)
      | currentPipeline <- legacyEffectPipelines currentDescriptor
      ]
    visibilityActions =
      [RequiresVisibilityMapping currentVisibility | visibilityIsPresent]
    currentVisibility =
      legacyEffectVisibility currentDescriptor
    visibilityIsPresent =
      not
        ( null (legacyVisibilityImports currentVisibility)
            && null (legacyVisibilityPrivateFacts currentVisibility)
            && null (legacyVisibilityExports currentVisibility)
        )
    externalTakeActions =
      [ RequiresReadSourceContract
          (legacyExternalTakeFact currentTake)
          (fmap normalizeLegacySchema (legacyExternalTakeOutputSchema currentTake))
      | currentTake <- legacyEffectExternalTakes currentDescriptor
      ]
    duplicateSendActions =
      map
        RequiresSendResolution
        (duplicates (map legacySendName (legacyEffectSends currentDescriptor)))
    duplicateHandlerActions =
      map
        RequiresHandlerRegistration
        ( duplicates
            (map legacyHandlerSend (legacyEffectHandlers currentDescriptor))
        )
    duplicateTransformActions =
      [ RequiresTransformResolution currentName [currentName]
      | currentName <-
          duplicates
            (map legacyTransformName (legacyEffectTransforms currentDescriptor))
      ]

factManualActions ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  [ManualMigrationAction]
factManualActions currentDescriptor currentFact =
  extraNeedActions
    ++ atomicSplitActions
    ++ sendActions
    ++ handlerActions
    ++ takeActions
    ++ argumentActions
    ++ transformActions
    ++ observationActions
    ++ failureActions
    ++ errorActions
    ++ externalActions
  where
    currentName =
      legacyFactName currentFact
    currentUses =
      legacyFactUses currentFact
    extraNeedActions =
      [ RequiresExplicitAstControl currentName currentExtras
      | let currentExtras = drop 1 (legacyFactNeeds currentFact)
      , not (null currentExtras)
      ]
    atomicSplitActions =
      [RequiresAtomicESplit currentName currentUses | length currentUses > 1]
    sendActions =
      [ RequiresSendResolution currentSend
      | currentSend <- currentUses
      , length (sendsNamed currentDescriptor currentSend) /= 1
      ]
    handlerActions =
      [ RequiresHandlerRegistration currentSend
      | currentSend <- currentUses
      , length (handlersNamed currentDescriptor currentSend) /= 1
      ]
    takeActions =
      case legacyFactTakes currentFact of
        [] ->
          []
        [currentSchema] ->
          let currentCandidates =
                artifactSourceNames currentDescriptor currentSchema
          in [ RequiresArtifactProducerResolution
                 currentName
                 (normalizeLegacySchema currentSchema)
                 currentCandidates
             | length currentCandidates /= 1
             ]
        currentSchemas ->
          [ RequiresClosedArgumentRecord
              currentName
              (map normalizeLegacySchema currentSchemas)
          ]
    argumentActions =
      case currentUses of
        [currentSend] ->
          case sendsNamed currentDescriptor currentSend of
            [currentSignature] ->
              [ RequiresArgumentExpression
                  currentName
                  (normalizeLegacySchema (legacySendInputSchema currentSignature))
              | resolveImplementationArgument
                  currentDescriptor
                  currentFact
                  currentSignature
                  == Nothing
              ]
            _ ->
              []
        _ ->
          []
    transformActions =
      [ RequiresTransformResolution currentName currentTransforms
      | let currentTransforms = legacyFactTransforms currentFact
      , any
          ( \currentTransform ->
              length (transformsNamed currentDescriptor currentTransform) /= 1
          )
          currentTransforms
      ]
    observationActions =
      [ RequiresObservationResolution
          currentName
          (map normalizeLegacySchema (legacyFactMakes currentFact))
      | not (null (legacyFactMakes currentFact))
      , plannedObservationRead currentDescriptor currentFact == Nothing
      ]
    failureActions =
      [ RequiresAstFailureControl
          currentName
          (legacyFactOnFailure currentFact)
      | not (null (legacyFactOnFailure currentFact))
      ]
    errorActions =
      [ RequiresHandlerErrorDecision
          currentName
          (legacyFactErrorDispatch currentFact)
      | not (null (legacyFactErrorDispatch currentFact))
      ]
    externalActions =
      [RequiresReadSourceContract currentName Nothing | legacyFactExternal currentFact]

globalIssues :: LegacyEffectDescriptor -> [MigrationIssue]
globalIssues currentDescriptor =
  identityIssues
    ++ duplicateFactIssues
    ++ duplicateSendIssues
    ++ duplicateHandlerIssues
    ++ duplicatePolicyIssues
    ++ duplicateTransformIssues
    ++ artifactProducerIssues
    ++ policyIssues
    ++ pipelineIssues
    ++ visibilityIssues
    ++ externalTakeIssues
    ++ danglingHandlerIssues
  where
    identityIssues =
      [ migrationIssue
          MigrationBlocker
          EmptyLegacyEffectName
          "effect"
          "legacy effect name must not be empty"
      | null (legacyEffectName currentDescriptor)
      ]
        ++ [ migrationIssue
               MigrationBlocker
               EmptyLegacyFactName
               "fact"
               "legacy fact name must not be empty"
           | currentFact <- legacyEffectFacts currentDescriptor
           , null (legacyFactName currentFact)
           ]
        ++ [ migrationIssue
               MigrationBlocker
               EmptyLegacySendName
               "send"
               "legacy send name must not be empty"
           | currentSend <- legacyEffectSends currentDescriptor
           , null (legacySendName currentSend)
           ]
        ++ [ migrationIssue
               MigrationBlocker
               EmptyLegacyHandlerName
               (legacyHandlerSend currentHandler)
               "legacy handler name must not be empty"
           | currentHandler <- legacyEffectHandlers currentDescriptor
           , null (legacyHandlerName currentHandler)
           ]
        ++ [ migrationIssue
               MigrationBlocker
               EmptyLegacyTransformName
               "transform"
               "legacy transform name must not be empty"
           | currentTransform <- legacyEffectTransforms currentDescriptor
           , null (legacyTransformName currentTransform)
           ]
        ++ [ migrationIssue
               MigrationBlocker
               EmptyLegacyPipelineName
               "pipeline"
               "legacy pipeline name must not be empty"
           | currentPipeline <- legacyEffectPipelines currentDescriptor
           , null (legacyPipelineName currentPipeline)
           ]
    duplicateFactIssues =
      [ migrationIssue
          MigrationBlocker
          DuplicateFactProducer
          currentName
          "the same legacy fact has more than one producer declaration"
      | currentName <-
          duplicates (map legacyFactName (legacyEffectFacts currentDescriptor))
      ]
    duplicateSendIssues =
      [ migrationIssue
          MigrationBlocker
          DuplicateSendSignature
          currentName
          "the same send has more than one signature"
      | currentName <-
          duplicates (map legacySendName (legacyEffectSends currentDescriptor))
      ]
    duplicateHandlerIssues =
      [ migrationIssue
          MigrationBlocker
          AmbiguousHandlerResolution
          currentName
          "the same send has more than one handler declaration"
      | currentName <-
          duplicates
            (map legacyHandlerSend (legacyEffectHandlers currentDescriptor))
      ]
    duplicatePolicyIssues =
      [ migrationIssue
          MigrationBlocker
          AmbiguousPolicyDeclaration
          currentName
          "the same send has more than one policy declaration"
      | currentName <-
          duplicates
            (map legacyPolicySend (legacyEffectPolicies currentDescriptor))
      ]
    duplicateTransformIssues =
      [ migrationIssue
          MigrationBlocker
          AmbiguousTransformSignature
          currentName
          "the same transform name has more than one signature declaration"
      | currentName <-
          duplicates
            (map legacyTransformName (legacyEffectTransforms currentDescriptor))
      ]
    artifactProducerIssues =
      [ migrationIssue
          MigrationBlocker
          DuplicateArtifactProducer
          currentSchema
          ( "artifact schema has multiple legacy producers: "
              ++ show (artifactProducerNames currentDescriptor currentSchema)
          )
      | currentSchema <- allMadeSchemas currentDescriptor
      , length (artifactProducerNames currentDescriptor currentSchema) > 1
      ]
    policyIssues =
      [ migrationIssue
          MigrationWarning
          PolicyRequiresHandlerDecision
          (legacyPolicySend currentPolicy)
          ( "retry and idempotency remain Handler/manual decisions: "
              ++ show currentPolicy
          )
      | currentPolicy <- legacyEffectPolicies currentDescriptor
      ]
    pipelineIssues =
      [ migrationIssue
          MigrationWarning
          PipelineRequiresManualAst
          (legacyPipelineName currentPipeline)
          "legacy pipeline order must be reviewed as explicit AST control"
      | currentPipeline <- legacyEffectPipelines currentDescriptor
      ]
    visibilityIssues =
      visibilityDescriptorIssues
        (legacyEffectVisibility currentDescriptor)
        (map legacyFactName (legacyEffectFacts currentDescriptor))
    externalTakeIssues =
      [ migrationIssue
          MigrationWarning
          ExternalTakeRequiresReadContract
          (legacyExternalTakeFact currentTake)
          ( "external take can only plan an R source; codec/handler remains manual: "
              ++ show (legacyExternalTakeOutputSchema currentTake)
          )
      | currentTake <- legacyEffectExternalTakes currentDescriptor
      ]
    danglingHandlerIssues =
      [ migrationIssue
          MigrationWarning
          MissingSendSignature
          (legacyHandlerSend currentHandler)
          "handler declaration refers to a send without a unique signature"
      | currentHandler <- legacyEffectHandlers currentDescriptor
      , length
          (sendsNamed currentDescriptor (legacyHandlerSend currentHandler))
          /= 1
      ]

factIssues ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  [MigrationIssue]
factIssues currentDescriptor currentFact =
  commandKindIssues
    ++ primaryInputIssues
    ++ extraNeedIssues
    ++ useIssues
    ++ handlerIssues
    ++ takeIssues
    ++ argumentIssues
    ++ makeIssues
    ++ transformWarnings
    ++ failureIssues
    ++ errorIssues
    ++ externalIssues
  where
    currentName =
      legacyFactName currentFact
    currentUses =
      legacyFactUses currentFact
    commandKindIssues =
      [ migrationIssue
          MigrationWarning
          CommandKindDefaultedToE
          currentName
          "legacy semantics do not prove C, U, or D; the mechanical plan uses E"
      ]
    primaryInputIssues =
      case firstItem (legacyFactNeeds currentFact) of
        Nothing ->
          []
        Just currentInput ->
          [ migrationIssue
              MigrationBlocker
              SelfPrimaryInput
              currentName
              "the first legacy need would create a self input"
          | currentInput == currentName
          ]
            ++ [ migrationIssue
                   MigrationBlocker
                   UnknownPrimaryInput
                   currentName
                   ("the first legacy need has no unique producer: " ++ currentInput)
               | length (factsNamed currentDescriptor currentInput) /= 1
               ]
    extraNeedIssues =
      [ migrationIssue
          MigrationBlocker
          ExtraNeedsRequireExplicitAstControl
          currentName
          ( "only the first need may become input; extra needs require explicit AST control: "
              ++ show currentExtras
          )
      | let currentExtras = drop 1 (legacyFactNeeds currentFact)
      , not (null currentExtras)
      ]
    useIssues =
      [ migrationIssue
          MigrationBlocker
          MultipleUsesRequireAtomicSplit
          currentName
          ( "multiple uses require separate atomic E handles before materialization: "
              ++ show currentUses
          )
      | length currentUses > 1
      ]
        ++ concatMap (sendUseIssues currentDescriptor currentName) currentUses
    handlerIssues =
      concatMap (handlerUseIssues currentDescriptor currentName) currentUses
    takeIssues =
      factTakeIssues currentDescriptor currentFact
    argumentIssues =
      case currentUses of
        [currentSend] ->
          case sendsNamed currentDescriptor currentSend of
            [currentSignature] ->
              implementationArgumentIssues
                currentDescriptor
                currentFact
                currentSignature
            _ ->
              []
        _ ->
          []
    makeIssues =
      factMakeIssues currentDescriptor currentFact
    transformWarnings =
      [ migrationIssue
          MigrationWarning
          TransformRequiresReadFact
          currentName
          ( "transform is planned as an explicit R Fact handler: "
              ++ currentTransform
          )
      | currentTransform <- legacyFactTransforms currentFact
      ]
    failureIssues =
      [ migrationIssue
          MigrationWarning
          FailurePathRequiresManualAst
          currentName
          ( "onFailure is not migrated into CURDE; review explicit AST control: "
              ++ show (legacyFactOnFailure currentFact)
          )
      | not (null (legacyFactOnFailure currentFact))
      ]
    errorIssues =
      [ migrationIssue
          MigrationWarning
          ErrorDispatchRequiresHandlerDecision
          currentName
          ( "error dispatch remains a Handler decision: "
              ++ show (legacyFactErrorDispatch currentFact)
          )
      | not (null (legacyFactErrorDispatch currentFact))
      ]
    externalIssues =
      [ migrationIssue
          MigrationWarning
          ExternalFactRequiresReadContract
          currentName
          "external fact state requires an explicit R source codec/handler contract"
      | legacyFactExternal currentFact
      ]

sendUseIssues ::
  LegacyEffectDescriptor ->
  String ->
  String ->
  [MigrationIssue]
sendUseIssues currentDescriptor currentFact currentSend =
  case sendsNamed currentDescriptor currentSend of
    [] ->
      [ migrationIssue
          MigrationBlocker
          MissingSendSignature
          currentFact
          ("used send has no signature: " ++ currentSend)
      ]
    [_] ->
      []
    _ ->
      [ migrationIssue
          MigrationBlocker
          AmbiguousSendSignature
          currentFact
          ("used send has multiple signatures: " ++ currentSend)
      ]

handlerUseIssues ::
  LegacyEffectDescriptor ->
  String ->
  String ->
  [MigrationIssue]
handlerUseIssues currentDescriptor currentFact currentSend =
  case handlersNamed currentDescriptor currentSend of
    [] ->
      [ migrationIssue
          MigrationWarning
          MissingHandlerResolution
          currentFact
          ("send requires an explicit Handler registration: " ++ currentSend)
      ]
    [_] ->
      []
    _ ->
      [ migrationIssue
          MigrationBlocker
          AmbiguousHandlerResolution
          currentFact
          ("send has multiple Handler declarations: " ++ currentSend)
      ]

factTakeIssues ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  [MigrationIssue]
factTakeIssues currentDescriptor currentFact =
  case legacyFactTakes currentFact of
    [] ->
      []
    [currentSchema] ->
      case artifactSources currentDescriptor currentSchema of
        [] ->
          [ migrationIssue
              MigrationBlocker
              MissingArtifactProducer
              currentName
              ("take has no local observation or external R source: " ++ currentSchema)
          ]
        [LocalArtifactSource currentProducer] ->
          [ migrationIssue
              MigrationBlocker
              ArtifactProducerNotReadable
              currentName
              ( "artifact producer cannot expose the required observation R: "
                  ++ legacyFactName currentProducer
                  ++ " -> "
                  ++ currentSchema
              )
          | plannedObservationRead currentDescriptor currentProducer == Nothing
          ]
        [_] ->
          []
        currentSources ->
          [ migrationIssue
              MigrationBlocker
              AmbiguousArtifactConsumer
              currentName
              ( "take has multiple possible artifact sources for "
                  ++ currentSchema
                  ++ ": "
                  ++ show (map artifactSourceName currentSources)
              )
          ]
    currentSchemas ->
      [ migrationIssue
          MigrationBlocker
          MultipleTakesRequireClosedRecord
          currentName
          ( "multiple takes require an explicit closed RExpr record: "
              ++ show currentSchemas
          )
      ]
  where
    currentName =
      legacyFactName currentFact

implementationArgumentIssues ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  LegacySendDescriptor ->
  [MigrationIssue]
implementationArgumentIssues currentDescriptor currentFact currentSend =
  case resolveInitialArgument currentDescriptor currentFact currentSend of
    Left currentIssue ->
      [currentIssue]
    Right currentInitial ->
      case
        applyTransformChain
          currentDescriptor
          currentFact
          currentInitial of
        Left currentIssue ->
          [currentIssue]
        Right (_, currentSchema)
          | currentSchema /= normalizeLegacySchema (legacySendInputSchema currentSend) ->
              [ migrationIssue
                  MigrationBlocker
                  ImplementationArgumentSchemaMismatch
                  (legacyFactName currentFact)
                  ( "RExpr result does not match send input: result="
                      ++ currentSchema
                      ++ ", sendInput="
                      ++ normalizeLegacySchema (legacySendInputSchema currentSend)
                  )
              ]
          | otherwise ->
              []

factMakeIssues ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  [MigrationIssue]
factMakeIssues currentDescriptor currentFact =
  case legacyFactMakes currentFact of
    [] ->
      []
    [currentSchema] ->
      case legacyFactUses currentFact of
        [currentSend] ->
          case sendsNamed currentDescriptor currentSend of
            [currentSignature]
              | normalizeLegacySchema (legacySendOutputSchema currentSignature)
                  == normalizeLegacySchema currentSchema ->
                  []
              | otherwise ->
                  [ migrationIssue
                      MigrationBlocker
                      ObservationSchemaMismatch
                      currentName
                      ( "make schema does not match command observation: make="
                          ++ currentSchema
                          ++ ", sendOutput="
                          ++ normalizeLegacySchema
                            (legacySendOutputSchema currentSignature)
                      )
                  ]
            _ ->
              [ambiguousObservationIssue currentName]
        _ ->
          [ambiguousObservationIssue currentName]
    currentSchemas ->
      [ migrationIssue
          MigrationBlocker
          MultipleMakesAmbiguous
          currentName
          ( "multiple makes do not define one command observation/R contract: "
              ++ show currentSchemas
          )
      ]
  where
    currentName =
      legacyFactName currentFact
    ambiguousObservationIssue currentSubject =
      migrationIssue
        MigrationBlocker
        ObservationSourceAmbiguous
        currentSubject
        "make requires exactly one used send with exactly one signature"

globalIssue ::
  MigrationSeverity ->
  MigrationIssueCode ->
  String ->
  String ->
  MigrationIssue
globalIssue =
  migrationIssue

visibilityDescriptorIssues ::
  LegacyVisibilityDescriptor ->
  [String] ->
  [MigrationIssue]
visibilityDescriptorIssues currentVisibility currentFacts =
  mappingIssues ++ conflictIssues ++ unresolvedExportIssues
  where
    currentImports =
      legacyVisibilityImports currentVisibility
    currentPrivate =
      legacyVisibilityPrivateFacts currentVisibility
    currentExports =
      legacyVisibilityExports currentVisibility
    mappingIssues =
      [ globalIssue
          MigrationWarning
          VisibilityRequiresManualMapping
          "visibility"
          "imports/private/exports require an explicit CURDE visibility review"
      | not
          ( null currentImports
              && null currentPrivate
              && null currentExports
          )
      ]
    conflictIssues =
      [ visibilityConflict "private and exported" currentName
      | currentName <- listIntersection currentPrivate currentExports
      ]
        ++ [ visibilityConflict "private and imported" currentName
           | currentName <- listIntersection currentPrivate currentImports
           ]
        ++ [ visibilityConflict "produced and imported" currentName
           | currentName <- listIntersection currentFacts currentImports
           ]
    unresolvedExportIssues =
      [ globalIssue
          MigrationWarning
          VisibilityRequiresManualMapping
          currentExport
          "export has no local producer; provider identity requires manual mapping"
      | currentExport <- currentExports
      , currentExport `notElem` currentFacts
      , currentExport `notElem` currentImports
      ]
    visibilityConflict currentConflict currentSubject =
      globalIssue
        MigrationBlocker
        VisibilityConflict
        currentSubject
        ("legacy visibility conflict: " ++ currentConflict)

resolveImplementationArgument ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  LegacySendDescriptor ->
  Maybe (PlannedRExpr, String)
resolveImplementationArgument currentDescriptor currentFact currentSend =
  case resolveInitialArgument currentDescriptor currentFact currentSend of
    Left _ ->
      Nothing
    Right currentInitial ->
      case applyTransformChain currentDescriptor currentFact currentInitial of
        Left _ ->
          Nothing
        Right currentResult@(_, currentSchema)
          | currentSchema
              == normalizeLegacySchema (legacySendInputSchema currentSend) ->
              Just currentResult
          | otherwise ->
              Nothing

resolveInitialArgument ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  LegacySendDescriptor ->
  Either MigrationIssue (PlannedRExpr, String)
resolveInitialArgument currentDescriptor currentFact currentSend =
  case legacyFactTakes currentFact of
    [] ->
      resolveUnitStart
    [currentSchema] ->
      case resolvePlannedRead currentDescriptor currentSchema of
        Just currentRead ->
          Right
            ( PlannedRReference currentRead
            , normalizeLegacySchema currentSchema
            )
        Nothing ->
          Left
            ( migrationIssue
                MigrationBlocker
                MissingImplementationArgument
                currentName
                ("take cannot resolve one R source: " ++ currentSchema)
            )
    currentSchemas ->
      Left
        ( migrationIssue
            MigrationBlocker
            MultipleTakesRequireClosedRecord
            currentName
            ("multiple takes require a closed record: " ++ show currentSchemas)
        )
  where
    currentName =
      legacyFactName currentFact
    resolveUnitStart =
      case legacyFactTransforms currentFact of
        [] ->
          if isUnitSchema (legacySendInputSchema currentSend)
            then Right (PlannedUnitLiteral "Unit", "Unit")
            else missingArgument
        firstTransform : _ ->
          case transformsNamed currentDescriptor firstTransform of
            [] ->
              Left
                ( migrationIssue
                    MigrationBlocker
                    MissingTransformSignature
                    currentName
                    ("transform has no signature: " ++ firstTransform)
                )
            [currentTransform]
              | isUnitSchema (legacyTransformInputSchema currentTransform) ->
                  Right (PlannedUnitLiteral "Unit", "Unit")
              | otherwise ->
                  missingArgument
            _ ->
              Left
                ( migrationIssue
                    MigrationBlocker
                    AmbiguousTransformSignature
                    currentName
                    ("transform has multiple signatures: " ++ firstTransform)
                )
    missingArgument =
      Left
        ( migrationIssue
            MigrationBlocker
            MissingImplementationArgument
            currentName
            ( "send requires an RExpr argument with schema "
                ++ normalizeLegacySchema (legacySendInputSchema currentSend)
            )
        )

applyTransformChain ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  (PlannedRExpr, String) ->
  Either MigrationIssue (PlannedRExpr, String)
applyTransformChain currentDescriptor currentFact =
  applyAll (legacyFactTransforms currentFact)
  where
    currentName =
      legacyFactName currentFact
    applyAll [] currentValue =
      Right currentValue
    applyAll
      (currentTransformName : remaining)
      (currentExpression, currentSchema) =
        case transformsNamed currentDescriptor currentTransformName of
          [] ->
            Left
              ( migrationIssue
                  MigrationBlocker
                  MissingTransformSignature
                  currentName
                  ("transform has no signature: " ++ currentTransformName)
              )
          [currentTransform]
            | normalizedInput /= currentSchema ->
                Left
                  ( migrationIssue
                      MigrationBlocker
                      TransformChainSchemaMismatch
                      currentName
                      ( "operator input does not match previous RExpr result: operator="
                          ++ currentTransformName
                          ++ ", expected="
                          ++ normalizedInput
                          ++ ", actual="
                          ++ currentSchema
                      )
                  )
            | otherwise ->
                applyAll
                  remaining
                  ( PlannedOperatorApplication
                      currentTransformName
                      normalizedOutput
                      currentExpression
                  , normalizedOutput
                  )
            where
              normalizedInput =
                normalizeLegacySchema
                  (legacyTransformInputSchema currentTransform)
              normalizedOutput =
                normalizeLegacySchema
                  (legacyTransformOutputSchema currentTransform)
          _ ->
            Left
              ( migrationIssue
                  MigrationBlocker
                  AmbiguousTransformSignature
                  currentName
                  ("transform has multiple signatures: " ++ currentTransformName)
              )

plannedObservationFor ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  Int ->
  LegacySendDescriptor ->
  PlannedObservation
plannedObservationFor
  currentDescriptor
  currentFact
  currentOccurrence
  currentSignature
  | makeIdentifiesObservation || observationHasRConsumer =
      PlannedCaptureObservation currentOutput
  | otherwise =
      PlannedDiscardObservation
  where
    currentOutput =
      normalizeLegacySchema (legacySendOutputSchema currentSignature)
    makeIdentifiesObservation =
      map normalizeLegacySchema (legacyFactMakes currentFact) == [currentOutput]
        && matchingOccurrences == [currentOccurrence]
    matchingOccurrences =
      [ currentIndex
      | (currentIndex, currentSend) <-
          indexedItems (legacyFactUses currentFact)
      , [currentSendSignature] <-
          [sendsNamed currentDescriptor currentSend]
      , normalizeLegacySchema
          (legacySendOutputSchema currentSendSignature)
          == currentOutput
      ]
    observationHasRConsumer =
      any
        (elem currentOutput . map normalizeLegacySchema . legacyFactTakes)
        (legacyEffectFacts currentDescriptor)
        && commandOutputOccurrences currentDescriptor currentOutput
          == [(currentFact, currentOccurrence)]

commandOutputOccurrences ::
  LegacyEffectDescriptor ->
  String ->
  [(LegacyFactDescriptor, Int)]
commandOutputOccurrences currentDescriptor currentSchema =
  [ (currentFact, currentOccurrence)
  | currentFact <- legacyEffectFacts currentDescriptor
  , (currentOccurrence, currentSend) <-
      indexedItems (legacyFactUses currentFact)
  , [currentSignature] <-
      [sendsNamed currentDescriptor currentSend]
  , normalizeLegacySchema (legacySendOutputSchema currentSignature)
      == normalizeLegacySchema currentSchema
  ]

plannedObservationRead ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  Maybe PlannedRead
plannedObservationRead currentDescriptor currentFact =
  case legacyFactUses currentFact of
    [currentSend] ->
      case sendsNamed currentDescriptor currentSend of
        [currentSignature] ->
          case
            plannedObservationFor
              currentDescriptor
              currentFact
              0
              currentSignature of
            PlannedDiscardObservation ->
              Nothing
            PlannedCaptureObservation currentSchema ->
              Just
                PlannedRead
                  { plannedReadName =
                      observationReadName
                        (legacyFactName currentFact)
                        currentSchema
                  , plannedReadSchema = currentSchema
                  , plannedReadSource =
                      PlannedCommandObservation (legacyFactName currentFact)
                  }
        _ ->
          Nothing
    _ ->
      Nothing

resolvePlannedRead ::
  LegacyEffectDescriptor ->
  String ->
  Maybe PlannedRead
resolvePlannedRead currentDescriptor currentSchema =
  case artifactSources currentDescriptor normalizedSchema of
    [LocalArtifactSource currentProducer] ->
      plannedObservationRead currentDescriptor currentProducer
    [ExternalArtifactSource currentTake] ->
      Just (plannedExternalRead currentTake normalizedSchema)
    _ ->
      Nothing
  where
    normalizedSchema =
      normalizeLegacySchema currentSchema

plannedExternalRead ::
  LegacyExternalTakeDescriptor ->
  String ->
  PlannedRead
plannedExternalRead currentTake currentSchema =
  PlannedRead
    { plannedReadName =
        externalReadName
          (legacyExternalTakeFact currentTake)
          normalizedSchema
    , plannedReadSchema = normalizedSchema
    , plannedReadSource =
        PlannedExternalTake (legacyExternalTakeFact currentTake)
    }
  where
    normalizedSchema =
      normalizeLegacySchema currentSchema

data ArtifactSource
  = LocalArtifactSource LegacyFactDescriptor
  | ExternalArtifactSource LegacyExternalTakeDescriptor

artifactSources ::
  LegacyEffectDescriptor ->
  String ->
  [ArtifactSource]
artifactSources currentDescriptor currentSchema =
  map LocalArtifactSource localSources
    ++ map ExternalArtifactSource externalSources
  where
    normalizedSchema =
      normalizeLegacySchema currentSchema
    localSources =
      stableUnique
        [ currentFact
        | currentFact <- legacyEffectFacts currentDescriptor
        , factCanProduceSchema currentDescriptor currentFact normalizedSchema
        ]
    externalSources =
      externalTakesForSchema currentDescriptor normalizedSchema

factCanProduceSchema ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  String ->
  Bool
factCanProduceSchema currentDescriptor currentFact currentSchema =
  normalizedSchema
    `elem` map normalizeLegacySchema (legacyFactMakes currentFact)
    || commandOutputSchema currentDescriptor currentFact
      == Just normalizedSchema
  where
    normalizedSchema =
      normalizeLegacySchema currentSchema

commandOutputSchema ::
  LegacyEffectDescriptor ->
  LegacyFactDescriptor ->
  Maybe String
commandOutputSchema currentDescriptor currentFact =
  case legacyFactUses currentFact of
    [currentSend] ->
      case sendsNamed currentDescriptor currentSend of
        [currentSignature] ->
          Just
            (normalizeLegacySchema (legacySendOutputSchema currentSignature))
        _ ->
          Nothing
    _ ->
      Nothing

artifactSourceNames ::
  LegacyEffectDescriptor ->
  String ->
  [String]
artifactSourceNames currentDescriptor currentSchema =
  sort
    ( map artifactSourceName
        (artifactSources currentDescriptor currentSchema)
    )

artifactSourceName :: ArtifactSource -> String
artifactSourceName currentSource =
  case currentSource of
    LocalArtifactSource currentFact ->
      "fact:" ++ legacyFactName currentFact
    ExternalArtifactSource currentTake ->
      "external-take:" ++ legacyExternalTakeFact currentTake

artifactProducerNames ::
  LegacyEffectDescriptor ->
  String ->
  [String]
artifactProducerNames currentDescriptor currentSchema =
  uniqueSorted
    [ legacyFactName currentFact
    | currentFact <- legacyEffectFacts currentDescriptor
    , normalizeLegacySchema currentSchema
        `elem` map normalizeLegacySchema (legacyFactMakes currentFact)
    ]

allMadeSchemas :: LegacyEffectDescriptor -> [String]
allMadeSchemas currentDescriptor =
  uniqueSorted
    [ normalizeLegacySchema currentSchema
    | currentFact <- legacyEffectFacts currentDescriptor
    , currentSchema <- legacyFactMakes currentFact
    ]

factsNamed ::
  LegacyEffectDescriptor ->
  String ->
  [LegacyFactDescriptor]
factsNamed currentDescriptor currentName =
  [ currentFact
  | currentFact <- legacyEffectFacts currentDescriptor
  , legacyFactName currentFact == currentName
  ]

sendsNamed ::
  LegacyEffectDescriptor ->
  String ->
  [LegacySendDescriptor]
sendsNamed currentDescriptor currentName =
  [ currentSend
  | currentSend <- legacyEffectSends currentDescriptor
  , legacySendName currentSend == currentName
  ]

handlersNamed ::
  LegacyEffectDescriptor ->
  String ->
  [LegacyHandlerDescriptor]
handlersNamed currentDescriptor currentName =
  [ currentHandler
  | currentHandler <- legacyEffectHandlers currentDescriptor
  , legacyHandlerSend currentHandler == currentName
  ]

transformsNamed ::
  LegacyEffectDescriptor ->
  String ->
  [LegacyTransformDescriptor]
transformsNamed currentDescriptor currentName =
  [ currentTransform
  | currentTransform <- legacyEffectTransforms currentDescriptor
  , legacyTransformName currentTransform == currentName
  ]

externalTakesForSchema ::
  LegacyEffectDescriptor ->
  String ->
  [LegacyExternalTakeDescriptor]
externalTakesForSchema currentDescriptor currentSchema =
  [ currentTake
  | currentTake <- legacyEffectExternalTakes currentDescriptor
  , fmap normalizeLegacySchema (legacyExternalTakeOutputSchema currentTake)
      == Just (normalizeLegacySchema currentSchema)
  ]

migrationIssue ::
  MigrationSeverity ->
  MigrationIssueCode ->
  String ->
  String ->
  MigrationIssue
migrationIssue currentSeverity currentCode currentSubject currentDetail =
  MigrationIssue
    { migrationIssueCode = currentCode
    , migrationIssueSeverity = currentSeverity
    , migrationIssueSubject = currentSubject
    , migrationIssueDetail = currentDetail
    }

observationReadName :: String -> String -> String
observationReadName currentFact currentSchema =
  "migration-r:observation:"
    ++ encodeSegment currentFact
    ++ encodeSegment currentSchema

externalReadName :: String -> String -> String
externalReadName currentFact currentSchema =
  "migration-r:external:"
    ++ encodeSegment currentFact
    ++ encodeSegment currentSchema

atomicHandleName :: String -> Int -> String -> String
atomicHandleName currentFact currentOccurrence currentSend =
  "migration-e:atomic:"
    ++ encodeSegment currentFact
    ++ encodeSegment (show currentOccurrence)
    ++ encodeSegment currentSend

encodeSegment :: String -> String
encodeSegment currentValue =
  show (length currentValue) ++ ":" ++ currentValue

-- | Normalize legacy no-argument spellings in plans and comparisons only.
-- The source descriptor remains byte-for-byte semantically unchanged.
normalizeLegacySchema :: String -> String
normalizeLegacySchema currentSchema
  | currentSchema == "NoInput" = "Unit"
  | currentSchema == "()" = "Unit"
  | otherwise = currentSchema

isUnitSchema :: String -> Bool
isUnitSchema =
  (== "Unit") . normalizeLegacySchema

firstItem :: [item] -> Maybe item
firstItem currentItems =
  case currentItems of
    [] -> Nothing
    currentItem : _ -> Just currentItem

duplicates :: Ord item => [item] -> [item]
duplicates currentItems =
  [ currentItem
  | currentItem : _ : _ <- group (sort currentItems)
  ]

stableUnique :: Ord item => [item] -> [item]
stableUnique currentItems =
  reverse (snd (foldl appendStable (Set.empty, []) currentItems))
  where
    appendStable (currentSeen, currentResult) currentItem
      | currentItem `Set.member` currentSeen =
          (currentSeen, currentResult)
      | otherwise =
          ( Set.insert currentItem currentSeen
          , currentItem : currentResult
          )

uniqueSorted :: Ord item => [item] -> [item]
uniqueSorted =
  Set.toAscList . Set.fromList

indexedItems :: [item] -> [(Int, item)]
indexedItems =
  zip [0 ..]

listIntersection :: Ord item => [item] -> [item] -> [item]
listIntersection left right =
  Set.toAscList
    (Set.intersection (Set.fromList left) (Set.fromList right))
