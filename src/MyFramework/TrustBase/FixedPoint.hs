module MyFramework.TrustBase.FixedPoint
  ( EvidenceDiff (..)
  , FixedPointReport (..)
  , FixedPointStatus (..)
  , StageDiffKey (..)
  , StageEvidence (..)
  , StageValue (..)
  , buildFixedPointReport
  , canonicalStageEvidence
  , diffStageEvidence
  , fixedPointDiffClaimName
  , fixedPointDiffKeyName
  , fixedPointDiffKeys
  , fixedPointPassed
  , fixedPointReportSchemaV1
  , fixedPointSummarySchemaV1
  , stageValue
  ) where

import Data.List
  ( sort
  )

import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , EvidenceStatus (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  )

data StageEvidence = StageEvidence
  { stageEvidenceName :: String
  , stageEvidenceStatus :: EvidenceStatus
  , stageEvidenceSurfaceModules :: Int
  , stageEvidenceSurfaceCapabilities :: Int
  , stageEvidenceConstraintTotal :: Int
  , stageEvidenceConstraintFailed :: Int
  , stageEvidenceDeclaredFacts :: [String]
  , stageEvidenceRootFacts :: [String]
  , stageEvidencePlannedRuntimeFacts :: [String]
  , stageEvidenceFinalRuntimeFacts :: [String]
  , stageEvidenceMissingFinalFacts :: [String]
  , stageEvidenceExtraFinalFacts :: [String]
  , stageEvidenceHandlerCoverage :: [String]
  , stageEvidenceArtifactTypes :: [String]
  , stageEvidenceFailures :: [String]
  }
  deriving (Eq, Show)

-- | These constructors and their order are the stable fixed-point diff schema.
-- Stage names are provenance only and intentionally do not appear here.
data StageDiffKey
  = StageStatusKey
  | SurfaceModulesKey
  | SurfaceCapabilitiesKey
  | ConstraintTotalKey
  | ConstraintFailedKey
  | DeclaredFactsKey
  | RootFactsKey
  | PlannedRuntimeFactsKey
  | FinalRuntimeFactsKey
  | MissingFinalFactsKey
  | ExtraFinalFactsKey
  | HandlerCoverageKey
  | ArtifactTypesKey
  | FailuresKey
  deriving (Eq, Ord, Show)

data StageValue
  = StageStatusValue EvidenceStatus
  | StageCountValue Int
  | StageNamesValue [String]
  deriving (Eq, Show)

data EvidenceDiff = EvidenceDiff
  { evidenceDiffKey :: StageDiffKey
  , evidenceDiffStage0 :: StageValue
  , evidenceDiffStage1 :: StageValue
  }
  deriving (Eq, Show)

data FixedPointReport = FixedPointReport
  { fixedPointStatus :: FixedPointStatus
  , fixedPointStage0 :: StageEvidence
  , fixedPointStage1 :: StageEvidence
  , fixedPointDiffs :: [EvidenceDiff]
  }
  deriving (Eq, Show)

data FixedPointStatus
  = FixedPointPassed
  | FixedPointFailed
  deriving (Eq, Show)

fixedPointReportSchemaV1 :: SchemaId
fixedPointReportSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "fixed-point-report"
    , schemaIdVersion = SchemaVersion 1
    }

fixedPointSummarySchemaV1 :: SchemaId
fixedPointSummarySchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "fixed-point-summary"
    , schemaIdVersion = SchemaVersion 1
    }

fixedPointDiffKeys :: [StageDiffKey]
fixedPointDiffKeys =
  [ StageStatusKey
  , SurfaceModulesKey
  , SurfaceCapabilitiesKey
  , ConstraintTotalKey
  , ConstraintFailedKey
  , DeclaredFactsKey
  , RootFactsKey
  , PlannedRuntimeFactsKey
  , FinalRuntimeFactsKey
  , MissingFinalFactsKey
  , ExtraFinalFactsKey
  , HandlerCoverageKey
  , ArtifactTypesKey
  , FailuresKey
  ]

fixedPointDiffKeyName :: StageDiffKey -> String
fixedPointDiffKeyName key =
  case key of
    StageStatusKey -> "status"
    SurfaceModulesKey -> "surface modules"
    SurfaceCapabilitiesKey -> "surface capabilities"
    ConstraintTotalKey -> "constraint total"
    ConstraintFailedKey -> "constraint failed"
    DeclaredFactsKey -> "declared facts"
    RootFactsKey -> "root facts"
    PlannedRuntimeFactsKey -> "planned runtime facts"
    FinalRuntimeFactsKey -> "final runtime facts"
    MissingFinalFactsKey -> "missing final facts"
    ExtraFinalFactsKey -> "extra final facts"
    HandlerCoverageKey -> "handler coverage"
    ArtifactTypesKey -> "artifact types"
    FailuresKey -> "failures"

fixedPointDiffClaimName :: StageDiffKey -> ClaimName
fixedPointDiffClaimName key =
  ClaimName
    ( case key of
        StageStatusKey -> "fixed-point-diff-status"
        SurfaceModulesKey -> "fixed-point-diff-surface-modules"
        SurfaceCapabilitiesKey -> "fixed-point-diff-surface-capabilities"
        ConstraintTotalKey -> "fixed-point-diff-constraint-total"
        ConstraintFailedKey -> "fixed-point-diff-constraint-failed"
        DeclaredFactsKey -> "fixed-point-diff-declared-facts"
        RootFactsKey -> "fixed-point-diff-root-facts"
        PlannedRuntimeFactsKey -> "fixed-point-diff-planned-runtime-facts"
        FinalRuntimeFactsKey -> "fixed-point-diff-final-runtime-facts"
        MissingFinalFactsKey -> "fixed-point-diff-missing-final-facts"
        ExtraFinalFactsKey -> "fixed-point-diff-extra-final-facts"
        HandlerCoverageKey -> "fixed-point-diff-handler-coverage"
        ArtifactTypesKey -> "fixed-point-diff-artifact-types"
        FailuresKey -> "fixed-point-diff-failures"
    )

canonicalStageEvidence :: StageEvidence -> StageEvidence
canonicalStageEvidence evidence =
  evidence
    { stageEvidenceDeclaredFacts =
        sort (stageEvidenceDeclaredFacts evidence)
    , stageEvidenceRootFacts =
        sort (stageEvidenceRootFacts evidence)
    , stageEvidencePlannedRuntimeFacts =
        sort (stageEvidencePlannedRuntimeFacts evidence)
    , stageEvidenceFinalRuntimeFacts =
        sort (stageEvidenceFinalRuntimeFacts evidence)
    , stageEvidenceMissingFinalFacts =
        sort (stageEvidenceMissingFinalFacts evidence)
    , stageEvidenceExtraFinalFacts =
        sort (stageEvidenceExtraFinalFacts evidence)
    , stageEvidenceHandlerCoverage =
        sort (stageEvidenceHandlerCoverage evidence)
    , stageEvidenceArtifactTypes =
        sort (stageEvidenceArtifactTypes evidence)
    , stageEvidenceFailures =
        sort (stageEvidenceFailures evidence)
    }

stageValue :: StageDiffKey -> StageEvidence -> StageValue
stageValue key evidence =
  case key of
    StageStatusKey ->
      StageStatusValue (stageEvidenceStatus evidence)
    SurfaceModulesKey ->
      StageCountValue (stageEvidenceSurfaceModules evidence)
    SurfaceCapabilitiesKey ->
      StageCountValue (stageEvidenceSurfaceCapabilities evidence)
    ConstraintTotalKey ->
      StageCountValue (stageEvidenceConstraintTotal evidence)
    ConstraintFailedKey ->
      StageCountValue (stageEvidenceConstraintFailed evidence)
    DeclaredFactsKey ->
      StageNamesValue (stageEvidenceDeclaredFacts evidence)
    RootFactsKey ->
      StageNamesValue (stageEvidenceRootFacts evidence)
    PlannedRuntimeFactsKey ->
      StageNamesValue (stageEvidencePlannedRuntimeFacts evidence)
    FinalRuntimeFactsKey ->
      StageNamesValue (stageEvidenceFinalRuntimeFacts evidence)
    MissingFinalFactsKey ->
      StageNamesValue (stageEvidenceMissingFinalFacts evidence)
    ExtraFinalFactsKey ->
      StageNamesValue (stageEvidenceExtraFinalFacts evidence)
    HandlerCoverageKey ->
      StageNamesValue (stageEvidenceHandlerCoverage evidence)
    ArtifactTypesKey ->
      StageNamesValue (stageEvidenceArtifactTypes evidence)
    FailuresKey ->
      StageNamesValue (stageEvidenceFailures evidence)

diffStageEvidence :: StageEvidence -> StageEvidence -> [EvidenceDiff]
diffStageEvidence stage0 stage1 =
  [ EvidenceDiff
      { evidenceDiffKey = key
      , evidenceDiffStage0 = left
      , evidenceDiffStage1 = right
      }
  | key <- fixedPointDiffKeys
  , let left = stageValue key canonicalStage0
  , let right = stageValue key canonicalStage1
  , left /= right
  ]
  where
    canonicalStage0 =
      canonicalStageEvidence stage0
    canonicalStage1 =
      canonicalStageEvidence stage1

buildFixedPointReport :: StageEvidence -> StageEvidence -> FixedPointReport
buildFixedPointReport stage0 stage1 =
  FixedPointReport
    { fixedPointStatus = status
    , fixedPointStage0 = canonicalStage0
    , fixedPointStage1 = canonicalStage1
    , fixedPointDiffs = diffs
    }
  where
    canonicalStage0 =
      canonicalStageEvidence stage0
    canonicalStage1 =
      canonicalStageEvidence stage1
    diffs =
      diffStageEvidence canonicalStage0 canonicalStage1
    status
      | stageEvidenceStatus canonicalStage0 == EvidencePassed
          && stageEvidenceStatus canonicalStage1 == EvidencePassed
          && null diffs =
          FixedPointPassed
      | otherwise =
          FixedPointFailed

fixedPointPassed :: FixedPointReport -> Bool
fixedPointPassed report =
  fixedPointStatus report == FixedPointPassed
    && stageEvidenceStatus (fixedPointStage0 report) == EvidencePassed
    && stageEvidenceStatus (fixedPointStage1 report) == EvidencePassed
    && null (fixedPointDiffs report)
