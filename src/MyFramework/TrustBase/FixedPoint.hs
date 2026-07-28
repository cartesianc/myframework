module MyFramework.TrustBase.FixedPoint
  ( EvidenceDiff (..)
  , FixedPointReport (..)
  , FixedPointStatus (..)
  , StageDiffKey (..)
  , StageEvidence
  , StageEvidenceCollectionError (..)
  , StageValue (..)
  , buildFixedPointReport
  , canonicalStageEvidence
  , collectStageEvidence
  , diffStageEvidence
  , fixedPointDiffClaimName
  , fixedPointDiffKeyName
  , fixedPointDiffKeys
  , fixedPointPassed
  , fixedPointReportSchemaV1
  , fixedPointSummarySchemaV1
  , renderStageEvidenceJson
  , stageEvidenceArtifactManifestDigest
  , stageEvidenceControlPlanDigest
  , stageEvidenceFacadeLoweringDigest
  , stageEvidenceFailures
  , stageEvidenceName
  , stageEvidenceRuntimeWitnessDigest
  , stageEvidenceSelfModelDigest
  , stageEvidenceSemanticWitnessDigest
  , stageEvidenceStatus
  , stageValue
  ) where

import Control.Exception
  ( IOException
  , try
  )
import Data.Char
  ( ord )
import Data.List
  ( intercalate
  , isInfixOf
  , sort
  )
import Numeric
  ( showHex )
import MyFramework.Self.Artifact
  ( ArtifactError
  , ArtifactManifest (..)
  , verifyArtifactManifest
  )
import MyFramework.Self.ControlTrace
  ( compileControlTrace
  , renderControlTraceJson
  )
import MyFramework.Self.Model
  ( canonicalSelfModelJson
  , encodeSelfModel
  , selfModel
  )
import MyFramework.TrustBase.Digest
  ( sha256 )
import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , EvidenceStatus (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  )

-- | The constructor is intentionally not exported. Evidence can only be
-- collected from the framework's actual SelfModel, witness files and verified
-- artifact manifest.
data StageEvidence = StageEvidence
  { stageEvidenceName :: String
  , stageEvidenceStatus :: EvidenceStatus
  , stageEvidenceSelfModelDigest :: String
  , stageEvidenceControlPlanDigest :: String
  , stageEvidenceFacadeLoweringDigest :: String
  , stageEvidenceSemanticWitnessDigest :: String
  , stageEvidenceRuntimeWitnessDigest :: String
  , stageEvidenceArtifactManifestDigest :: String
  , stageEvidenceFailures :: [String]
  }
  deriving (Eq, Show)

data StageEvidenceCollectionError
  = StageEvidenceSelfModelFailed String
  | StageEvidenceControlPlanFailed String
  | StageEvidenceReadFailed FilePath String
  | StageEvidenceSemanticWitnessInvalid FilePath
  | StageEvidenceRuntimeWitnessInvalid FilePath
  | StageEvidenceArtifactInvalid FilePath [ArtifactError]
  deriving (Eq, Show)

data StageDiffKey
  = StageStatusKey
  | SelfModelDigestKey
  | ControlPlanDigestKey
  | FacadeLoweringDigestKey
  | SemanticWitnessDigestKey
  | RuntimeWitnessDigestKey
  | ArtifactManifestDigestKey
  | FailuresKey
  deriving (Eq, Ord, Show)

data StageValue
  = StageStatusValue EvidenceStatus
  | StageDigestValue String
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
  , SelfModelDigestKey
  , ControlPlanDigestKey
  , FacadeLoweringDigestKey
  , SemanticWitnessDigestKey
  , RuntimeWitnessDigestKey
  , ArtifactManifestDigestKey
  , FailuresKey
  ]

fixedPointDiffKeyName :: StageDiffKey -> String
fixedPointDiffKeyName currentKey =
  case currentKey of
    StageStatusKey -> "status"
    SelfModelDigestKey -> "self model digest"
    ControlPlanDigestKey -> "control plan digest"
    FacadeLoweringDigestKey -> "facade lowering digest"
    SemanticWitnessDigestKey -> "semantic witness digest"
    RuntimeWitnessDigestKey -> "runtime witness digest"
    ArtifactManifestDigestKey -> "artifact manifest digest"
    FailuresKey -> "failures"

fixedPointDiffClaimName :: StageDiffKey -> ClaimName
fixedPointDiffClaimName currentKey =
  ClaimName
    ( case currentKey of
        StageStatusKey -> "fixed-point-diff-status"
        SelfModelDigestKey -> "fixed-point-diff-self-model"
        ControlPlanDigestKey -> "fixed-point-diff-control-plan"
        FacadeLoweringDigestKey -> "fixed-point-diff-facade-lowering"
        SemanticWitnessDigestKey -> "fixed-point-diff-semantic-witness"
        RuntimeWitnessDigestKey -> "fixed-point-diff-runtime-witness"
        ArtifactManifestDigestKey -> "fixed-point-diff-artifact-manifest"
        FailuresKey -> "fixed-point-diff-failures"
    )

collectStageEvidence ::
  String ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO (Either [StageEvidenceCollectionError] StageEvidence)
collectStageEvidence
  currentName
  semanticWitnessPath
  runtimeWitnessPath
  artifactRoot =
    case selfModel of
      Left currentErrors ->
        pure
          (Left [StageEvidenceSelfModelFailed (show currentErrors)])
      Right currentModel ->
        case compileControlTrace currentModel of
          Left currentError ->
            pure
              (Left [StageEvidenceControlPlanFailed (show currentError)])
          Right currentControlTrace -> do
            semanticResult <- readEvidenceFile semanticWitnessPath
            runtimeResult <- readEvidenceFile runtimeWitnessPath
            artifactResult <- verifyArtifactManifest artifactRoot
            let collectionErrors =
                  semanticErrors semanticWitnessPath semanticResult
                    ++ runtimeErrors runtimeWitnessPath runtimeResult
                    ++ artifactErrors artifactRoot artifactResult
            pure
              ( case
                    ( collectionErrors
                    , semanticResult
                    , runtimeResult
                    , artifactResult
                    ) of
                  ([], Right semanticText, Right runtimeText, Right currentManifest) ->
                    Right
                      StageEvidence
                        { stageEvidenceName = currentName
                        , stageEvidenceStatus = EvidencePassed
                        , stageEvidenceSelfModelDigest =
                            sha256 (encodeSelfModel currentModel)
                        , stageEvidenceControlPlanDigest =
                            sha256
                              (renderControlTraceJson currentControlTrace)
                        , stageEvidenceFacadeLoweringDigest =
                            sha256 (canonicalSelfModelJson currentModel)
                        , stageEvidenceSemanticWitnessDigest =
                            sha256 semanticText
                        , stageEvidenceRuntimeWitnessDigest =
                            sha256 runtimeText
                        , stageEvidenceArtifactManifestDigest =
                            artifactManifestPayloadDigest currentManifest
                        , stageEvidenceFailures = []
                        }
                  _ ->
                    Left collectionErrors
              )

readEvidenceFile ::
  FilePath ->
  IO (Either StageEvidenceCollectionError String)
readEvidenceFile currentPath = do
  currentResult <-
    try (readFile currentPath) :: IO (Either IOException String)
  pure
    ( case currentResult of
        Left currentError ->
          Left
            (StageEvidenceReadFailed currentPath (show currentError))
        Right currentText ->
          Right currentText
    )

semanticErrors ::
  FilePath ->
  Either StageEvidenceCollectionError String ->
  [StageEvidenceCollectionError]
semanticErrors currentPath currentResult =
  case currentResult of
    Left currentError ->
      [currentError]
    Right currentText
      | semanticWitnessValid currentText ->
          []
      | otherwise ->
          [StageEvidenceSemanticWitnessInvalid currentPath]

runtimeErrors ::
  FilePath ->
  Either StageEvidenceCollectionError String ->
  [StageEvidenceCollectionError]
runtimeErrors currentPath currentResult =
  case currentResult of
    Left currentError ->
      [currentError]
    Right currentText
      | runtimeWitnessValid currentText ->
          []
      | otherwise ->
          [StageEvidenceRuntimeWitnessInvalid currentPath]

artifactErrors ::
  FilePath ->
  Either [ArtifactError] ArtifactManifest ->
  [StageEvidenceCollectionError]
artifactErrors currentRoot currentResult =
  case currentResult of
    Left currentErrors ->
      [StageEvidenceArtifactInvalid currentRoot currentErrors]
    Right _ ->
      []

semanticWitnessValid :: String -> Bool
semanticWitnessValid currentText =
  all
    (`isInfixOf` currentText)
    [ "\"schema\":\"curde-semantics-evidence.v1\""
    , "\"artifact\":\"curde-semantics-witness\""
    , "\"result\":\"passed\""
    , "\"exact21Plus1\":true"
    ]
    && countSubstring "\"status\":\"established\"" currentText >= 19

runtimeWitnessValid :: String -> Bool
runtimeWitnessValid currentText =
  all
    (`isInfixOf` currentText)
    [ "\"schema\":\"curde-runtime-witness.v1\""
    , "\"artifact\":\"curde-runtime-witness\""
    , "\"result\":\"passed\""
    ]
    && countSubstring "\"passed\":true" currentText == 48
    && not ("\"passed\":false" `isInfixOf` currentText)

countSubstring :: String -> String -> Int
countSubstring currentNeedle currentHaystack
  | null currentNeedle =
      0
  | otherwise =
      go currentHaystack
  where
    go [] =
      0
    go remainingText@(_ : rest)
      | currentNeedle `isPrefixOfText` remainingText =
          1 + go (drop (length currentNeedle) remainingText)
      | otherwise =
          go rest

isPrefixOfText :: String -> String -> Bool
isPrefixOfText [] _ =
  True
isPrefixOfText _ [] =
  False
isPrefixOfText (left : leftRest) (right : rightRest) =
  left == right && isPrefixOfText leftRest rightRest

canonicalStageEvidence :: StageEvidence -> StageEvidence
canonicalStageEvidence currentEvidence =
  currentEvidence
    { stageEvidenceFailures =
        sort (stageEvidenceFailures currentEvidence)
    }

stageValue :: StageDiffKey -> StageEvidence -> StageValue
stageValue currentKey currentEvidence =
  case currentKey of
    StageStatusKey ->
      StageStatusValue (stageEvidenceStatus currentEvidence)
    SelfModelDigestKey ->
      StageDigestValue (stageEvidenceSelfModelDigest currentEvidence)
    ControlPlanDigestKey ->
      StageDigestValue (stageEvidenceControlPlanDigest currentEvidence)
    FacadeLoweringDigestKey ->
      StageDigestValue (stageEvidenceFacadeLoweringDigest currentEvidence)
    SemanticWitnessDigestKey ->
      StageDigestValue (stageEvidenceSemanticWitnessDigest currentEvidence)
    RuntimeWitnessDigestKey ->
      StageDigestValue (stageEvidenceRuntimeWitnessDigest currentEvidence)
    ArtifactManifestDigestKey ->
      StageDigestValue (stageEvidenceArtifactManifestDigest currentEvidence)
    FailuresKey ->
      StageNamesValue (stageEvidenceFailures currentEvidence)

diffStageEvidence :: StageEvidence -> StageEvidence -> [EvidenceDiff]
diffStageEvidence stage0 stage1 =
  [ EvidenceDiff
      { evidenceDiffKey = currentKey
      , evidenceDiffStage0 = leftValue
      , evidenceDiffStage1 = rightValue
      }
  | currentKey <- fixedPointDiffKeys
  , let leftValue = stageValue currentKey canonicalStage0
  , let rightValue = stageValue currentKey canonicalStage1
  , leftValue /= rightValue
  ]
  where
    canonicalStage0 =
      canonicalStageEvidence stage0
    canonicalStage1 =
      canonicalStageEvidence stage1

buildFixedPointReport :: StageEvidence -> StageEvidence -> FixedPointReport
buildFixedPointReport stage0 stage1 =
  FixedPointReport
    { fixedPointStatus = currentStatus
    , fixedPointStage0 = canonicalStage0
    , fixedPointStage1 = canonicalStage1
    , fixedPointDiffs = currentDiffs
    }
  where
    canonicalStage0 =
      canonicalStageEvidence stage0
    canonicalStage1 =
      canonicalStageEvidence stage1
    currentDiffs =
      diffStageEvidence canonicalStage0 canonicalStage1
    currentStatus
      | stageEvidenceStatus canonicalStage0 == EvidencePassed
          && stageEvidenceStatus canonicalStage1 == EvidencePassed
          && null currentDiffs =
          FixedPointPassed
      | otherwise =
          FixedPointFailed

fixedPointPassed :: FixedPointReport -> Bool
fixedPointPassed currentReport =
  fixedPointStatus currentReport == FixedPointPassed
    && stageEvidenceStatus (fixedPointStage0 currentReport)
      == EvidencePassed
    && stageEvidenceStatus (fixedPointStage1 currentReport)
      == EvidencePassed
    && null (fixedPointDiffs currentReport)

renderStageEvidenceJson :: StageEvidence -> String
renderStageEvidenceJson currentEvidence =
  jsonObject
    [ ("name", jsonString (stageEvidenceName currentEvidence))
    , ( "status"
      , jsonString
          ( case stageEvidenceStatus currentEvidence of
              EvidencePassed -> "passed"
              EvidenceFailed -> "failed"
          )
      )
    , ( "selfModelDigest"
      , jsonString (stageEvidenceSelfModelDigest currentEvidence)
      )
    , ( "controlPlanDigest"
      , jsonString (stageEvidenceControlPlanDigest currentEvidence)
      )
    , ( "facadeLoweringDigest"
      , jsonString (stageEvidenceFacadeLoweringDigest currentEvidence)
      )
    , ( "semanticWitnessDigest"
      , jsonString (stageEvidenceSemanticWitnessDigest currentEvidence)
      )
    , ( "runtimeWitnessDigest"
      , jsonString (stageEvidenceRuntimeWitnessDigest currentEvidence)
      )
    , ( "artifactManifestDigest"
      , jsonString (stageEvidenceArtifactManifestDigest currentEvidence)
      )
    , ("failures", jsonArray (map jsonString (stageEvidenceFailures currentEvidence)))
    ]

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

jsonString :: String -> String
jsonString currentValue =
  "\"" ++ concatMap escapeJsonChar currentValue ++ "\""

escapeJsonChar :: Char -> String
escapeJsonChar currentChar =
  case currentChar of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _
      | ord currentChar < 0x20 ->
          "\\u" ++ padLeft 4 '0' (showHex (ord currentChar) "")
      | otherwise ->
          [currentChar]

padLeft :: Int -> Char -> String -> String
padLeft currentWidth currentFill currentValue =
  replicate (max 0 (currentWidth - length currentValue)) currentFill
    ++ currentValue
