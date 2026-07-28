module MyFramework.TrustBase.Promotion
  ( CoreManifest (..)
  , CurrentCorePointer (..)
  , PromotionDecision (..)
  , PromotionEvidence
  , PromotionRecord (..)
  , PromotionViolation (..)
  , approvePromotion
  , buildCoreManifest
  , collectPromotionEvidence
  , coreManifestTrustBaseRef
  , decodeCoreManifest
  , decodeCurrentCorePointer
  , decodePromotionRecord
  , preparePromotionRecord
  , promotionEvidenceArtifactDigest
  , promotionEvidenceEmptyBusinessDigest
  , promotionEvidenceSemanticDigest
  , renderCoreManifestJson
  , renderCurrentCorePointerJson
  , renderPromotionRecordJson
  , validateCoreManifest
  , validateCurrentCorePointer
  , validatePromotionRecord
  ) where

import Data.Char
  ( ord )
import Data.List
  ( intercalate
  , isInfixOf
  )
import Numeric
  ( showHex )
import Text.Read
  ( readMaybe )

import MyFramework.TrustBase.Core
  ( CoreId (..)
  , Digest (..)
  , TrustBaseRef (..)
  , TrustBaseRefViolation
  , validateTrustBaseRef
  )
import MyFramework.TrustBase.Digest
  ( sha256 )
import MyFramework.TrustBase.Json
  ( JsonValue (..)
  , jsonArrayItems
  , jsonObjectField
  , jsonStringValue
  , parseJson
  )
import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  )

data CoreManifest = CoreManifest
  { coreManifestSchema :: String
  , coreManifestCoreId :: CoreId
  , coreManifestArtifactDigest :: Digest
  , coreManifestCoreSchema :: SchemaId
  , coreManifestKernelClaims :: [ClaimName]
  , coreManifestRuntimeDependencies :: [CoreId]
  }
  deriving (Eq, Ord, Show)

data PromotionDecision
  = PromotionPending
  | PromotionApproved
  | PromotionRejected
  deriving (Eq, Ord, Read, Show)

-- | Constructor intentionally hidden. Evidence digests can only be collected
-- from reports that contain the stable passing claims.
data PromotionEvidence = PromotionEvidence
  { promotionEvidenceSemanticDigest :: Digest
  , promotionEvidenceArtifactDigest :: Digest
  , promotionEvidenceEmptyBusinessDigest :: Digest
  }
  deriving (Eq, Ord, Show)

data PromotionRecord = PromotionRecord
  { promotionRecordSchema :: String
  , promotionPreviousCore :: TrustBaseRef
  , promotionCandidateCore :: TrustBaseRef
  , promotionSemanticEvidence :: Digest
  , promotionArtifactEvidence :: Digest
  , promotionEmptyBusinessProof :: Digest
  , promotionDecision :: PromotionDecision
  }
  deriving (Eq, Ord, Show)

newtype CurrentCorePointer = CurrentCorePointer
  { currentCoreRef :: TrustBaseRef
  }
  deriving (Eq, Ord, Show)

data PromotionViolation
  = PromotionEvidenceSemanticRejected
  | PromotionEvidenceArtifactRejected
  | PromotionEvidenceEmptyBusinessRejected
  | PromotionPreviousRefInvalid [TrustBaseRefViolation]
  | PromotionCandidateRefInvalid [TrustBaseRefViolation]
  | PromotionCoreManifestSchemaMismatch String
  | PromotionRecordSchemaMismatch String
  | PromotionCandidateManifestMismatch TrustBaseRef TrustBaseRef
  | PromotionCoreIdentityUnchanged CoreId
  | PromotionPreviousCoreBackReference CoreId
  | PromotionEvidenceDigestInvalid String Digest
  | PromotionDecisionNotPending PromotionDecision
  | PromotionDecisionNotApproved PromotionDecision
  | PromotionPointerMismatch TrustBaseRef TrustBaseRef
  | PromotionDecodeFailed String
  deriving (Eq, Ord, Show)

coreManifestSchemaV1 :: String
coreManifestSchemaV1 =
  "myframework-core-manifest.v1"

promotionRecordSchemaV1 :: String
promotionRecordSchemaV1 =
  "myframework-promotion-record.v1"

currentCorePointerSchemaV1 :: String
currentCorePointerSchemaV1 =
  "myframework-current-core.v1"

buildCoreManifest ::
  CoreId ->
  Digest ->
  SchemaId ->
  [ClaimName] ->
  [CoreId] ->
  CoreManifest
buildCoreManifest currentCoreId currentArtifact currentSchema currentClaims currentDependencies =
  CoreManifest
    { coreManifestSchema = coreManifestSchemaV1
    , coreManifestCoreId = currentCoreId
    , coreManifestArtifactDigest = currentArtifact
    , coreManifestCoreSchema = currentSchema
    , coreManifestKernelClaims = currentClaims
    , coreManifestRuntimeDependencies = currentDependencies
    }

coreManifestTrustBaseRef :: CoreManifest -> TrustBaseRef
coreManifestTrustBaseRef currentManifest =
  TrustBaseRef
    { trustBaseCoreId = coreManifestCoreId currentManifest
    , trustBaseArtifactDigest =
        coreManifestArtifactDigest currentManifest
    , trustBaseManifestDigest =
        Digest (sha256 (renderCoreManifestJson currentManifest))
    , trustBaseSchema = coreManifestCoreSchema currentManifest
    , trustBaseKernelClaims =
        coreManifestKernelClaims currentManifest
    }

collectPromotionEvidence ::
  String ->
  String ->
  String ->
  Either [PromotionViolation] PromotionEvidence
collectPromotionEvidence semanticReport artifactReport emptyBusinessReport =
  case currentViolations of
    [] ->
      Right
        PromotionEvidence
          { promotionEvidenceSemanticDigest =
              Digest (sha256 semanticReport)
          , promotionEvidenceArtifactDigest =
              Digest (sha256 artifactReport)
          , promotionEvidenceEmptyBusinessDigest =
              Digest (sha256 emptyBusinessReport)
          }
    _ ->
      Left currentViolations
  where
    currentViolations =
      [ PromotionEvidenceSemanticRejected
      | not (validSemanticReport semanticReport)
      ]
        ++ [ PromotionEvidenceArtifactRejected
           | not (validArtifactReport artifactReport)
           ]
        ++ [ PromotionEvidenceEmptyBusinessRejected
           | not (validEmptyBusinessReport emptyBusinessReport)
           ]

preparePromotionRecord ::
  TrustBaseRef ->
  CoreManifest ->
  PromotionEvidence ->
  Either [PromotionViolation] PromotionRecord
preparePromotionRecord previousRef candidateManifest currentEvidence =
  case currentViolations of
    [] ->
      Right currentRecord
    _ ->
      Left currentViolations
  where
    currentRecord =
      PromotionRecord
        { promotionRecordSchema = promotionRecordSchemaV1
        , promotionPreviousCore = previousRef
        , promotionCandidateCore =
            coreManifestTrustBaseRef candidateManifest
        , promotionSemanticEvidence =
            promotionEvidenceSemanticDigest currentEvidence
        , promotionArtifactEvidence =
            promotionEvidenceArtifactDigest currentEvidence
        , promotionEmptyBusinessProof =
            promotionEvidenceEmptyBusinessDigest currentEvidence
        , promotionDecision = PromotionPending
        }
    currentViolations =
      validatePromotionRecord candidateManifest currentRecord

approvePromotion ::
  CoreManifest ->
  PromotionRecord ->
  Either [PromotionViolation] (PromotionRecord, CurrentCorePointer)
approvePromotion candidateManifest currentRecord =
  case currentViolations of
    [] ->
      Right
        ( approvedRecord
        , CurrentCorePointer
            { currentCoreRef =
                promotionCandidateCore approvedRecord
            }
        )
    _ ->
      Left currentViolations
  where
    currentViolations =
      validatePromotionRecord candidateManifest currentRecord
        ++ [ PromotionDecisionNotPending
               (promotionDecision currentRecord)
           | promotionDecision currentRecord /= PromotionPending
           ]
    approvedRecord =
      currentRecord
        { promotionDecision = PromotionApproved
        }

validateCoreManifest ::
  CoreId ->
  CoreManifest ->
  [PromotionViolation]
validateCoreManifest previousCore currentManifest =
  [ PromotionCoreManifestSchemaMismatch
      (coreManifestSchema currentManifest)
  | coreManifestSchema currentManifest /= coreManifestSchemaV1
  ]
    ++ [ PromotionCandidateRefInvalid currentViolations
  | let currentViolations =
          validateTrustBaseRef
            (coreManifestTrustBaseRef currentManifest)
  , not (null currentViolations)
  ]
    ++ [ PromotionPreviousCoreBackReference previousCore
       | previousCore
           `elem` coreManifestRuntimeDependencies currentManifest
       ]

validatePromotionRecord ::
  CoreManifest ->
  PromotionRecord ->
  [PromotionViolation]
validatePromotionRecord candidateManifest currentRecord =
  recordSchemaViolations
    ++ previousViolations
    ++ candidateViolations
    ++ manifestViolations
    ++ identityViolations
    ++ evidenceViolations
  where
    previousRef =
      promotionPreviousCore currentRecord
    candidateRef =
      promotionCandidateCore currentRecord
    expectedCandidate =
      coreManifestTrustBaseRef candidateManifest
    recordSchemaViolations =
      [ PromotionRecordSchemaMismatch
          (promotionRecordSchema currentRecord)
      | promotionRecordSchema currentRecord
          /= promotionRecordSchemaV1
      ]
    previousViolations =
      [ PromotionPreviousRefInvalid currentViolations
      | let currentViolations = validateTrustBaseRef previousRef
      , not (null currentViolations)
      ]
    candidateViolations =
      [ PromotionCandidateRefInvalid currentViolations
      | let currentViolations = validateTrustBaseRef candidateRef
      , not (null currentViolations)
      ]
    manifestViolations =
      validateCoreManifest
        (trustBaseCoreId previousRef)
        candidateManifest
        ++ [ PromotionCandidateManifestMismatch
               expectedCandidate
               candidateRef
           | expectedCandidate /= candidateRef
           ]
    identityViolations =
      [ PromotionCoreIdentityUnchanged
          (trustBaseCoreId previousRef)
      | trustBaseCoreId previousRef
          == trustBaseCoreId candidateRef
      ]
    evidenceViolations =
      concat
        [ digestViolations
            "semantic"
            (promotionSemanticEvidence currentRecord)
        , digestViolations
            "artifact"
            (promotionArtifactEvidence currentRecord)
        , digestViolations
            "empty-business"
            (promotionEmptyBusinessProof currentRecord)
        ]

validateCurrentCorePointer ::
  CoreManifest ->
  PromotionRecord ->
  CurrentCorePointer ->
  [PromotionViolation]
validateCurrentCorePointer candidateManifest currentRecord currentPointer =
  validatePromotionRecord candidateManifest currentRecord
    ++ [ PromotionDecisionNotApproved
           (promotionDecision currentRecord)
       | promotionDecision currentRecord /= PromotionApproved
       ]
    ++ [ PromotionPointerMismatch
           (promotionCandidateCore currentRecord)
           (currentCoreRef currentPointer)
       | promotionCandidateCore currentRecord
           /= currentCoreRef currentPointer
       ]

renderCoreManifestJson :: CoreManifest -> String
renderCoreManifestJson currentManifest =
  jsonObject
    [ ("schema", jsonString (coreManifestSchema currentManifest))
    , ("coreId", jsonString (unCoreId (coreManifestCoreId currentManifest)))
    , ( "artifactDigest"
      , jsonString
          (unDigest (coreManifestArtifactDigest currentManifest))
      )
    , ("coreSchema", renderSchemaJson (coreManifestCoreSchema currentManifest))
    , ( "kernelClaims"
      , jsonArray
          (map (jsonString . unClaimName) (coreManifestKernelClaims currentManifest))
      )
    , ( "runtimeDependencies"
      , jsonArray
          (map (jsonString . unCoreId) (coreManifestRuntimeDependencies currentManifest))
      )
    ]

renderPromotionRecordJson :: PromotionRecord -> String
renderPromotionRecordJson currentRecord =
  jsonObject
    [ ("schema", jsonString (promotionRecordSchema currentRecord))
    , ("previousCore", renderTrustBaseRefJson (promotionPreviousCore currentRecord))
    , ("candidateCore", renderTrustBaseRefJson (promotionCandidateCore currentRecord))
    , ("semanticEvidence", renderDigestJson (promotionSemanticEvidence currentRecord))
    , ("artifactEvidence", renderDigestJson (promotionArtifactEvidence currentRecord))
    , ("emptyBusinessProof", renderDigestJson (promotionEmptyBusinessProof currentRecord))
    , ("decision", jsonString (renderDecision (promotionDecision currentRecord)))
    ]

renderCurrentCorePointerJson :: CurrentCorePointer -> String
renderCurrentCorePointerJson currentPointer =
  jsonObject
    [ ("schema", jsonString currentCorePointerSchemaV1)
    , ("core", renderTrustBaseRefJson (currentCoreRef currentPointer))
    ]

decodeCoreManifest :: String -> Either PromotionViolation CoreManifest
decodeCoreManifest currentText = do
  currentJson <- parsePromotionJson currentText
  currentSchema <- fieldString "schema" currentJson
  currentCoreId <- CoreId <$> fieldString "coreId" currentJson
  currentArtifact <- Digest <$> fieldString "artifactDigest" currentJson
  currentCoreSchema <- fieldSchema "coreSchema" currentJson
  currentClaims <- map ClaimName <$> fieldStringArray "kernelClaims" currentJson
  currentDependencies <- map CoreId <$> fieldStringArray "runtimeDependencies" currentJson
  pure
    CoreManifest
      { coreManifestSchema = currentSchema
      , coreManifestCoreId = currentCoreId
      , coreManifestArtifactDigest = currentArtifact
      , coreManifestCoreSchema = currentCoreSchema
      , coreManifestKernelClaims = currentClaims
      , coreManifestRuntimeDependencies = currentDependencies
      }

decodePromotionRecord :: String -> Either PromotionViolation PromotionRecord
decodePromotionRecord currentText = do
  currentJson <- parsePromotionJson currentText
  currentSchema <- fieldString "schema" currentJson
  previousJson <- fieldValue "previousCore" currentJson
  candidateJson <- fieldValue "candidateCore" currentJson
  previousRef <- parseTrustBaseRef previousJson
  candidateRef <- parseTrustBaseRef candidateJson
  semanticDigest <- Digest <$> fieldString "semanticEvidence" currentJson
  artifactDigest <- Digest <$> fieldString "artifactEvidence" currentJson
  emptyDigest <- Digest <$> fieldString "emptyBusinessProof" currentJson
  currentDecision <- fieldString "decision" currentJson >>= parseDecision
  pure
    PromotionRecord
      { promotionRecordSchema = currentSchema
      , promotionPreviousCore = previousRef
      , promotionCandidateCore = candidateRef
      , promotionSemanticEvidence = semanticDigest
      , promotionArtifactEvidence = artifactDigest
      , promotionEmptyBusinessProof = emptyDigest
      , promotionDecision = currentDecision
      }

decodeCurrentCorePointer :: String -> Either PromotionViolation CurrentCorePointer
decodeCurrentCorePointer currentText = do
  currentJson <- parsePromotionJson currentText
  currentSchema <- fieldString "schema" currentJson
  if currentSchema /= currentCorePointerSchemaV1
    then Left (PromotionDecodeFailed "unexpected current pointer schema")
    else do
      currentRef <- fieldValue "core" currentJson >>= parseTrustBaseRef
      pure (CurrentCorePointer currentRef)

renderTrustBaseRefJson :: TrustBaseRef -> String
renderTrustBaseRefJson currentRef =
  jsonObject
    [ ("coreId", jsonString (unCoreId (trustBaseCoreId currentRef)))
    , ("artifactDigest", renderDigestJson (trustBaseArtifactDigest currentRef))
    , ("manifestDigest", renderDigestJson (trustBaseManifestDigest currentRef))
    , ("schema", renderSchemaJson (trustBaseSchema currentRef))
    , ( "kernelClaims"
      , jsonArray
          (map (jsonString . unClaimName) (trustBaseKernelClaims currentRef))
      )
    ]

parseTrustBaseRef :: JsonValue -> Either PromotionViolation TrustBaseRef
parseTrustBaseRef currentJson = do
  currentCoreId <- CoreId <$> fieldString "coreId" currentJson
  currentArtifact <- Digest <$> fieldString "artifactDigest" currentJson
  currentManifest <- Digest <$> fieldString "manifestDigest" currentJson
  currentSchema <- fieldSchema "schema" currentJson
  currentClaims <- map ClaimName <$> fieldStringArray "kernelClaims" currentJson
  pure
    TrustBaseRef
      { trustBaseCoreId = currentCoreId
      , trustBaseArtifactDigest = currentArtifact
      , trustBaseManifestDigest = currentManifest
      , trustBaseSchema = currentSchema
      , trustBaseKernelClaims = currentClaims
      }

renderSchemaJson :: SchemaId -> String
renderSchemaJson currentSchema =
  jsonObject
    [ ("name", jsonString (unSchemaName (schemaIdName currentSchema)))
    , ( "major"
      , show
          (schemaVersionMajor (schemaIdVersion currentSchema))
      )
    ]

fieldSchema :: String -> JsonValue -> Either PromotionViolation SchemaId
fieldSchema currentName currentJson = do
  schemaJson <- fieldValue currentName currentJson
  currentSchemaName <- fieldString "name" schemaJson
  currentMajorText <-
    case jsonObjectField "major" schemaJson of
      Left currentError -> Left (PromotionDecodeFailed currentError)
      Right (JsonNumber currentNumber) -> Right currentNumber
      Right _ -> Left (PromotionDecodeFailed "expected schema major number")
  currentMajor <-
    maybe
      (Left (PromotionDecodeFailed "invalid schema major"))
      Right
      (readMaybe currentMajorText)
  pure
    SchemaId
      { schemaIdName = SchemaName currentSchemaName
      , schemaIdVersion = SchemaVersion currentMajor
      }

fieldString :: String -> JsonValue -> Either PromotionViolation String
fieldString currentName currentJson =
  fieldValue currentName currentJson >>= parseString

fieldStringArray :: String -> JsonValue -> Either PromotionViolation [String]
fieldStringArray currentName currentJson = do
  currentValue <- fieldValue currentName currentJson
  currentItems <-
    either
      (Left . PromotionDecodeFailed)
      Right
      (jsonArrayItems currentValue)
  mapM parseString currentItems

fieldValue :: String -> JsonValue -> Either PromotionViolation JsonValue
fieldValue currentName currentJson =
  either
    (Left . PromotionDecodeFailed)
    Right
    (jsonObjectField currentName currentJson)

parseString :: JsonValue -> Either PromotionViolation String
parseString currentValue =
  either
    (Left . PromotionDecodeFailed)
    Right
    (jsonStringValue currentValue)

parsePromotionJson :: String -> Either PromotionViolation JsonValue
parsePromotionJson =
  either
    (Left . PromotionDecodeFailed)
    Right
    . parseJson

parseDecision :: String -> Either PromotionViolation PromotionDecision
parseDecision currentText =
  case currentText of
    "pending" -> Right PromotionPending
    "approved" -> Right PromotionApproved
    "rejected" -> Right PromotionRejected
    _ -> Left (PromotionDecodeFailed "unknown promotion decision")

renderDecision :: PromotionDecision -> String
renderDecision currentDecision =
  case currentDecision of
    PromotionPending -> "pending"
    PromotionApproved -> "approved"
    PromotionRejected -> "rejected"

renderDigestJson :: Digest -> String
renderDigestJson =
  jsonString . unDigest

digestViolations :: String -> Digest -> [PromotionViolation]
digestViolations currentName currentDigest =
  [ PromotionEvidenceDigestInvalid currentName currentDigest
  | not (validDigest currentDigest)
  ]

validDigest :: Digest -> Bool
validDigest (Digest currentValue) =
  length currentValue == 64
    && all (`elem` ("0123456789abcdef" :: String)) currentValue

validSemanticReport :: String -> Bool
validSemanticReport currentText =
  all
    (`isInfixOf` currentText)
    [ "\"schema\":\"core-self-interpret-evidence.v1\""
    , "\"result\":\"passed\""
    , "\"name\":\"semantic-fixed-point-passed\",\"status\":\"passed\""
    , "\"name\":\"candidate-has-no-previous-core-runtime-dependency\",\"status\":\"passed\""
    ]

validEmptyBusinessReport :: String -> Bool
validEmptyBusinessReport currentText =
  all
    (`isInfixOf` currentText)
    [ "\"schema\":\"core-self-interpret-evidence.v1\""
    , "\"result\":\"passed\""
    , "\"name\":\"empty-business-closes-recursion\",\"status\":\"passed\""
    , "\"name\":\"empty-business-has-no-curde\",\"status\":\"passed\""
    , "\"name\":\"empty-business-has-no-handler\",\"status\":\"passed\""
    , "\"name\":\"empty-business-has-no-host-io\",\"status\":\"passed\""
    ]

validArtifactReport :: String -> Bool
validArtifactReport currentText =
  all
    (`isInfixOf` currentText)
    [ "\"schema\":\"myframework-self-artifact-fixed-point.v1\""
    , "\"result\":\"passed\""
    , "\"selfModelDiffs\":0"
    , "\"controlTraceDiffs\":0"
    , "\"payloadDiffs\":0"
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
