module Main (main) where

import Control.Monad
  ( unless )
import Data.Char
  ( ord )
import Data.List
  ( intercalate )
import Numeric
  ( showHex )
import System.Exit
  ( exitFailure )

import MyFramework.TrustBase.Core
import MyFramework.TrustBase.Digest
  ( sha256 )
import MyFramework.TrustBase.Evidence
  ( completeEvidence
  , evidenceFor
  , promotionClaimCatalog
  , promotionEvidenceSchemaV1
  , validateEvidenceClaims
  )
import MyFramework.TrustBase.Promotion
import MyFramework.TrustBase.Types
  ( ArtifactName (..)
  , ClaimName (..)
  , Evidence (..)
  , EvidenceStatus (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  , evidencePassed
  , renderSchemaId
  )

main :: IO ()
main = do
  let previousManifest =
        buildCoreManifest
          (CoreId "core0")
          artifactDigest
          coreSchema
          coreClaims
          []
      previousRef =
        coreManifestTrustBaseRef previousManifest
      candidateManifest =
        buildCoreManifest
          (CoreId "core1")
          artifactDigest
          coreSchema
          coreClaims
          []
      contaminatedManifest =
        candidateManifest
          { coreManifestRuntimeDependencies = [CoreId "core0"]
          }
      evidenceResult =
        collectPromotionEvidence
          semanticReport
          artifactReport
          semanticReport
      recordResult =
        evidenceResult
          >>= preparePromotionRecord
            previousRef
            candidateManifest
      approvalResult =
        recordResult
          >>= approvePromotion candidateManifest
      manifestRoundTrip =
        decodeCoreManifest
          (renderCoreManifestJson candidateManifest)
          == Right candidateManifest
      recordRoundTrip =
        case recordResult of
          Left _ -> False
          Right currentRecord ->
            decodePromotionRecord
              (renderPromotionRecordJson currentRecord)
              == Right currentRecord
      approvedPointerValid =
        case approvalResult of
          Left _ -> False
          Right (approvedRecord, currentPointer) ->
            null
              ( validateCurrentCorePointer
                  candidateManifest
                  approvedRecord
                  currentPointer
              )
      pointerRoundTrip =
        case approvalResult of
          Left _ -> False
          Right (_, currentPointer) ->
            decodeCurrentCorePointer
              (renderCurrentCorePointerJson currentPointer)
              == Right currentPointer
      pendingCannotBeCurrent =
        case recordResult of
          Left _ -> False
          Right currentRecord ->
            not
              ( null
                  ( validateCurrentCorePointer
                      candidateManifest
                      currentRecord
                      (CurrentCorePointer (promotionCandidateCore currentRecord))
                  )
              )
      backReferenceRejected =
        case evidenceResult of
          Left _ -> False
          Right currentEvidence ->
            case
                preparePromotionRecord
                  previousRef
                  contaminatedManifest
                  currentEvidence
              of
                Left currentViolations ->
                  PromotionPreviousCoreBackReference (CoreId "core0")
                    `elem` currentViolations
                Right _ ->
                  False
      candidateMismatchRejected =
        case recordResult of
          Left _ -> False
          Right currentRecord ->
            let tamperedRecord =
                  currentRecord
                    { promotionCandidateCore =
                        (promotionCandidateCore currentRecord)
                          { trustBaseCoreId = CoreId "core2"
                          }
                    }
             in any
                  isCandidateMismatch
                  (validatePromotionRecord candidateManifest tamperedRecord)
      badSemanticRejected =
        case
            collectPromotionEvidence
              "{\"result\":\"passed\"}"
              artifactReport
              semanticReport
          of
            Left currentViolations ->
              PromotionEvidenceSemanticRejected
                `elem` currentViolations
            Right _ ->
              False
      evidenceDigestsBound =
        case (evidenceResult, recordResult) of
          (Right currentEvidence, Right currentRecord) ->
            promotionEvidenceSemanticDigest currentEvidence
              == promotionSemanticEvidence currentRecord
              && promotionEvidenceArtifactDigest currentEvidence
                == promotionArtifactEvidence currentRecord
              && promotionEvidenceEmptyBusinessDigest currentEvidence
                == promotionEmptyBusinessProof currentRecord
          _ ->
            False
      approvalTargetsCandidate =
        case approvalResult of
          Left _ -> False
          Right (approvedRecord, currentPointer) ->
            promotionDecision approvedRecord == PromotionApproved
              && currentCoreRef currentPointer
                == coreManifestTrustBaseRef candidateManifest
      checks =
        [ ( "promotion-core-manifest-content-addressed"
          , trustBaseManifestDigest
              (coreManifestTrustBaseRef candidateManifest)
              == Digest
                (sha256 (renderCoreManifestJson candidateManifest))
          )
        , ("promotion-core-manifest-roundtrip", manifestRoundTrip)
        , ("promotion-evidence-reports-validated", either (const False) (const True) evidenceResult)
        , ("promotion-evidence-digests-bound", evidenceDigestsBound)
        , ("promotion-record-roundtrip", recordRoundTrip)
        , ("promotion-previous-core-back-reference-rejected", backReferenceRejected)
        , ("promotion-candidate-manifest-mismatch-rejected", candidateMismatchRejected)
        , ("promotion-invalid-semantic-report-rejected", badSemanticRejected)
        , ("promotion-pending-cannot-be-current", pendingCannotBeCurrent)
        , ("promotion-approved-pointer-matches-candidate", approvalTargetsCandidate && approvedPointerValid)
        , ("promotion-current-pointer-roundtrip", pointerRoundTrip)
        ]
      coreEvidence =
        [ evidenceFor
            (ClaimName currentName)
            currentPassed
            "passed"
            (show currentPassed)
            witnessArtifact
        | (currentName, currentPassed) <- checks
        ]
      allEvidence =
        completeEvidence
          promotionClaimCatalog
          witnessArtifact
          coreEvidence
      catalogValid =
        null
          ( validateEvidenceClaims
              promotionClaimCatalog
              allEvidence
          )
      succeeded =
        catalogValid && all evidencePassed allEvidence
  putStrLn (renderReport allEvidence succeeded)
  unless succeeded exitFailure

isCandidateMismatch :: PromotionViolation -> Bool
isCandidateMismatch currentViolation =
  case currentViolation of
    PromotionCandidateManifestMismatch _ _ -> True
    _ -> False

artifactDigest :: Digest
artifactDigest =
  Digest (sha256 "candidate-artifact")

coreSchema :: SchemaId
coreSchema =
  SchemaId
    { schemaIdName = SchemaName "myframework-core"
    , schemaIdVersion = SchemaVersion 1
    }

coreClaims :: [ClaimName]
coreClaims =
  [ ClaimName "host-kernel-closed"
  , ClaimName "semantic-self-interpret"
  ]

semanticReport :: String
semanticReport =
  "{\"schema\":\"core-self-interpret-evidence.v1\",\"result\":\"passed\",\"claims\":[{\"name\":\"semantic-fixed-point-passed\",\"status\":\"passed\"},{\"name\":\"candidate-has-no-previous-core-runtime-dependency\",\"status\":\"passed\"},{\"name\":\"empty-business-closes-recursion\",\"status\":\"passed\"},{\"name\":\"empty-business-has-no-curde\",\"status\":\"passed\"},{\"name\":\"empty-business-has-no-handler\",\"status\":\"passed\"},{\"name\":\"empty-business-has-no-host-io\",\"status\":\"passed\"}]}"

artifactReport :: String
artifactReport =
  "{\"schema\":\"myframework-self-artifact-fixed-point.v1\",\"result\":\"passed\",\"selfModelDiffs\":0,\"controlTraceDiffs\":0,\"payloadDiffs\":0}"

witnessArtifact :: ArtifactName
witnessArtifact =
  ArtifactName "core-promotion-witness"

renderReport :: [Evidence] -> Bool -> String
renderReport currentEvidence succeeded =
  jsonObject
    [ ("schema", jsonString (renderSchemaId promotionEvidenceSchemaV1))
    , ("artifact", jsonString "core-promotion-witness")
    , ("result", jsonString (if succeeded then "passed" else "failed"))
    , ("claims", jsonArray (map renderEvidence currentEvidence))
    ]

renderEvidence :: Evidence -> String
renderEvidence currentEvidence =
  jsonObject
    [ ("name", jsonString (unClaimName (evidenceClaim currentEvidence)))
    , ( "status"
      , jsonString
          ( case evidenceStatus currentEvidence of
              EvidencePassed -> "passed"
              EvidenceFailed -> "failed"
          )
      )
    , ("expected", jsonString (evidenceExpected currentEvidence))
    , ("observed", jsonString (evidenceObserved currentEvidence))
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
