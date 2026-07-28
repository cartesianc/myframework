module Main (main) where

import Control.Exception
  ( finally )
import Control.Monad
  ( unless )
import Data.Char
  ( ord )
import Data.List
  ( intercalate )
import Numeric
  ( showHex )
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , getCurrentDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Exit
  ( exitFailure )
import System.FilePath
  ( normalise
  , takeDirectory
  , (</>)
  )
import System.IO
  ( hClose
  , openTempFile
  )

import MyFramework.SDK.Package
import MyFramework.SDK.SourceArtifact
  ( SdkSourceInput (..)
  , buildSdkSourceReport
  , handlerCoverageFromIds
  , mkSdkSourceInput
  , sdkSourceReportReady
  )
import MyFramework.Self.Artifact
  ( ArtifactManifest (..)
  , OutputDir (..)
  , materializeStageFrom
  )
import MyFramework.Self.CoreModel
  ( frameworkAsBusiness
  , frameworkAsBusinessAstSeed
  , frameworkAsBusinessEffectSystems
  , frameworkAsBusinessHandlerCoverage
  )
import MyFramework.Self.Model
  ( selfModel )
import MyFramework.TrustBase.Core
  ( CoreId (..)
  , Digest (..)
  , SdkCoreLock (..)
  , TrustBaseRef (..)
  )
import MyFramework.TrustBase.Evidence
  ( completeEvidence
  , evidenceFor
  , sdkPackageClaimCatalog
  , sdkPackageEvidenceSchemaV1
  , validateEvidenceClaims
  )
import MyFramework.TrustBase.Manifest
  ( TrustBaseManifest (..)
  )
import MyFramework.TrustBase.Promotion
import MyFramework.TrustBase.Self
  ( myFrameworkTrustBaseManifest )
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
  sourceRoot <- getCurrentDirectory
  let generatedRoot =
        sourceRoot </> ".generated"
  createDirectoryIfMissing True generatedRoot
  probeRoot <- freshGeneratedChild generatedRoot "sdk-package-probe"
  packageRoot <- freshGeneratedChild generatedRoot "sdk-package-output"
  runWitness sourceRoot probeRoot packageRoot
    `finally` do
      safeRemoveGeneratedChild generatedRoot probeRoot
      safeRemoveGeneratedChild generatedRoot packageRoot

runWitness :: FilePath -> FilePath -> FilePath -> IO ()
runWitness sourceRoot probeRoot packageRoot =
  case (selfModel, frameworkAsBusiness) of
    (Left currentErrors, _) ->
      failWitness "self-model" (show currentErrors)
    (_, Left currentErrors) ->
      failWitness "framework-as-business" (show currentErrors)
    (Right currentSelfModel, Right currentFramework) -> do
      probeResult <-
        materializeStageFrom
          sourceRoot
          (OutputDir probeRoot)
          "sdk-package-probe"
          currentSelfModel
      case probeResult of
        Left currentError ->
          failWitness "probe-materialize" (show currentError)
        Right probeManifest -> do
          let currentArtifactDigest =
                Digest
                  (artifactManifestPayloadDigest probeManifest)
              previousManifest =
                coreManifest
                  (CoreId "core0")
                  currentArtifactDigest
              candidateManifest =
                coreManifest
                  (CoreId "core1")
                  currentArtifactDigest
              evidenceResult =
                collectPromotionEvidence
                  semanticReport
                  artifactReport
                  semanticReport
              pendingResult =
                evidenceResult
                  >>= preparePromotionRecord
                    (coreManifestTrustBaseRef previousManifest)
                    candidateManifest
              approvedResult =
                pendingResult
                  >>= approvePromotion candidateManifest
              currentInput =
                mkSdkSourceInput
                  (coreManifestTrustBaseRef candidateManifest)
                  "0.1.0-beta.1"
                  (frameworkAsBusinessEffectSystems currentFramework)
                  (frameworkAsBusinessAstSeed currentFramework)
                  ( Just
                      ( handlerCoverageFromIds
                          ( frameworkAsBusinessHandlerCoverage
                              currentFramework
                          )
                      )
                  )
              currentReport =
                buildSdkSourceReport currentInput
              tamperedSurface =
                currentInput
                  { sdkSourceCoreLock =
                      (sdkSourceCoreLock currentInput)
                        { sdkSurfaceDigest =
                            Digest (replicate 64 '0')
                        }
                  }
              tamperedLowering =
                currentInput
                  { sdkSourceCoreLock =
                      (sdkSourceCoreLock currentInput)
                        { sdkLoweringDigest =
                            Digest (replicate 64 '0')
                        }
                  }
              pendingRejected =
                case pendingResult of
                  Left _ -> False
                  Right currentPending ->
                    not
                      ( null
                          ( validateCurrentCorePointer
                              candidateManifest
                              currentPending
                              ( CurrentCorePointer
                                  (promotionCandidateCore currentPending)
                              )
                          )
                      )
          packageResult <-
            case approvedResult of
              Left currentErrors ->
                pure
                  (Left (SdkPackagePromotionRejected (map show currentErrors)))
              Right (approvedRecord, currentPointer) ->
                materializeApprovedSdkPackage
                  sourceRoot
                  (OutputDir packageRoot)
                  "0.1.0-beta.1"
                  candidateManifest
                  approvedRecord
                  currentPointer
          verificationResult <-
            case packageResult of
              Left _ ->
                pure (Left [])
              Right _ ->
                verifySdkPackage packageRoot
          let checks =
                [ ( "sdk-core-lock-valid"
                  , sdkSourceReportReady currentReport
                  )
                , ( "sdk-surface-digest-tamper-blocked"
                  , not
                      ( sdkSourceReportReady
                          (buildSdkSourceReport tamperedSurface)
                      )
                  )
                , ( "sdk-lowering-digest-tamper-blocked"
                  , not
                      ( sdkSourceReportReady
                          (buildSdkSourceReport tamperedLowering)
                      )
                  )
                , ("sdk-pending-core-rejected", pendingRejected)
                , ( "sdk-approved-artifact-digest-bound"
                  , trustBaseArtifactDigest
                      (coreManifestTrustBaseRef candidateManifest)
                      == currentArtifactDigest
                  )
                , ( "sdk-package-materialized"
                  , either (const False) (const True) packageResult
                  )
                , ( "sdk-package-verifies"
                  , either (const False) (const True) verificationResult
                  )
                , ( "sdk-package-manifest-binds-core-lock"
                  , case (packageResult, verificationResult) of
                      (Right leftManifest, Right rightManifest) ->
                        sdkPackageManifestCoreLock leftManifest
                          == sdkPackageManifestCoreLock rightManifest
                      _ ->
                        False
                  )
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
                  sdkPackageClaimCatalog
                  witnessArtifact
                  coreEvidence
              succeeded =
                null
                  ( validateEvidenceClaims
                      sdkPackageClaimCatalog
                      allEvidence
                  )
                  && all evidencePassed allEvidence
          putStrLn (renderReport allEvidence succeeded)
          unless succeeded exitFailure

coreManifest :: CoreId -> Digest -> CoreManifest
coreManifest currentCoreId currentArtifactDigest =
  buildCoreManifest
    currentCoreId
    currentArtifactDigest
    coreSchema
    (trustBaseManifestEvidenceClaims myFrameworkTrustBaseManifest)
    []

coreSchema :: SchemaId
coreSchema =
  SchemaId
    { schemaIdName = SchemaName "myframework-core"
    , schemaIdVersion = SchemaVersion 1
    }

semanticReport :: String
semanticReport =
  "{\"schema\":\"core-self-interpret-evidence.v1\",\"result\":\"passed\",\"claims\":[{\"name\":\"semantic-fixed-point-passed\",\"status\":\"passed\"},{\"name\":\"candidate-has-no-previous-core-runtime-dependency\",\"status\":\"passed\"},{\"name\":\"empty-business-closes-recursion\",\"status\":\"passed\"},{\"name\":\"empty-business-has-no-curde\",\"status\":\"passed\"},{\"name\":\"empty-business-has-no-handler\",\"status\":\"passed\"},{\"name\":\"empty-business-has-no-host-io\",\"status\":\"passed\"}]}"

artifactReport :: String
artifactReport =
  "{\"schema\":\"myframework-self-artifact-fixed-point.v1\",\"result\":\"passed\",\"selfModelDiffs\":0,\"controlTraceDiffs\":0,\"payloadDiffs\":0}"

freshGeneratedChild :: FilePath -> String -> IO FilePath
freshGeneratedChild generatedRoot currentPrefix = do
  (currentPath, currentHandle) <-
    openTempFile generatedRoot currentPrefix
  hClose currentHandle
  removeFile currentPath
  pure currentPath

safeRemoveGeneratedChild :: FilePath -> FilePath -> IO ()
safeRemoveGeneratedChild generatedRoot currentPath =
  if normalise (takeDirectory currentPath) == normalise generatedRoot
    then do
      currentExists <- doesDirectoryExist currentPath
      if currentExists
        then removeDirectoryRecursive currentPath
        else pure ()
    else
      failWitness
        "cleanup-boundary"
        ("refusing cleanup outside generated root: " ++ currentPath)

witnessArtifact :: ArtifactName
witnessArtifact =
  ArtifactName "sdk-package-witness"

failWitness :: String -> String -> IO value
failWitness currentStage currentObserved = do
  putStrLn
    ( jsonObject
        [ ("schema", jsonString "sdk-package-witness.v1")
        , ("artifact", jsonString "sdk-package-witness")
        , ("result", jsonString "failed")
        , ("stage", jsonString currentStage)
        , ("observed", jsonString currentObserved)
        ]
    )
  exitFailure

renderReport :: [Evidence] -> Bool -> String
renderReport currentEvidence succeeded =
  jsonObject
    [ ("schema", jsonString (renderSchemaId sdkPackageEvidenceSchemaV1))
    , ("artifact", jsonString "sdk-package-witness")
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
