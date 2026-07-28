module Main (main) where

import Control.Exception
  ( IOException
  , try
  )
import Data.List
  ( intercalate )
import System.Directory
  ( createDirectoryIfMissing
  , renameFile
  )
import System.Environment
  ( getArgs )
import System.Exit
  ( exitFailure )
import System.FilePath
  ( takeDirectory )

import MyFramework.Self.Artifact
  ( ArtifactManifest (..)
  , verifyArtifactManifest
  )
import MyFramework.TrustBase.Core
  ( CoreId (..)
  , Digest (..)
  )
import MyFramework.TrustBase.Manifest
  ( TrustBaseManifest (..)
  )
import MyFramework.TrustBase.Promotion
import MyFramework.TrustBase.Self
  ( myFrameworkTrustBaseManifest )
import MyFramework.TrustBase.Types
  ( SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  )

main :: IO ()
main = do
  currentArgs <- getArgs
  case currentArgs of
    ["genesis-manifest", currentCoreId, artifactRoot, outputPath] ->
      runGenesisManifest currentCoreId artifactRoot outputPath
    [ "prepare"
      , previousManifestPath
      , candidateCoreId
      , artifactRoot
      , semanticPath
      , artifactEvidencePath
      , emptyBusinessPath
      , candidateManifestPath
      , pendingRecordPath
      ] ->
        runPrepare
          previousManifestPath
          candidateCoreId
          artifactRoot
          semanticPath
          artifactEvidencePath
          emptyBusinessPath
          candidateManifestPath
          pendingRecordPath
    [ "verify"
      , candidateManifestPath
      , recordPath
      , semanticPath
      , artifactEvidencePath
      , emptyBusinessPath
      ] ->
        runVerify
          candidateManifestPath
          recordPath
          semanticPath
          artifactEvidencePath
          emptyBusinessPath
    [ "approve"
      , candidateManifestPath
      , pendingRecordPath
      , semanticPath
      , artifactEvidencePath
      , emptyBusinessPath
      , approvedRecordPath
      , currentPointerPath
      ] ->
        runApprove
          candidateManifestPath
          pendingRecordPath
          semanticPath
          artifactEvidencePath
          emptyBusinessPath
          approvedRecordPath
          currentPointerPath
    _ ->
      failTool
        "arguments"
        "expected genesis-manifest CORE_ID ARTIFACT_ROOT OUTPUT | prepare PREVIOUS_MANIFEST CANDIDATE_ID ARTIFACT_ROOT SEMANTIC ARTIFACT_EVIDENCE EMPTY_BUSINESS CANDIDATE_MANIFEST PENDING_RECORD | verify CANDIDATE_MANIFEST RECORD SEMANTIC ARTIFACT_EVIDENCE EMPTY_BUSINESS | approve CANDIDATE_MANIFEST PENDING_RECORD SEMANTIC ARTIFACT_EVIDENCE EMPTY_BUSINESS APPROVED_RECORD CURRENT_POINTER"

runGenesisManifest :: String -> FilePath -> FilePath -> IO ()
runGenesisManifest currentCoreId artifactRoot outputPath = do
  currentResult <-
    manifestFromArtifact
      (CoreId currentCoreId)
      artifactRoot
  case currentResult of
    Left currentErrors ->
      failTool "genesis-manifest" currentErrors
    Right currentManifest -> do
      writeResult <-
        writeAtomic
          outputPath
          (renderCoreManifestJson currentManifest)
      finishWrite "genesis-manifest" outputPath writeResult

runPrepare ::
  FilePath ->
  String ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO ()
runPrepare
  previousManifestPath
  candidateCoreId
  artifactRoot
  semanticPath
  artifactEvidencePath
  emptyBusinessPath
  candidateManifestPath
  pendingRecordPath = do
    previousResult <- readDecoded previousManifestPath decodeCoreManifest
    candidateResult <-
      manifestFromArtifact
        (CoreId candidateCoreId)
        artifactRoot
    evidenceResult <-
      readPromotionEvidence
        semanticPath
        artifactEvidencePath
        emptyBusinessPath
    case (previousResult, candidateResult, evidenceResult) of
      (Right previousManifest, Right candidateManifest, Right currentEvidence) ->
        case
            preparePromotionRecord
              (coreManifestTrustBaseRef previousManifest)
              candidateManifest
              currentEvidence
          of
            Left currentViolations ->
              failTool "prepare" (show currentViolations)
            Right currentRecord -> do
              manifestWrite <-
                writeAtomic
                  candidateManifestPath
                  (renderCoreManifestJson candidateManifest)
              case manifestWrite of
                Left currentError ->
                  failTool "prepare" currentError
                Right () -> do
                  recordWrite <-
                    writeAtomic
                      pendingRecordPath
                      (renderPromotionRecordJson currentRecord)
                  case recordWrite of
                    Left currentError ->
                      failTool "prepare" currentError
                    Right () ->
                      putStrLn
                        ( successJson
                            "prepare"
                            [ ("candidateManifest", candidateManifestPath)
                            , ("pendingRecord", pendingRecordPath)
                            ]
                        )
      currentFailure ->
        failTool "prepare" (showTripleFailure currentFailure)

runVerify ::
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO ()
runVerify candidateManifestPath recordPath semanticPath artifactPath emptyPath = do
  currentResult <-
    loadVerifiedPromotion
      candidateManifestPath
      recordPath
      semanticPath
      artifactPath
      emptyPath
  case currentResult of
    Left currentErrors ->
      failTool "verify" (show currentErrors)
    Right _ ->
      putStrLn (successJson "verify" [])

runApprove ::
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO ()
runApprove
  candidateManifestPath
  pendingRecordPath
  semanticPath
  artifactPath
  emptyPath
  approvedRecordPath
  currentPointerPath = do
    currentResult <-
      loadVerifiedPromotion
        candidateManifestPath
        pendingRecordPath
        semanticPath
        artifactPath
        emptyPath
    case currentResult of
      Left currentErrors ->
        failTool "approve" (show currentErrors)
      Right (candidateManifest, currentRecord) ->
        case approvePromotion candidateManifest currentRecord of
          Left currentViolations ->
            failTool "approve" (show currentViolations)
          Right (approvedRecord, currentPointer) -> do
            approvedWrite <-
              writeAtomic
                approvedRecordPath
                (renderPromotionRecordJson approvedRecord)
            case approvedWrite of
              Left currentError ->
                failTool "approve" currentError
              Right () -> do
                pointerWrite <-
                  writeAtomic
                    currentPointerPath
                    (renderCurrentCorePointerJson currentPointer)
                case pointerWrite of
                  Left currentError ->
                    failTool "approve" currentError
                  Right () ->
                    putStrLn
                      ( successJson
                          "approve"
                          [ ("approvedRecord", approvedRecordPath)
                          , ("currentPointer", currentPointerPath)
                          ]
                      )

loadVerifiedPromotion ::
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO (Either [String] (CoreManifest, PromotionRecord))
loadVerifiedPromotion candidatePath recordPath semanticPath artifactPath emptyPath = do
  candidateResult <- readDecoded candidatePath decodeCoreManifest
  recordResult <- readDecoded recordPath decodePromotionRecord
  evidenceResult <-
    readPromotionEvidence semanticPath artifactPath emptyPath
  pure
    ( case (candidateResult, recordResult, evidenceResult) of
        (Right candidateManifest, Right currentRecord, Right currentEvidence) ->
          let currentErrors =
                map show
                  (validatePromotionRecord candidateManifest currentRecord)
                  ++ evidenceBindingErrors currentEvidence currentRecord
           in if null currentErrors
                then Right (candidateManifest, currentRecord)
                else Left currentErrors
        currentFailure ->
          Left [showTripleFailure currentFailure]
    )

evidenceBindingErrors ::
  PromotionEvidence ->
  PromotionRecord ->
  [String]
evidenceBindingErrors currentEvidence currentRecord =
  [ "semantic evidence digest mismatch"
  | promotionEvidenceSemanticDigest currentEvidence
      /= promotionSemanticEvidence currentRecord
  ]
    ++ [ "artifact evidence digest mismatch"
       | promotionEvidenceArtifactDigest currentEvidence
           /= promotionArtifactEvidence currentRecord
       ]
    ++ [ "EmptyBusiness evidence digest mismatch"
       | promotionEvidenceEmptyBusinessDigest currentEvidence
           /= promotionEmptyBusinessProof currentRecord
       ]

manifestFromArtifact ::
  CoreId ->
  FilePath ->
  IO (Either String CoreManifest)
manifestFromArtifact currentCoreId artifactRoot = do
  currentResult <- verifyArtifactManifest artifactRoot
  pure
    ( case currentResult of
        Left currentErrors ->
          Left (show currentErrors)
        Right currentArtifact ->
          Right
            ( buildCoreManifest
                currentCoreId
                (Digest (artifactManifestPayloadDigest currentArtifact))
                coreSchema
                (trustBaseManifestEvidenceClaims myFrameworkTrustBaseManifest)
                []
            )
    )

coreSchema :: SchemaId
coreSchema =
  SchemaId
    { schemaIdName = SchemaName "myframework-core"
    , schemaIdVersion = SchemaVersion 1
    }

readPromotionEvidence ::
  FilePath ->
  FilePath ->
  FilePath ->
  IO (Either String PromotionEvidence)
readPromotionEvidence semanticPath artifactPath emptyPath = do
  semanticResult <- readText semanticPath
  artifactResult <- readText artifactPath
  emptyResult <- readText emptyPath
  pure
    ( case (semanticResult, artifactResult, emptyResult) of
        (Right semanticText, Right artifactText, Right emptyText) ->
          case
              collectPromotionEvidence
                semanticText
                artifactText
                emptyText
            of
              Left currentViolations ->
                Left (show currentViolations)
              Right currentEvidence ->
                Right currentEvidence
        currentFailure ->
          Left (show currentFailure)
    )

readDecoded ::
  FilePath ->
  (String -> Either error value) ->
  IO (Either String value)
readDecoded currentPath currentDecoder = do
  currentResult <- readText currentPath
  pure
    ( case currentResult of
        Left currentError ->
          Left currentError
        Right currentText ->
          case currentDecoder currentText of
            Left _ ->
              Left ("decode failed: " ++ currentPath)
            Right currentValue ->
              Right currentValue
    )

readText :: FilePath -> IO (Either String String)
readText currentPath = do
  currentResult <-
    try (readFile currentPath) :: IO (Either IOException String)
  pure
    ( case currentResult of
        Left currentError ->
          Left (currentPath ++ ": " ++ show currentError)
        Right currentText ->
          Right currentText
    )

writeAtomic :: FilePath -> String -> IO (Either String ())
writeAtomic currentPath currentText = do
  let temporaryPath =
        currentPath ++ ".tmp"
  createResult <-
    try
      (createDirectoryIfMissing True (takeDirectory currentPath)) ::
      IO (Either IOException ())
  case createResult of
    Left currentError ->
      pure (Left (show currentError))
    Right () -> do
      writeResult <-
        try (writeFile temporaryPath (currentText ++ "\n")) ::
        IO (Either IOException ())
      case writeResult of
        Left currentError ->
          pure (Left (show currentError))
        Right () -> do
          renameResult <-
            try (renameFile temporaryPath currentPath) ::
            IO (Either IOException ())
          pure
            ( case renameResult of
                Left currentError -> Left (show currentError)
                Right () -> Right ()
            )

finishWrite :: String -> FilePath -> Either String () -> IO ()
finishWrite currentStage currentPath currentResult =
  case currentResult of
    Left currentError ->
      failTool currentStage currentError
    Right () ->
      putStrLn
        (successJson currentStage [("output", currentPath)])

showTripleFailure :: Show value => value -> String
showTripleFailure =
  show

failTool :: String -> String -> IO value
failTool currentStage currentObserved = do
  putStrLn
    ( jsonObject
        [ ("schema", jsonString "myframework-core-promotion-tool.v1")
        , ("result", jsonString "failed")
        , ("stage", jsonString currentStage)
        , ("observed", jsonString currentObserved)
        ]
    )
  exitFailure

successJson :: String -> [(String, String)] -> String
successJson currentStage currentFields =
  jsonObject
    ( [ ("schema", jsonString "myframework-core-promotion-tool.v1")
      , ("result", jsonString "passed")
      , ("stage", jsonString currentStage)
      ]
        ++ [ (currentName, jsonString currentValue)
           | (currentName, currentValue) <- currentFields
           ]
    )

jsonObject :: [(String, String)] -> String
jsonObject currentFields =
  "{"
    ++ intercalate
      ","
      [ jsonString currentName ++ ":" ++ currentValue
      | (currentName, currentValue) <- currentFields
      ]
    ++ "}"

jsonString :: String -> String
jsonString currentValue =
  show currentValue
