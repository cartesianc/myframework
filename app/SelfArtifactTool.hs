module Main (main) where

import Control.Monad
  ( unless )
import Data.Char
  ( ord )
import Data.List
  ( intercalate )
import Numeric
  ( showHex )
import System.Directory
  ( getCurrentDirectory )
import System.Environment
  ( getArgs )
import System.Exit
  ( exitFailure )

import MyFramework.Self.Artifact
  ( ArtifactManifest (..)
  , OutputDir (..)
  , materializeStageFrom
  , renderArtifactManifestJson
  , verifyArtifactManifest
  )
import MyFramework.Self.ControlTrace
  ( compileControlTrace
  , controlTraceConstructorWitness
  , controlTraceConstructorChecks
  , renderControlTraceJson
  )
import MyFramework.Self.Model
  ( canonicalSelfModelJson
  , selfModel
  , selfModelJsonRoundTrip
  , selfModelTextRoundTrip
  )
import MyFramework.TrustBase.Digest
  ( sha256
  , sha256VectorsValid
  )
import MyFramework.TrustBase.FixedPoint
  ( FixedPointReport (..)
  , buildFixedPointReport
  , collectStageEvidence
  , fixedPointPassed
  , renderStageEvidenceJson
  )
import MyFramework.TrustBase.Self
  ( renderTrustBaseSelfReportJson
  , validateSelfTrustBase
  )

main :: IO ()
main = do
  currentArgs <- getArgs
  case currentArgs of
    ["report"] ->
      runReport
    ["trust-base", sourceRoot] ->
      runTrustBase sourceRoot
    ["materialize", stageName, outputRoot] ->
      runMaterialize stageName outputRoot
    ["verify", artifactRoot] ->
      runVerify artifactRoot
    ["compare", leftRoot, rightRoot] ->
      runCompare leftRoot rightRoot
    ["evidence", stageName, semanticPath, runtimePath, artifactRoot] ->
      runEvidence stageName semanticPath runtimePath artifactRoot
    [ "fixed-point"
      , leftName
      , leftSemantic
      , leftRuntime
      , leftArtifact
      , rightName
      , rightSemantic
      , rightRuntime
      , rightArtifact
      ] ->
        runEvidenceFixedPoint
          leftName
          leftSemantic
          leftRuntime
          leftArtifact
          rightName
          rightSemantic
          rightRuntime
          rightArtifact
    _ -> do
      putStrLn
        ( failureJson
            "arguments"
            "expected report | trust-base ROOT | materialize STAGE OUTPUT | verify ROOT | compare LEFT RIGHT | evidence STAGE SEMANTIC RUNTIME ARTIFACT | fixed-point LEFT_NAME LEFT_SEM LEFT_RUN LEFT_ART RIGHT_NAME RIGHT_SEM RIGHT_RUN RIGHT_ART"
        )
      exitFailure

runTrustBase :: FilePath -> IO ()
runTrustBase sourceRoot = do
  currentErrors <- validateSelfTrustBase sourceRoot
  putStrLn (renderTrustBaseSelfReportJson currentErrors)
  unless (null currentErrors) exitFailure

runReport :: IO ()
runReport =
  case selfModel of
    Left currentErrors -> do
      putStrLn (failureJson "self-model" (show currentErrors))
      exitFailure
    Right currentModel ->
      case compileControlTrace currentModel of
        Left currentError -> do
          putStrLn (failureJson "control-trace" (show currentError))
          exitFailure
        Right currentTrace -> do
          let modelJson =
                canonicalSelfModelJson currentModel
              controlJson =
                renderControlTraceJson currentTrace
              succeeded =
                sha256VectorsValid
                  && selfModelJsonRoundTrip currentModel
                  && selfModelTextRoundTrip currentModel
                  && controlTraceConstructorWitness
          putStrLn
            ( jsonObject
                [ ("schema", jsonString "myframework-self-model-report.v1")
                , ("result", jsonString (passFail succeeded))
                , ("selfModel", modelJson)
                , ("selfModelDigest", jsonString (sha256 modelJson))
                , ("controlTrace", controlJson)
                , ("controlTraceDigest", jsonString (sha256 controlJson))
                , ("sha256Vectors", jsonBool sha256VectorsValid)
                , ( "selfModelJsonRoundTrip"
                  , jsonBool (selfModelJsonRoundTrip currentModel)
                  )
                , ( "selfModelRoundTrip"
                  , jsonBool (selfModelTextRoundTrip currentModel)
                  )
                , ( "controlConstructorCoverage"
                  , jsonBool controlTraceConstructorWitness
                  )
                , ( "controlConstructorChecks"
                  , jsonArray
                      [ jsonObject
                          [ ("constructor", jsonString currentName)
                          , ("passed", jsonBool currentPassed)
                          ]
                      | (currentName, currentPassed) <-
                          controlTraceConstructorChecks
                      ]
                  )
                ]
            )
          unless succeeded exitFailure

runMaterialize :: String -> FilePath -> IO ()
runMaterialize stageName outputRoot =
  case selfModel of
    Left currentErrors -> do
      putStrLn (failureJson "self-model" (show currentErrors))
      exitFailure
    Right currentModel -> do
      sourceRoot <- getCurrentDirectory
      currentResult <-
        materializeStageFrom
          sourceRoot
          (OutputDir outputRoot)
          stageName
          currentModel
      case currentResult of
        Left currentError -> do
          putStrLn (failureJson "materialize" (show currentError))
          exitFailure
        Right currentManifest ->
          putStrLn (renderArtifactManifestJson currentManifest)

runVerify :: FilePath -> IO ()
runVerify artifactRoot = do
  currentResult <- verifyArtifactManifest artifactRoot
  case currentResult of
    Left currentErrors -> do
      putStrLn (failureJson "verify" (show currentErrors))
      exitFailure
    Right currentManifest ->
      putStrLn
        ( jsonObject
            [ ("schema", jsonString "myframework-self-artifact-verify.v1")
            , ("result", jsonString "passed")
            , ("manifest", renderArtifactManifestJson currentManifest)
            ]
        )

runCompare :: FilePath -> FilePath -> IO ()
runCompare leftRoot rightRoot = do
  leftResult <- verifyArtifactManifest leftRoot
  rightResult <- verifyArtifactManifest rightRoot
  case (leftResult, rightResult) of
    (Right leftManifest, Right rightManifest) -> do
      let modelEqual =
            artifactManifestSelfModelDigest leftManifest
              == artifactManifestSelfModelDigest rightManifest
          controlEqual =
            artifactManifestControlTraceDigest leftManifest
              == artifactManifestControlTraceDigest rightManifest
          payloadEqual =
            artifactManifestPayloadDigest leftManifest
              == artifactManifestPayloadDigest rightManifest
          succeeded =
            modelEqual && controlEqual && payloadEqual
      putStrLn
        ( jsonObject
            [ ("schema", jsonString "myframework-self-artifact-fixed-point.v1")
            , ("result", jsonString (passFail succeeded))
            , ("selfModelDiffs", show (if modelEqual then 0 else 1 :: Int))
            , ("controlTraceDiffs", show (if controlEqual then 0 else 1 :: Int))
            , ("payloadDiffs", show (if payloadEqual then 0 else 1 :: Int))
            , ( "leftPayloadDigest"
              , jsonString (artifactManifestPayloadDigest leftManifest)
              )
            , ( "rightPayloadDigest"
              , jsonString (artifactManifestPayloadDigest rightManifest)
              )
            ]
        )
      unless succeeded exitFailure
    currentFailure -> do
      putStrLn (failureJson "compare" (show currentFailure))
      exitFailure

runEvidence :: String -> FilePath -> FilePath -> FilePath -> IO ()
runEvidence stageName semanticPath runtimePath artifactRoot = do
  currentResult <-
    collectStageEvidence
      stageName
      semanticPath
      runtimePath
      artifactRoot
  case currentResult of
    Left currentErrors -> do
      putStrLn (failureJson "evidence" (show currentErrors))
      exitFailure
    Right currentEvidence ->
      putStrLn
        ( jsonObject
            [ ("schema", jsonString "myframework-stage-evidence.v1")
            , ("result", jsonString "passed")
            , ("evidence", renderStageEvidenceJson currentEvidence)
            ]
        )

runEvidenceFixedPoint ::
  String ->
  FilePath ->
  FilePath ->
  FilePath ->
  String ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO ()
runEvidenceFixedPoint
  leftName
  leftSemantic
  leftRuntime
  leftArtifact
  rightName
  rightSemantic
  rightRuntime
  rightArtifact = do
    leftResult <-
      collectStageEvidence
        leftName
        leftSemantic
        leftRuntime
        leftArtifact
    rightResult <-
      collectStageEvidence
        rightName
        rightSemantic
        rightRuntime
        rightArtifact
    case (leftResult, rightResult) of
      (Right leftEvidence, Right rightEvidence) -> do
        let currentReport =
              buildFixedPointReport leftEvidence rightEvidence
            succeeded =
              fixedPointPassed currentReport
        putStrLn
          ( jsonObject
              [ ("schema", jsonString "myframework-stage-fixed-point.v1")
              , ("result", jsonString (passFail succeeded))
              , ("diffCount", show (length (fixedPointDiffs currentReport)))
              , ("diffs", jsonString (show (fixedPointDiffs currentReport)))
              , ("stage0", renderStageEvidenceJson leftEvidence)
              , ("stage1", renderStageEvidenceJson rightEvidence)
              ]
          )
        unless succeeded exitFailure
      currentFailure -> do
        putStrLn (failureJson "fixed-point" (show currentFailure))
        exitFailure
passFail :: Bool -> String
passFail True =
  "passed"
passFail False =
  "failed"

failureJson :: String -> String -> String
failureJson currentStage currentObserved =
  jsonObject
    [ ("schema", jsonString "myframework-self-artifact-tool.v1")
    , ("result", jsonString "failed")
    , ("stage", jsonString currentStage)
    , ("observed", jsonString currentObserved)
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

jsonBool :: Bool -> String
jsonBool True =
  "true"
jsonBool False =
  "false"

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
