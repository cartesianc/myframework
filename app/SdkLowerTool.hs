module Main (main) where

import Control.Exception
  ( IOException
  , try
  )
import Data.List
  ( intercalate )
import System.Environment
  ( getArgs )
import System.Exit
  ( exitFailure )

import MyFramework.SDK.Package
import MyFramework.Self.Artifact
  ( OutputDir (..)
  )
import MyFramework.TrustBase.Promotion
  ( decodeCoreManifest
  , decodeCurrentCorePointer
  , decodePromotionRecord
  )

main :: IO ()
main = do
  currentArgs <- getArgs
  case currentArgs of
    [ "materialize"
      , sourceRoot
      , outputRoot
      , sdkVersion
      , coreManifestPath
      , approvedRecordPath
      , currentPointerPath
      ] ->
        runMaterialize
          sourceRoot
          outputRoot
          sdkVersion
          coreManifestPath
          approvedRecordPath
          currentPointerPath
    ["verify", packageRoot] ->
      runVerify packageRoot
    _ ->
      failTool
        "arguments"
        "expected materialize SOURCE_ROOT OUTPUT_ROOT SDK_VERSION CORE_MANIFEST APPROVED_RECORD CURRENT_POINTER | verify PACKAGE_ROOT"

runMaterialize ::
  FilePath ->
  FilePath ->
  String ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO ()
runMaterialize
  sourceRoot
  outputRoot
  sdkVersion
  coreManifestPath
  approvedRecordPath
  currentPointerPath = do
    coreResult <- readDecoded coreManifestPath decodeCoreManifest
    recordResult <- readDecoded approvedRecordPath decodePromotionRecord
    pointerResult <- readDecoded currentPointerPath decodeCurrentCorePointer
    case (coreResult, recordResult, pointerResult) of
      (Right currentCore, Right currentRecord, Right currentPointer) -> do
        currentResult <-
          materializeApprovedSdkPackage
            sourceRoot
            (OutputDir outputRoot)
            sdkVersion
            currentCore
            currentRecord
            currentPointer
        case currentResult of
          Left currentError ->
            failTool "materialize" (show currentError)
          Right currentManifest ->
            putStrLn
              ( jsonObject
                  [ ("schema", jsonString "myframework-sdk-lower-tool.v1")
                  , ("result", jsonString "passed")
                  , ("stage", jsonString "materialize")
                  , ( "manifest"
                    , renderSdkPackageManifestJson currentManifest
                    )
                  ]
              )
      currentFailure ->
        failTool "materialize-input" (show currentFailure)

runVerify :: FilePath -> IO ()
runVerify packageRoot = do
  currentResult <- verifySdkPackage packageRoot
  case currentResult of
    Left currentErrors ->
      failTool "verify" (show currentErrors)
    Right currentManifest ->
      putStrLn
        ( jsonObject
            [ ("schema", jsonString "myframework-sdk-lower-tool.v1")
            , ("result", jsonString "passed")
            , ("stage", jsonString "verify")
            , ("manifest", renderSdkPackageManifestJson currentManifest)
            ]
        )

readDecoded ::
  FilePath ->
  (String -> Either error value) ->
  IO (Either String value)
readDecoded currentPath currentDecoder = do
  currentResult <-
    try (readFile currentPath) :: IO (Either IOException String)
  pure
    ( case currentResult of
        Left currentError ->
          Left (currentPath ++ ": " ++ show currentError)
        Right currentText ->
          case currentDecoder currentText of
            Left _ ->
              Left ("decode failed: " ++ currentPath)
            Right currentValue ->
              Right currentValue
    )

failTool :: String -> String -> IO value
failTool currentStage currentObserved = do
  putStrLn
    ( jsonObject
        [ ("schema", jsonString "myframework-sdk-lower-tool.v1")
        , ("result", jsonString "failed")
        , ("stage", jsonString currentStage)
        , ("observed", jsonString currentObserved)
        ]
    )
  exitFailure

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
jsonString =
  show
