module MyFramework.SDK.Package
  ( SdkPackageError (..)
  , SdkPackageFile (..)
  , SdkPackageManifest (..)
  , materializeApprovedSdkPackage
  , renderSdkPackageManifestJson
  , verifySdkPackage
  ) where

import Control.Exception
  ( IOException
  , try
  )
import Control.Monad
  ( forM )
import Data.Char
  ( ord )
import Data.List
  ( intercalate
  , sortOn
  )
import Numeric
  ( showHex )
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath
  ( takeDirectory
  , (</>)
  )
import Text.Read
  ( readMaybe )

import MyFramework.SDK.SourceArtifact
  ( SdkGeneratedSource (..)
  , buildSdkSourceReport
  , handlerCoverageFromIds
  , mkSdkSourceInput
  , renderGeneratedSdkSource
  , renderSdkSourceReportJson
  , sdkSourceCoreLock
  , sdkSourceReportReady
  )
import MyFramework.Self.Artifact
  ( ArtifactManifest (..)
  , OutputDir (..)
  , materializeStageFrom
  , verifyArtifactManifest
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
  ( Digest (..)
  , SdkCoreLock (..)
  , TrustBaseRef (..)
  , decodeSdkCoreLock
  , encodeSdkCoreLock
  , validateSdkCoreLock
  )
import MyFramework.TrustBase.Digest
  ( sha256 )
import MyFramework.TrustBase.Promotion
  ( CoreManifest
  , CurrentCorePointer (..)
  , PromotionRecord
  , decodeCoreManifest
  , decodeCurrentCorePointer
  , decodePromotionRecord
  , renderCoreManifestJson
  , renderCurrentCorePointerJson
  , renderPromotionRecordJson
  , validateCurrentCorePointer
  )

data SdkPackageFile = SdkPackageFile
  { sdkPackageFilePath :: FilePath
  , sdkPackageFileDigest :: String
  }
  deriving (Eq, Ord, Read, Show)

data SdkPackageManifest = SdkPackageManifest
  { sdkPackageManifestSchema :: String
  , sdkPackageManifestVersion :: String
  , sdkPackageManifestCoreLock :: String
  , sdkPackageManifestBaseArtifactDigest :: String
  , sdkPackageManifestFiles :: [SdkPackageFile]
  }
  deriving (Eq, Ord, Read, Show)

data SdkPackageError
  = SdkPackagePromotionRejected [String]
  | SdkPackageFrameworkModelFailed String
  | SdkPackageSelfModelFailed String
  | SdkPackageSourceBlocked String
  | SdkPackageBaseMaterializationFailed String
  | SdkPackageWriteFailed FilePath String
  | SdkPackageReadFailed FilePath String
  | SdkPackageManifestDecodeFailed
  | SdkPackageBaseVerificationFailed String
  | SdkPackageBaseDigestMismatch String String
  | SdkPackageApprovedArtifactMismatch String String
  | SdkPackageFileMissing FilePath
  | SdkPackageFileDigestMismatch FilePath String String
  | SdkPackageCoreLockDecodeFailed String
  | SdkPackageCoreLockInvalid String
  | SdkPackageCoreLockMismatch
  | SdkPackageEmbeddedPromotionInvalid [String]
  deriving (Eq, Ord, Show)

sdkPackageManifestSchemaV1 :: String
sdkPackageManifestSchemaV1 =
  "myframework-sdk-package-manifest.v1"

sdkPackageManifestReadPath :: FilePath
sdkPackageManifestReadPath =
  "sdk-package-manifest.read"

sdkPackageManifestJsonPath :: FilePath
sdkPackageManifestJsonPath =
  "sdk-package-manifest.json"

sdkCoreLockPath :: FilePath
sdkCoreLockPath =
  "sdk-core-lock.read"

sdkSourceReportPath :: FilePath
sdkSourceReportPath =
  "sdk-source-report.json"

embeddedCurrentPath :: FilePath
embeddedCurrentPath =
  "trustbase/current.json"

embeddedCoreManifestPath :: FilePath
embeddedCoreManifestPath =
  "trustbase/core-manifest.json"

embeddedPromotionPath :: FilePath
embeddedPromotionPath =
  "trustbase/promotion.approved.json"

materializeApprovedSdkPackage ::
  FilePath ->
  OutputDir ->
  String ->
  CoreManifest ->
  PromotionRecord ->
  CurrentCorePointer ->
  IO (Either SdkPackageError SdkPackageManifest)
materializeApprovedSdkPackage
  sourceRoot
  outputDir@(OutputDir outputRoot)
  sdkVersion
  coreManifest
  promotionRecord
  currentPointer =
    case promotionErrors of
      currentErrors@(_ : _) ->
        pure (Left (SdkPackagePromotionRejected currentErrors))
      [] ->
        case (frameworkAsBusiness, selfModel) of
          (Left currentErrors, _) ->
            pure
              (Left (SdkPackageFrameworkModelFailed (show currentErrors)))
          (_, Left currentErrors) ->
            pure
              (Left (SdkPackageSelfModelFailed (show currentErrors)))
          (Right currentFramework, Right currentSelfModel) -> do
            let currentInput =
                  mkSdkSourceInput
                    (currentCoreRef currentPointer)
                    sdkVersion
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
            if not (sdkSourceReportReady currentReport)
              then
                pure
                  ( Left
                      ( SdkPackageSourceBlocked
                          (renderSdkSourceReportJson currentReport)
                      )
                  )
              else do
                baseResult <-
                  materializeStageFrom
                    sourceRoot
                    outputDir
                    "sdk-beta"
                    currentSelfModel
                case baseResult of
                  Left currentError ->
                    pure
                      ( Left
                          (SdkPackageBaseMaterializationFailed (show currentError))
                      )
                  Right baseManifest
                    | trustBaseArtifactDigest
                        (currentCoreRef currentPointer)
                        /= Digest
                          (artifactManifestPayloadDigest baseManifest) ->
                        pure
                          ( Left
                              ( SdkPackageApprovedArtifactMismatch
                                  (show (trustBaseArtifactDigest (currentCoreRef currentPointer)))
                                  (artifactManifestPayloadDigest baseManifest)
                              )
                          )
                    | otherwise -> do
                    let generatedSource =
                          renderGeneratedSdkSource currentInput
                        extraPayloads =
                          [ ( sdkGeneratedSourcePath generatedSource
                            , unlines
                                (sdkGeneratedSourceLines generatedSource)
                            )
                          , ( sdkCoreLockPath
                            , encodeSdkCoreLock
                                (sdkSourceCoreLock currentInput)
                            )
                          , ( sdkSourceReportPath
                            , renderSdkSourceReportJson currentReport
                            )
                          , ( embeddedCurrentPath
                            , renderCurrentCorePointerJson currentPointer
                            )
                          , ( embeddedCoreManifestPath
                            , renderCoreManifestJson coreManifest
                            )
                          , ( embeddedPromotionPath
                            , renderPromotionRecordJson promotionRecord
                            )
                          ]
                    writeResult <-
                      writePayloads outputRoot extraPayloads
                    case writeResult of
                      Left currentError ->
                        pure (Left currentError)
                      Right currentFiles -> do
                        let packageManifest =
                              SdkPackageManifest
                                { sdkPackageManifestSchema =
                                    sdkPackageManifestSchemaV1
                                , sdkPackageManifestVersion = sdkVersion
                                , sdkPackageManifestCoreLock =
                                    encodeSdkCoreLock
                                      (sdkSourceCoreLock currentInput)
                                , sdkPackageManifestBaseArtifactDigest =
                                    artifactManifestPayloadDigest baseManifest
                                , sdkPackageManifestFiles =
                                    sortOn
                                      sdkPackageFilePath
                                      currentFiles
                                }
                        manifestWrite <-
                          writePayloads
                            outputRoot
                            [ ( sdkPackageManifestReadPath
                              , show packageManifest
                              )
                            , ( sdkPackageManifestJsonPath
                              , renderSdkPackageManifestJson packageManifest
                              )
                            ]
                        pure
                          ( case manifestWrite of
                              Left currentError ->
                                Left currentError
                              Right _ ->
                                Right packageManifest
                          )
  where
    promotionErrors =
      map show
        ( validateCurrentCorePointer
            coreManifest
            promotionRecord
            currentPointer
        )

verifySdkPackage ::
  FilePath ->
  IO (Either [SdkPackageError] SdkPackageManifest)
verifySdkPackage packageRoot = do
  baseResult <- verifyArtifactManifest packageRoot
  manifestTextResult <-
    readPackageText packageRoot sdkPackageManifestReadPath
  case (baseResult, manifestTextResult) of
    (Left currentErrors, _) ->
      pure
        (Left [SdkPackageBaseVerificationFailed (show currentErrors)])
    (_, Left currentError) ->
      pure (Left [currentError])
    (Right baseManifest, Right manifestText) ->
      case readMaybe manifestText of
        Nothing ->
          pure (Left [SdkPackageManifestDecodeFailed])
        Just packageManifest -> do
          fileErrors <-
            concat
              <$> forM
                (sdkPackageManifestFiles packageManifest)
                (verifyPackageFile packageRoot)
          embeddedErrors <-
            verifyEmbeddedPromotion
              packageRoot
              packageManifest
          let schemaErrors =
                [ SdkPackageManifestDecodeFailed
                | sdkPackageManifestSchema packageManifest
                    /= sdkPackageManifestSchemaV1
                ]
              baseErrors =
                [ SdkPackageBaseDigestMismatch
                    (sdkPackageManifestBaseArtifactDigest packageManifest)
                    (artifactManifestPayloadDigest baseManifest)
                | sdkPackageManifestBaseArtifactDigest packageManifest
                    /= artifactManifestPayloadDigest baseManifest
                ]
              allErrors =
                schemaErrors
                  ++ baseErrors
                  ++ fileErrors
                  ++ embeddedErrors
          pure
            ( if null allErrors
                then Right packageManifest
                else Left allErrors
            )

verifyEmbeddedPromotion ::
  FilePath ->
  SdkPackageManifest ->
  IO [SdkPackageError]
verifyEmbeddedPromotion packageRoot packageManifest = do
  lockResult <- readPackageText packageRoot sdkCoreLockPath
  currentResult <- readPackageText packageRoot embeddedCurrentPath
  coreResult <- readPackageText packageRoot embeddedCoreManifestPath
  promotionResult <- readPackageText packageRoot embeddedPromotionPath
  pure
    ( case (lockResult, currentResult, coreResult, promotionResult) of
        (Right lockText, Right currentText, Right coreText, Right promotionText) ->
          case
              ( decodeSdkCoreLock lockText
              , decodeCurrentCorePointer currentText
              , decodeCoreManifest coreText
              , decodePromotionRecord promotionText
              )
            of
              (Right currentLock, Right currentPointer, Right coreManifest, Right promotionRecord) ->
                map
                  (SdkPackageCoreLockInvalid . show)
                  (validateSdkCoreLock currentLock)
                  ++ [ SdkPackageCoreLockMismatch
                     | encodeSdkCoreLock currentLock
                         /= sdkPackageManifestCoreLock packageManifest
                         || sdkCoreRef currentLock
                           /= currentCoreRef currentPointer
                         || trustBaseArtifactDigest
                              (sdkCoreRef currentLock)
                           /= Digest
                             (sdkPackageManifestBaseArtifactDigest packageManifest)
                     ]
                  ++ [ SdkPackageEmbeddedPromotionInvalid currentErrors
                     | let currentErrors =
                             map show
                               ( validateCurrentCorePointer
                                   coreManifest
                                   promotionRecord
                                   currentPointer
                               )
                     , not (null currentErrors)
                     ]
              (Left currentError, _, _, _) ->
                [SdkPackageCoreLockDecodeFailed (show currentError)]
              currentFailure ->
                [SdkPackageEmbeddedPromotionInvalid [show currentFailure]]
        currentFailure ->
          [SdkPackageEmbeddedPromotionInvalid [show currentFailure]]
    )

writePayloads ::
  FilePath ->
  [(FilePath, String)] ->
  IO (Either SdkPackageError [SdkPackageFile])
writePayloads outputRoot currentPayloads =
  collectWrites
    <$> mapM writeOne currentPayloads
  where
    writeOne (currentPath, currentText) = do
      let outputPath =
            outputRoot </> currentPath
      createResult <-
        try
          (createDirectoryIfMissing True (takeDirectory outputPath)) ::
          IO (Either IOException ())
      case createResult of
        Left currentError ->
          pure
            (Left (SdkPackageWriteFailed currentPath (show currentError)))
        Right () -> do
          writeResult <-
            try (writeFile outputPath currentText) ::
            IO (Either IOException ())
          pure
            ( case writeResult of
                Left currentError ->
                  Left
                    (SdkPackageWriteFailed currentPath (show currentError))
                Right () ->
                  Right
                    SdkPackageFile
                      { sdkPackageFilePath = currentPath
                      , sdkPackageFileDigest = sha256 currentText
                      }
            )

collectWrites ::
  [Either SdkPackageError SdkPackageFile] ->
  Either SdkPackageError [SdkPackageFile]
collectWrites currentResults =
  case [currentError | Left currentError <- currentResults] of
    currentError : _ ->
      Left currentError
    [] ->
      Right [currentFile | Right currentFile <- currentResults]

verifyPackageFile ::
  FilePath ->
  SdkPackageFile ->
  IO [SdkPackageError]
verifyPackageFile packageRoot currentFile = do
  currentResult <-
    readPackageText packageRoot (sdkPackageFilePath currentFile)
  pure
    ( case currentResult of
        Left _ ->
          [SdkPackageFileMissing (sdkPackageFilePath currentFile)]
        Right currentText ->
          let observedDigest =
                sha256 currentText
           in [ SdkPackageFileDigestMismatch
                  (sdkPackageFilePath currentFile)
                  (sdkPackageFileDigest currentFile)
                  observedDigest
              | observedDigest
                  /= sdkPackageFileDigest currentFile
              ]
    )

readPackageText ::
  FilePath ->
  FilePath ->
  IO (Either SdkPackageError String)
readPackageText packageRoot currentPath = do
  let targetPath =
        packageRoot </> currentPath
  currentExists <- doesFileExist targetPath
  if not currentExists
    then
      pure (Left (SdkPackageFileMissing currentPath))
    else do
      currentResult <-
        try (readFile targetPath) :: IO (Either IOException String)
      pure
        ( case currentResult of
            Left currentError ->
              Left
                (SdkPackageReadFailed currentPath (show currentError))
            Right currentText ->
              Right currentText
        )

renderSdkPackageManifestJson :: SdkPackageManifest -> String
renderSdkPackageManifestJson currentManifest =
  jsonObject
    [ ("schema", jsonString (sdkPackageManifestSchema currentManifest))
    , ("sdkVersion", jsonString (sdkPackageManifestVersion currentManifest))
    , ("coreLock", jsonString (sdkPackageManifestCoreLock currentManifest))
    , ( "baseArtifactDigest"
      , jsonString (sdkPackageManifestBaseArtifactDigest currentManifest)
      )
    , ( "files"
      , jsonArray
          [ jsonObject
              [ ("path", jsonString (sdkPackageFilePath currentFile))
              , ("sha256", jsonString (sdkPackageFileDigest currentFile))
              ]
          | currentFile <- sdkPackageManifestFiles currentManifest
          ]
      )
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
