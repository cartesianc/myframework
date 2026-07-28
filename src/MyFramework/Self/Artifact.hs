module MyFramework.Self.Artifact
  ( ArtifactError (..)
  , ArtifactFile (..)
  , ArtifactFileClass (..)
  , ArtifactManifest (..)
  , OutputDir (..)
  , artifactSourceFiles
  , decodeArtifactManifest
  , materializeStage
  , materializeStageFrom
  , renderArtifactManifestJson
  , trustedSeedFiles
  , validateArtifactSourceText
  , verifyArtifactManifest
  ) where

import Control.Exception
  ( IOException
  , try
  )
import Control.Monad
  ( forM
  )
import Data.Char
  ( ord )
import Data.List
  ( intercalate
  , isInfixOf
  , sortOn
  )
import Numeric
  ( showHex )
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , makeAbsolute
  )
import System.FilePath
  ( normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )
import Text.Read
  ( readMaybe )

import MyFramework.Self.ControlTrace
  ( compileControlTrace
  , renderControlTraceJson
  )
import MyFramework.Self.Model
  ( SelfModel
  , canonicalSelfModelJson
  )
import MyFramework.TrustBase.Digest
  ( sha256 )

newtype OutputDir = OutputDir
  { unOutputDir :: FilePath
  }
  deriving (Eq, Ord, Read, Show)

data ArtifactFileClass
  = GeneratedFile
  | TrustedSeedFile
  deriving (Eq, Ord, Read, Show)

data ArtifactFile = ArtifactFile
  { artifactFilePath :: FilePath
  , artifactFileDigest :: String
  , artifactFileOrigin :: String
  , artifactFileClass :: ArtifactFileClass
  }
  deriving (Eq, Ord, Read, Show)

data ArtifactManifest = ArtifactManifest
  { artifactManifestSchema :: String
  , artifactManifestStageName :: String
  , artifactManifestSelfModelDigest :: String
  , artifactManifestControlTraceDigest :: String
  , artifactManifestPayloadDigest :: String
  , artifactManifestFiles :: [ArtifactFile]
  }
  deriving (Eq, Ord, Read, Show)

data ArtifactError
  = ArtifactOutputOutsideGeneratedRoot FilePath FilePath
  | ArtifactOutputAlreadyExists FilePath
  | ArtifactSourceMissing FilePath
  | ArtifactSourceReadFailed FilePath String
  | ArtifactWriteFailed FilePath String
  | ArtifactAbsoluteSourceLeak FilePath
  | ArtifactParentDependency FilePath
  | ArtifactControlTraceFailed String
  | ArtifactManifestDecodeFailed FilePath
  | ArtifactManifestFileMissing FilePath
  | ArtifactManifestDigestMismatch FilePath String String
  | ArtifactManifestPayloadMismatch String String
  deriving (Eq, Ord, Read, Show)

artifactManifestSchemaV1 :: String
artifactManifestSchemaV1 =
  "myframework-self-artifact-manifest.v1"

generatedModelPath :: FilePath
generatedModelPath =
  "src/MyFramework/Generated/SelfModel.hs"

manifestJsonPath :: FilePath
manifestJsonPath =
  "artifact-manifest.json"

manifestReadPath :: FilePath
manifestReadPath =
  "artifact-manifest.read"

-- | Explicit package closure. It intentionally excludes docs, .git, build
-- products and generated files. Generated files are recreated from SelfModel.
artifactSourceFiles :: [FilePath]
artifactSourceFiles =
  [ "LICENSE"
  , "cabal.project"
  , "myframework.cabal"
  , "stack.yaml"
  , "app/CurdeRuntimeWitness.hs"
  , "app/CurdeSemanticsWitness.hs"
  , "app/CoreSelfInterpretWitness.hs"
  , "app/CorePromotionWitness.hs"
  , "app/CorePromotionTool.hs"
  , "app/SdkLowerTool.hs"
  , "app/SdkPackageWitness.hs"
  , "app/SelfArtifactTool.hs"
  , "app/TrustBaseBindingWitness.hs"
  , "src/MyFramework.hs"
  , "src/MyFramework/Ast.hs"
  , "src/MyFramework/SDK/Package.hs"
  , "src/MyFramework/Ast/Layout.hs"
  , "src/MyFramework/Control.hs"
  , "src/MyFramework/CURDE.hs"
  , "src/MyFramework/CURDE/Core.hs"
  , "src/MyFramework/CURDE/Evidence.hs"
  , "src/MyFramework/CURDE/Expression.hs"
  , "src/MyFramework/CURDE/Lowering.hs"
  , "src/MyFramework/CURDE/Record.hs"
  , "src/MyFramework/CURDE/Types.hs"
  , "src/MyFramework/CURDE/Validate.hs"
  , "src/MyFramework/Handler.hs"
  , "src/MyFramework/Handler/Internal.hs"
  , "src/MyFramework/Legacy/Migration.hs"
  , "src/MyFramework/Recursion.hs"
  , "src/MyFramework/Runtime.hs"
  , "src/MyFramework/Runtime/Branch.hs"
  , "src/MyFramework/Runtime/Cancellation.hs"
  , "src/MyFramework/Runtime/Concurrent.hs"
  , "src/MyFramework/Runtime/Control.hs"
  , "src/MyFramework/Runtime/Demand.hs"
  , "src/MyFramework/Runtime/Demand/Control.hs"
  , "src/MyFramework/Runtime/Demand/Delta.hs"
  , "src/MyFramework/Runtime/Diagnosis.hs"
  , "src/MyFramework/Runtime/Expression.hs"
  , "src/MyFramework/Runtime/Observation.hs"
  , "src/MyFramework/Runtime/State.hs"
  , "src/MyFramework/Runtime/Types.hs"
  , "src/MyFramework/Runtime/Value.hs"
  , "src/MyFramework/SDK/SourceArtifact.hs"
  , "src/MyFramework/Self/Artifact.hs"
  , "src/MyFramework/Self/CoreModel.hs"
  , "src/MyFramework/Self/ControlTrace.hs"
  , "src/MyFramework/Self/Model.hs"
  , "src/MyFramework/TrustBase/Core.hs"
  , "src/MyFramework/TrustBase/Digest.hs"
  , "src/MyFramework/TrustBase/Evidence.hs"
  , "src/MyFramework/TrustBase/FixedPoint.hs"
  , "src/MyFramework/TrustBase/Json.hs"
  , "src/MyFramework/TrustBase/Manifest.hs"
  , "src/MyFramework/TrustBase/Promotion.hs"
  , "src/MyFramework/TrustBase/Self.hs"
  , "src/MyFramework/TrustBase/Types.hs"
  ]

trustedSeedFiles :: [FilePath]
trustedSeedFiles =
  [ "app/SelfArtifactTool.hs"
  , "src/MyFramework/Handler.hs"
  , "src/MyFramework/Handler/Internal.hs"
  , "src/MyFramework/Recursion.hs"
  , "src/MyFramework/Runtime.hs"
  , "src/MyFramework/Runtime/Branch.hs"
  , "src/MyFramework/Runtime/Cancellation.hs"
  , "src/MyFramework/Runtime/Concurrent.hs"
  , "src/MyFramework/Runtime/Control.hs"
  , "src/MyFramework/Runtime/Demand.hs"
  , "src/MyFramework/Runtime/Demand/Control.hs"
  , "src/MyFramework/Runtime/Demand/Delta.hs"
  , "src/MyFramework/Runtime/Diagnosis.hs"
  , "src/MyFramework/Runtime/Expression.hs"
  , "src/MyFramework/Runtime/Observation.hs"
  , "src/MyFramework/Runtime/State.hs"
  , "src/MyFramework/Runtime/Types.hs"
  , "src/MyFramework/Runtime/Value.hs"
  , "src/MyFramework/Self/Artifact.hs"
  , "src/MyFramework/TrustBase/Core.hs"
  , "src/MyFramework/TrustBase/Digest.hs"
  ]

materializeStage ::
  OutputDir ->
  SelfModel ->
  IO (Either ArtifactError ArtifactManifest)
materializeStage currentOutput currentModel = do
  currentSource <- getCurrentDirectory
  materializeStageFrom
    currentSource
    currentOutput
    "stage"
    currentModel

materializeStageFrom ::
  FilePath ->
  OutputDir ->
  String ->
  SelfModel ->
  IO (Either ArtifactError ArtifactManifest)
materializeStageFrom sourceRoot (OutputDir outputRoot) stageName currentModel = do
  absoluteSource <- makeAbsolute sourceRoot
  absoluteOutput <- makeAbsolute outputRoot
  case validateOutputRoot absoluteSource absoluteOutput of
    Left currentError ->
      pure (Left currentError)
    Right () -> do
      outputExists <- doesDirectoryExist absoluteOutput
      outputFileExists <- doesFileExist absoluteOutput
      if outputExists || outputFileExists
        then
          pure (Left (ArtifactOutputAlreadyExists absoluteOutput))
        else
          case compileControlTrace currentModel of
            Left currentError ->
              pure
                (Left (ArtifactControlTraceFailed (show currentError)))
            Right currentTrace -> do
              copiedResult <-
                copyArtifactSources
                  absoluteSource
                  absoluteOutput
              case copiedResult of
                Left currentError ->
                  pure (Left currentError)
                Right copiedFiles -> do
                  let modelJson =
                        canonicalSelfModelJson currentModel
                      controlJson =
                        renderControlTraceJson currentTrace
                      generatedSource =
                        renderGeneratedModelModule
                          modelJson
                          controlJson
                  generatedResult <-
                    writeArtifactText
                      absoluteOutput
                      generatedModelPath
                      generatedSource
                  case generatedResult of
                    Left currentError ->
                      pure (Left currentError)
                    Right () -> do
                      let generatedFile =
                            ArtifactFile
                              { artifactFilePath = generatedModelPath
                              , artifactFileDigest = sha256 generatedSource
                              , artifactFileOrigin = "self-model"
                              , artifactFileClass = GeneratedFile
                              }
                          currentFiles =
                            sortOn
                              artifactFilePath
                              (generatedFile : copiedFiles)
                          payloadDigest =
                            digestArtifactFiles currentFiles
                          currentManifest =
                            ArtifactManifest
                              { artifactManifestSchema =
                                  artifactManifestSchemaV1
                              , artifactManifestStageName = stageName
                              , artifactManifestSelfModelDigest =
                                  sha256 modelJson
                              , artifactManifestControlTraceDigest =
                                  sha256 controlJson
                              , artifactManifestPayloadDigest =
                                  payloadDigest
                              , artifactManifestFiles = currentFiles
                              }
                      jsonResult <-
                        writeArtifactText
                          absoluteOutput
                          manifestJsonPath
                          (renderArtifactManifestJson currentManifest)
                      readResult <-
                        writeArtifactText
                          absoluteOutput
                          manifestReadPath
                          (show currentManifest)
                      pure
                        ( case (jsonResult, readResult) of
                            (Left currentError, _) ->
                              Left currentError
                            (_, Left currentError) ->
                              Left currentError
                            (Right (), Right ()) ->
                              Right currentManifest
                        )

copyArtifactSources ::
  FilePath ->
  FilePath ->
  IO (Either ArtifactError [ArtifactFile])
copyArtifactSources sourceRoot outputRoot =
  collectResults
    <$> forM artifactSourceFiles copyOne
  where
    copyOne currentPath = do
      let sourcePath =
            sourceRoot </> currentPath
      sourceExists <- doesFileExist sourcePath
      if not sourceExists
        then
          pure (Left (ArtifactSourceMissing currentPath))
        else do
          sourceResult <-
            try (readFile sourcePath) :: IO (Either IOException String)
          case sourceResult of
            Left currentError ->
              pure
                ( Left
                    (ArtifactSourceReadFailed currentPath (show currentError))
                )
            Right currentText
              | Left currentError <-
                  validateArtifactSourceText
                    sourceRoot
                    currentPath
                    currentText ->
                  pure (Left currentError)
              | otherwise -> do
                  writeResult <-
                    writeArtifactText outputRoot currentPath currentText
                  pure
                    ( case writeResult of
                        Left currentError ->
                          Left currentError
                        Right () ->
                          Right
                            ArtifactFile
                              { artifactFilePath = currentPath
                              , artifactFileDigest = sha256 currentText
                              , artifactFileOrigin =
                                  "reproduced-source:" ++ currentPath
                              , artifactFileClass =
                                  classifyArtifactFile currentPath
                              }
                    )

writeArtifactText ::
  FilePath ->
  FilePath ->
  String ->
  IO (Either ArtifactError ())
writeArtifactText outputRoot currentPath currentText = do
  let outputPath =
        outputRoot </> currentPath
  createResult <-
    try
      (createDirectoryIfMissing True (takeDirectory outputPath)) ::
      IO (Either IOException ())
  case createResult of
    Left currentError ->
      pure (Left (ArtifactWriteFailed currentPath (show currentError)))
    Right () -> do
      writeResult <-
        try (writeFile outputPath currentText) ::
        IO (Either IOException ())
      pure
        ( case writeResult of
            Left currentError ->
              Left (ArtifactWriteFailed currentPath (show currentError))
            Right () ->
              Right ()
        )

verifyArtifactManifest ::
  FilePath ->
  IO (Either [ArtifactError] ArtifactManifest)
verifyArtifactManifest artifactRoot = do
  manifestResult <-
    decodeArtifactManifest
      <$> readArtifactFile artifactRoot manifestReadPath
  case manifestResult of
    Left currentError ->
      pure (Left [currentError])
    Right currentManifest -> do
      fileResults <-
        forM
          (artifactManifestFiles currentManifest)
          (verifyOne artifactRoot)
      let fileErrors =
            concat fileResults
          expectedPayload =
            digestArtifactFiles
              (artifactManifestFiles currentManifest)
          payloadErrors =
            [ ArtifactManifestPayloadMismatch
                expectedPayload
                (artifactManifestPayloadDigest currentManifest)
            | expectedPayload
                /= artifactManifestPayloadDigest currentManifest
            ]
          allErrors =
            fileErrors ++ payloadErrors
      pure
        ( if null allErrors
            then Right currentManifest
            else Left allErrors
        )

verifyOne :: FilePath -> ArtifactFile -> IO [ArtifactError]
verifyOne artifactRoot currentFile = do
  currentResult <-
    readArtifactFile artifactRoot (artifactFilePath currentFile)
  pure
    ( case currentResult of
        Left currentError ->
          [currentError]
        Right currentText ->
          let observedDigest =
                sha256 currentText
           in [ ArtifactManifestDigestMismatch
                  (artifactFilePath currentFile)
                  (artifactFileDigest currentFile)
                  observedDigest
              | observedDigest /= artifactFileDigest currentFile
              ]
    )

readArtifactFile ::
  FilePath ->
  FilePath ->
  IO (Either ArtifactError String)
readArtifactFile artifactRoot currentPath = do
  let targetPath =
        artifactRoot </> currentPath
  currentExists <- doesFileExist targetPath
  if not currentExists
    then
      pure (Left (ArtifactManifestFileMissing currentPath))
    else do
      currentResult <-
        try (readFile targetPath) ::
        IO (Either IOException String)
      pure
        ( case currentResult of
            Left currentError ->
              Left
                (ArtifactSourceReadFailed currentPath (show currentError))
            Right currentText ->
              Right currentText
        )

decodeArtifactManifest ::
  Either ArtifactError String ->
  Either ArtifactError ArtifactManifest
decodeArtifactManifest currentResult =
  case currentResult of
    Left currentError ->
      Left currentError
    Right currentText ->
      case readMaybe currentText of
        Nothing ->
          Left (ArtifactManifestDecodeFailed manifestReadPath)
        Just currentManifest ->
          Right currentManifest

validateOutputRoot ::
  FilePath ->
  FilePath ->
  Either ArtifactError ()
validateOutputRoot sourceRoot outputRoot
  | not (isDirectGeneratedChild sourceRoot outputRoot) =
      Left
        (ArtifactOutputOutsideGeneratedRoot sourceRoot outputRoot)
  | otherwise =
      Right ()

isDirectGeneratedChild :: FilePath -> FilePath -> Bool
isDirectGeneratedChild sourceRoot outputRoot =
  splitDirectories (normalise (takeDirectory outputRoot))
    == splitDirectories (normalise (sourceRoot </> ".generated"))

-- | Pure source-boundary check shared by the materializer and TrustBase
-- mutation witness. It makes the isolation rule testable without creating a
-- second materialization path.
validateArtifactSourceText ::
  FilePath ->
  FilePath ->
  String ->
  Either ArtifactError ()
validateArtifactSourceText sourceRoot currentPath currentText
  | normalise sourceRoot `isInfixOf` normalise currentText =
      Left (ArtifactAbsoluteSourceLeak currentPath)
  | isConfigPath currentPath && configUsesParent currentText =
      Left (ArtifactParentDependency currentPath)
  | otherwise =
      Right ()

classifyArtifactFile :: FilePath -> ArtifactFileClass
classifyArtifactFile currentPath
  | currentPath `elem` trustedSeedFiles =
      TrustedSeedFile
  | otherwise =
      GeneratedFile

digestArtifactFiles :: [ArtifactFile] -> String
digestArtifactFiles currentFiles =
  sha256
    ( intercalate
        "\n"
        [ intercalate
            "|"
            [ artifactFilePath currentFile
            , artifactFileDigest currentFile
            , show (artifactFileClass currentFile)
            , artifactFileOrigin currentFile
            ]
        | currentFile <- sortOn artifactFilePath currentFiles
        ]
    )

collectResults ::
  [Either ArtifactError item] ->
  Either ArtifactError [item]
collectResults currentResults =
  case
      [ currentError
      | Left currentError <- currentResults
      ] of
    currentError : _ ->
      Left currentError
    [] ->
      Right
        [ currentItem
        | Right currentItem <- currentResults
        ]

isConfigPath :: FilePath -> Bool
isConfigPath currentPath =
  currentPath
    `elem`
      [ "stack.yaml"
      , "cabal.project"
      , "myframework.cabal"
      ]

configUsesParent :: String -> Bool
configUsesParent currentText =
  any
    (`isInfixOf` currentText)
    [ "packages: .."
    , "packages:\n  - .."
    , "packages:\r\n  - .."
    , "source-dirs: .."
    , "hs-source-dirs: .."
    ]

renderGeneratedModelModule :: String -> String -> String
renderGeneratedModelModule modelJson controlJson =
  unlines
    [ "module MyFramework.Generated.SelfModel"
    , "  ( generatedSelfModelJson"
    , "  , generatedControlTraceJson"
    , "  ) where"
    , ""
    , "generatedSelfModelJson :: String"
    , "generatedSelfModelJson ="
    , "  " ++ show modelJson
    , ""
    , "generatedControlTraceJson :: String"
    , "generatedControlTraceJson ="
    , "  " ++ show controlJson
    ]

renderArtifactManifestJson :: ArtifactManifest -> String
renderArtifactManifestJson currentManifest =
  jsonObject
    [ ("schema", jsonString (artifactManifestSchema currentManifest))
    , ("stage", jsonString (artifactManifestStageName currentManifest))
    , ( "selfModelDigest"
      , jsonString (artifactManifestSelfModelDigest currentManifest)
      )
    , ( "controlTraceDigest"
      , jsonString (artifactManifestControlTraceDigest currentManifest)
      )
    , ( "payloadDigest"
      , jsonString (artifactManifestPayloadDigest currentManifest)
      )
    , ( "files"
      , jsonArray
          (map renderArtifactFileJson (artifactManifestFiles currentManifest))
      )
    ]

renderArtifactFileJson :: ArtifactFile -> String
renderArtifactFileJson currentFile =
  jsonObject
    [ ("path", jsonString (artifactFilePath currentFile))
    , ("sha256", jsonString (artifactFileDigest currentFile))
    , ("origin", jsonString (artifactFileOrigin currentFile))
    , ("class", jsonString (renderArtifactClass (artifactFileClass currentFile)))
    ]

renderArtifactClass :: ArtifactFileClass -> String
renderArtifactClass currentClass =
  case currentClass of
    GeneratedFile -> "generated"
    TrustedSeedFile -> "trusted-seed"

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
