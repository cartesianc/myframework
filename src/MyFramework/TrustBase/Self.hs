module MyFramework.TrustBase.Self
  ( TrustBaseSelfError (..)
  , myFrameworkTrustBaseManifest
  , renderTrustBaseSelfReportJson
  , validateSelfTrustBase
  ) where

import Data.Char
  ( isSpace
  , ord
  )
import Data.List
  ( group
  , intercalate
  , sort
  )
import Numeric
  ( showHex )
import System.Directory
  ( doesFileExist
  , makeAbsolute
  )
import System.FilePath
  ( (</>) )

import MyFramework.Self.Artifact
  ( ArtifactError (..)
  , artifactSourceFiles
  , trustedSeedFiles
  , validateArtifactSourceText
  )
import MyFramework.Self.ControlTrace
  ( compileControlTrace
  , controlTraceConstructorWitness
  )
import MyFramework.Self.Model
  ( selfModel
  , selfModelJsonRoundTrip
  )
import MyFramework.TrustBase.Digest
  ( sha256VectorsValid )
import MyFramework.TrustBase.Evidence
  ( claimCatalogClaims
  , fixedPointDiffClaimCatalog
  , promotionClaimCatalog
  , promotionEvidenceSchemaV1
  , sdkPackageClaimCatalog
  , sdkPackageEvidenceSchemaV1
  , selfInterpretClaimCatalog
  , selfInterpretEvidenceSchemaV1
  , trustBaseBindingClaimCatalog
  , trustBaseBindingEvidenceSchemaV1
  , trustBaseManifestClaimCatalog
  )
import MyFramework.TrustBase.FixedPoint
  ( fixedPointReportSchemaV1
  , fixedPointSummarySchemaV1
  )
import MyFramework.TrustBase.Manifest
  ( ManifestObservation (..)
  , ManifestViolation (..)
  , SchemaCatalogEntry (..)
  , TrustBaseManifest (..)
  , trustBaseManifestSchemaV2
  , validateManifest
  )
import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  )

data TrustBaseSelfError
  = TrustBaseManifestErrors [ManifestViolation]
  | TrustBaseCabalReadFailed FilePath String
  | TrustBaseSourceFilesMissing [FilePath]
  | TrustBaseSourceFilesDuplicated [FilePath]
  | TrustBaseTrustedSeedOutsideClosure [FilePath]
  | TrustBaseSha256VectorsFailed
  | TrustBaseSelfModelFailed String
  | TrustBaseSelfModelJsonRoundTripFailed
  | TrustBaseControlTraceFailed String
  | TrustBaseControlConstructorCoverageFailed
  | TrustBaseNegativeValidatorWitnessFailed [String]
  deriving (Eq, Show)

myFrameworkTrustBaseManifest :: TrustBaseManifest
myFrameworkTrustBaseManifest =
  TrustBaseManifest
    { trustBaseManifestSchema = trustBaseManifestSchemaV2
    , trustBaseManifestName = "myframework-semantic-self-artifact"
    , trustBaseManifestHostBoundary =
        [ "GHC-9.6.7"
        , "Stack"
        , "operating-system"
        , "filesystem"
        , "process-execution"
        ]
    , trustBaseManifestKernelModules =
        [ "MyFramework.Recursion"
        , "MyFramework.Self.Artifact"
        , "MyFramework.Self.CoreModel"
        , "MyFramework.TrustBase.Core"
        , "MyFramework.TrustBase.Digest"
        , "MyFramework.TrustBase.FixedPoint"
        , "MyFramework.TrustBase.Promotion"
        ]
    , trustBaseManifestFacadeModules =
        [ "MyFramework"
        , "MyFramework.Ast"
        , "MyFramework.CURDE"
        , "MyFramework.Handler"
        ]
    , trustBaseManifestSchemas = selfSchemaCatalog
    , trustBaseManifestEvidenceClaims = selfClaimCatalog
    }

selfSchemaCatalog :: [SchemaCatalogEntry]
selfSchemaCatalog =
  [ SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-self-model-report")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "self-artifact-tool report"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-self-artifact-manifest")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "self-artifact-tool materialize"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema = fixedPointReportSchemaV1
      , schemaCatalogEntryProducer = "self-artifact-tool fixed-point"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema = fixedPointSummarySchemaV1
      , schemaCatalogEntryProducer = "self-artifact-tool fixed-point"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema = trustBaseBindingEvidenceSchemaV1
      , schemaCatalogEntryProducer = "trustbase-binding-witness"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema = selfInterpretEvidenceSchemaV1
      , schemaCatalogEntryProducer = "core-self-interpret-witness"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema = promotionEvidenceSchemaV1
      , schemaCatalogEntryProducer = "core-promotion-witness"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-core-manifest")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "core-promotion-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-promotion-record")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "core-promotion-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-current-core")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "core-promotion-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-core-promotion-tool")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "core-promotion-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-sdk-source-artifact")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "sdk-lower-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-sdk-source-report")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "sdk-lower-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-sdk-package-manifest")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "sdk-lower-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema =
          SchemaId
            (SchemaName "myframework-sdk-lower-tool")
            (SchemaVersion 1)
      , schemaCatalogEntryProducer = "sdk-lower-tool"
      }
  , SchemaCatalogEntry
      { schemaCatalogEntrySchema = sdkPackageEvidenceSchemaV1
      , schemaCatalogEntryProducer = "sdk-package-witness"
      }
  ]

selfClaimCatalog :: [ClaimName]
selfClaimCatalog =
  claimCatalogClaims trustBaseManifestClaimCatalog
    ++ claimCatalogClaims fixedPointDiffClaimCatalog
    ++ claimCatalogClaims trustBaseBindingClaimCatalog
    ++ claimCatalogClaims selfInterpretClaimCatalog
    ++ claimCatalogClaims promotionClaimCatalog
    ++ claimCatalogClaims sdkPackageClaimCatalog

validateSelfTrustBase ::
  FilePath ->
  IO [TrustBaseSelfError]
validateSelfTrustBase sourceRoot = do
  absoluteSourceRoot <- makeAbsolute sourceRoot
  cabalResult <-
    readFileResult (absoluteSourceRoot </> "myframework.cabal")
  sourceExistence <-
    mapM
      (\currentPath -> do
          currentExists <-
            doesFileExist (absoluteSourceRoot </> currentPath)
          pure (currentPath, currentExists)
      )
      artifactSourceFiles
  let sourceErrors =
        [ TrustBaseSourceFilesMissing
            [ currentPath
            | (currentPath, False) <- sourceExistence
            ]
        | any (not . snd) sourceExistence
        ]
      duplicateErrors =
        [TrustBaseSourceFilesDuplicated currentDuplicates | not (null currentDuplicates)]
        where
          currentDuplicates =
            duplicates artifactSourceFiles
      seedErrors =
        [ TrustBaseTrustedSeedOutsideClosure currentOutside
        | let currentOutside =
                trustedSeedsOutsideClosure artifactSourceFiles trustedSeedFiles
        , not (null currentOutside)
        ]
      digestErrors =
        [TrustBaseSha256VectorsFailed | not sha256VectorsValid]
      constructorErrors =
        [ TrustBaseControlConstructorCoverageFailed
        | not controlTraceConstructorWitness
        ]
      modelErrors =
        case selfModel of
          Left currentErrors ->
            [TrustBaseSelfModelFailed (show currentErrors)]
          Right currentModel ->
            [ TrustBaseSelfModelJsonRoundTripFailed
            | not (selfModelJsonRoundTrip currentModel)
            ]
              ++ case compileControlTrace currentModel of
                Left currentError ->
                  [TrustBaseControlTraceFailed (show currentError)]
                Right _ ->
                  []
      manifestErrors =
        case cabalResult of
          Left currentError ->
            [currentError]
          Right currentCabal ->
            let currentObservation =
                  ManifestObservation
                    { manifestObservedExposedModules =
                        parseExposedModules currentCabal
                    , manifestObservedCoreSurfaceModules =
                        [ currentModule
                        | currentModule <-
                            trustBaseManifestKernelModules
                              myFrameworkTrustBaseManifest
                              ++ trustBaseManifestFacadeModules
                                myFrameworkTrustBaseManifest
                        , moduleFileExists
                            sourceExistence
                            currentModule
                        ]
                    , manifestObservedSchemas = selfSchemaCatalog
                    , manifestObservedEvidenceClaims = selfClaimCatalog
                    }
                currentViolations =
                  validateManifest
                    myFrameworkTrustBaseManifest
                    currentObservation
                currentNegativeFailures =
                  negativeValidatorFailures
                    absoluteSourceRoot
                    currentObservation
             in [TrustBaseManifestErrors currentViolations | not (null currentViolations)]
                  ++ [ TrustBaseNegativeValidatorWitnessFailed
                         currentNegativeFailures
                     | not (null currentNegativeFailures)
                     ]
  pure
    ( manifestErrors
        ++ sourceErrors
        ++ duplicateErrors
        ++ seedErrors
        ++ digestErrors
        ++ modelErrors
        ++ constructorErrors
    )

trustedSeedsOutsideClosure :: [FilePath] -> [FilePath] -> [FilePath]
trustedSeedsOutsideClosure currentClosure currentSeeds =
  [ currentPath
  | currentPath <- currentSeeds
  , currentPath `notElem` currentClosure
  ]

negativeValidatorFailures ::
  FilePath ->
  ManifestObservation ->
  [String]
negativeValidatorFailures sourceRoot currentObservation =
  concat
    [ [ "empty-claim-accepted"
      | not
          ( any isInvalidClaim
              (validateManifest emptyClaimManifest emptyClaimObservation)
          )
      ]
    , [ "duplicate-claim-accepted"
      | not
          ( any isDuplicateClaim
              (validateManifest duplicateClaimManifest duplicateClaimObservation)
          )
      ]
    , [ "unregistered-seed-accepted"
      | null
          ( trustedSeedsOutsideClosure
              artifactSourceFiles
              (trustedSeedFiles ++ ["src/UnregisteredSeed.hs"])
          )
      ]
    , [ "absolute-source-path-accepted"
      | case
          validateArtifactSourceText
            sourceRoot
            "src/AbsoluteLeak.hs"
            ("sourceRoot = " ++ show sourceRoot)
        of
          Left (ArtifactAbsoluteSourceLeak _) -> False
          _ -> True
      ]
    , [ "parent-config-dependency-accepted"
      | case
          validateArtifactSourceText
            sourceRoot
            "stack.yaml"
            "packages:\n  - ..\n"
        of
          Left (ArtifactParentDependency _) -> False
          _ -> True
      ]
    ]
  where
    emptyClaim =
      ClaimName ""
    emptyClaimManifest =
      myFrameworkTrustBaseManifest
        { trustBaseManifestEvidenceClaims = [emptyClaim]
        }
    emptyClaimObservation =
      currentObservation
        { manifestObservedEvidenceClaims = [emptyClaim]
        }
    duplicateClaim =
      case selfClaimCatalog of
        currentClaim : _ -> currentClaim
        [] -> ClaimName "negative-witness"
    duplicateClaimManifest =
      myFrameworkTrustBaseManifest
        { trustBaseManifestEvidenceClaims =
            [duplicateClaim, duplicateClaim]
        }
    duplicateClaimObservation =
      currentObservation
        { manifestObservedEvidenceClaims =
            [duplicateClaim, duplicateClaim]
        }
    isInvalidClaim currentViolation =
      case currentViolation of
        ManifestInvalidClaims _ -> True
        _ -> False
    isDuplicateClaim currentViolation =
      case currentViolation of
        ManifestDuplicateClaims _ -> True
        _ -> False

readFileResult ::
  FilePath ->
  IO (Either TrustBaseSelfError String)
readFileResult currentPath = do
  currentExists <- doesFileExist currentPath
  if currentExists
    then
      Right <$> readFile currentPath
    else
      pure
        ( Left
            ( TrustBaseCabalReadFailed
                currentPath
                "file does not exist"
            )
        )

parseExposedModules :: String -> [String]
parseExposedModules currentCabal =
  map trim
    ( takeWhile isIndentedModule
        ( drop 1
            (dropWhile (not . isExposedHeader) (lines currentCabal))
        )
    )
  where
    isExposedHeader currentLine =
      trim currentLine == "exposed-modules:"
    isIndentedModule currentLine =
      not (null currentLine)
        && isSpace (head currentLine)
        && "." `elem` words currentLine
          || ( not (null currentLine)
                && isSpace (head currentLine)
                && "MyFramework" `prefixOf` trim currentLine
             )

moduleFileExists :: [(FilePath, Bool)] -> String -> Bool
moduleFileExists sourceExistence currentModule =
  case lookup (modulePath currentModule) sourceExistence of
    Just currentExists ->
      currentExists
    Nothing ->
      False

modulePath :: String -> FilePath
modulePath currentModule =
  "src/" ++ map dotToSlash currentModule ++ ".hs"
  where
    dotToSlash '.' = '/'
    dotToSlash currentChar = currentChar

trim :: String -> String
trim =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace

prefixOf :: String -> String -> Bool
prefixOf [] _ =
  True
prefixOf _ [] =
  False
prefixOf (left : leftRest) (right : rightRest) =
  left == right && prefixOf leftRest rightRest

duplicates :: Ord item => [item] -> [item]
duplicates =
  map head
    . filter ((> 1) . length)
    . group
    . sort

renderTrustBaseSelfReportJson :: [TrustBaseSelfError] -> String
renderTrustBaseSelfReportJson currentErrors =
  jsonObject
    [ ("schema", jsonString "myframework-trust-base-self-report.v1")
    , ( "result"
      , jsonString (if null currentErrors then "passed" else "failed")
      )
    , ( "sourceFileCount"
      , show (length artifactSourceFiles)
      )
    , ( "trustedSeedFileCount"
      , show (length trustedSeedFiles)
      )
    , ("errors", jsonArray (map (jsonString . show) currentErrors))
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
