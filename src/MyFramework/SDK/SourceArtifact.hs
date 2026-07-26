module MyFramework.SDK.SourceArtifact
  ( SdkHandlerCoverage
  , SdkSourceInput (..)
  , SdkSourceArtifact
  , sdkSourceArtifactSchema
  , sdkSourceArtifactSdkVersion
  , sdkSourceArtifactEffectSystems
  , sdkSourceArtifactAstSeed
  , sdkSourceArtifactHandlerCoverage
  , SdkSourceIssueSeverity (..)
  , SdkSourceIssueCode (..)
  , SdkSourceIssue (..)
  , SdkSourceReport
  , sdkSourceReportSchema
  , sdkSourceReportInput
  , sdkSourceReportArtifact
  , sdkSourceReportIssues
  , SdkGeneratedSource (..)
  , sdkSourceArtifactSchemaV1
  , sdkSourceReportSchemaV1
  , handlerCoverageFromIds
  , handlerCoverageIds
  , canonicalizeSdkSourceInput
  , buildSdkSourceReport
  , sdkSourceReportReady
  , sdkSourceReportCanonical
  , sdkSourcePreservesAuthoringOrder
  , renderSdkSourceArtifactJson
  , renderSdkSourceReportJson
  , renderGeneratedSdkSource
  ) where

import Data.Char
  ( ord )
import Data.List
  ( group
  , sort
  , sortOn
  )
import qualified Data.Set as Set
import Numeric
  ( showHex )

import MyFramework.Ast
  ( AstBlueprintSeed
  , encodeAstBlueprintSeed
  )
import MyFramework.CURDE
  ( EffectSystemDecl (..)
  , EffectSystemName (..)
  , HandleId (..)
  , effectSystemDeclHandleIds
  , renderHandleId
  )

-- | Handler code and existential registry entries remain runtime-only.
-- Coverage is an optional set of erased identities and is not an authoring
-- surface, handler binding, or execution plan.
newtype SdkHandlerCoverage = SdkHandlerCoverage
  { handlerCoverageIds :: [HandleId]
  }
  deriving (Eq, Ord, Show)

-- | The authoritative SDK source boundary. CURDE and AST values enter through
-- their existing public erased/configuration types; no parallel syntax exists.
data SdkSourceInput = SdkSourceInput
  { sdkSourceSdkVersion :: String
  , sdkSourceEffectSystems :: [EffectSystemDecl]
  , sdkSourceAstSeed :: AstBlueprintSeed
  , sdkSourceHandlerCoverage :: Maybe SdkHandlerCoverage
  }
  deriving (Eq, Ord, Show)

-- | Canonical, typed material retained before rendering. It contains no
-- handler, closure, runtime resource, interpreter, or backend value.
data SdkSourceArtifact = SdkSourceArtifact
  { sdkSourceArtifactSchema :: String
  , sdkSourceArtifactSdkVersion :: String
  , sdkSourceArtifactEffectSystems :: [EffectSystemDecl]
  , sdkSourceArtifactAstSeed :: AstBlueprintSeed
  , sdkSourceArtifactHandlerCoverage :: Maybe SdkHandlerCoverage
  }
  deriving (Eq, Ord, Show)

data SdkSourceIssueSeverity
  = SdkSourceWarning
  | SdkSourceBlocker
  deriving (Eq, Ord, Show)

data SdkSourceIssueCode
  = EmptySdkSourceVersion
  | EmptyEffectSystemIdentity
  | DuplicateEffectSystemIdentity
  | EmptyHandlerCoverageIdentity
  | UnknownHandlerCoverageIdentity
  deriving (Eq, Ord, Show)

data SdkSourceIssue = SdkSourceIssue
  { sdkSourceIssueCode :: SdkSourceIssueCode
  , sdkSourceIssueSeverity :: SdkSourceIssueSeverity
  , sdkSourceIssueSubject :: String
  , sdkSourceIssueDetail :: String
  }
  deriving (Eq, Ord, Show)

data SdkSourceReport = SdkSourceReport
  { sdkSourceReportSchema :: String
  , sdkSourceReportInput :: SdkSourceInput
  , sdkSourceReportArtifact :: SdkSourceArtifact
  , sdkSourceReportIssues :: [SdkSourceIssue]
  }
  deriving (Eq, Ord, Show)

-- | A pure generated-source payload. Writing it to disk belongs to external
-- SDK generation tooling, never to this boundary.
data SdkGeneratedSource = SdkGeneratedSource
  { sdkGeneratedSourcePath :: FilePath
  , sdkGeneratedSourceLines :: [String]
  }
  deriving (Eq, Ord, Show)

sdkSourceArtifactSchemaV1 :: String
sdkSourceArtifactSchemaV1 =
  "myframework-sdk-source-artifact.v1"

sdkSourceReportSchemaV1 :: String
sdkSourceReportSchemaV1 =
  "myframework-sdk-source-report.v1"

handlerCoverageFromIds :: [HandleId] -> SdkHandlerCoverage
handlerCoverageFromIds =
  SdkHandlerCoverage . uniqueSorted

-- | Canonicalization treats only the top-level effect-system collection and
-- handler coverage as sets. Every sequence inside an EffectSystemDecl and the
-- complete AstBlueprintSeed are preserved exactly.
canonicalizeSdkSourceInput :: SdkSourceInput -> SdkSourceInput
canonicalizeSdkSourceInput currentInput =
  currentInput
    { sdkSourceEffectSystems =
        sortOn
          effectSystemCanonicalKey
          (sdkSourceEffectSystems currentInput)
    , sdkSourceHandlerCoverage =
        fmap
          (handlerCoverageFromIds . handlerCoverageIds)
          (sdkSourceHandlerCoverage currentInput)
    }

buildSdkSourceReport :: SdkSourceInput -> SdkSourceReport
buildSdkSourceReport originalInput =
  SdkSourceReport
    { sdkSourceReportSchema = sdkSourceReportSchemaV1
    , sdkSourceReportInput = canonicalInput
    , sdkSourceReportArtifact = sourceArtifactFrom canonicalInput
    , sdkSourceReportIssues = sourceIssues canonicalInput
    }
  where
    canonicalInput =
      canonicalizeSdkSourceInput originalInput

sdkSourceReportReady :: SdkSourceReport -> Bool
sdkSourceReportReady currentReport =
  sdkSourceReportCanonical currentReport
    && not
      ( any
          ((== SdkSourceBlocker) . sdkSourceIssueSeverity)
          (sdkSourceReportIssues currentReport)
      )

-- | Pure witness for generated-code tooling. It proves that the report uses a
-- canonical source value, the exact deterministic artifact, and sorted unique
-- issues rebuilt from that source.
sdkSourceReportCanonical :: SdkSourceReport -> Bool
sdkSourceReportCanonical currentReport =
  sdkSourceReportSchema currentReport == sdkSourceReportSchemaV1
    && currentInput == canonicalizeSdkSourceInput currentInput
    && currentArtifact == sourceArtifactFrom currentInput
    && currentIssues == sourceIssues currentInput
    && currentIssues == uniqueSorted currentIssues
  where
    currentInput =
      sdkSourceReportInput currentReport
    currentArtifact =
      sdkSourceReportArtifact currentReport
    currentIssues =
      sdkSourceReportIssues currentReport

-- | Witness that canonicalization changes only top-level set order. Equality
-- with the rebuilt report preserves every declaration-internal sequence and
-- the complete AST seed from the original authoritative input.
sdkSourcePreservesAuthoringOrder ::
  SdkSourceInput ->
  SdkSourceReport ->
  Bool
sdkSourcePreservesAuthoringOrder originalInput currentReport =
  sdkSourceReportInput currentReport
    == canonicalizeSdkSourceInput originalInput
    && currentReport == buildSdkSourceReport originalInput

renderSdkSourceArtifactJson :: SdkSourceArtifact -> String
renderSdkSourceArtifactJson currentArtifact =
  jsonObject
    [ jsonField
        "schema"
        (jsonString (sdkSourceArtifactSchema currentArtifact))
    , jsonField
        "sdkVersion"
        (jsonString (sdkSourceArtifactSdkVersion currentArtifact))
    , jsonField
        "curde"
        ( jsonArray
            ( map
                renderEffectSystemDeclJson
                (sdkSourceArtifactEffectSystems currentArtifact)
            )
        )
    , jsonField
        "ast"
        (renderAstSeedJson (sdkSourceArtifactAstSeed currentArtifact))
    , jsonField
        "handlerCoverage"
        ( jsonMaybe
            renderHandlerCoverageJson
            (sdkSourceArtifactHandlerCoverage currentArtifact)
        )
    ]

renderSdkSourceReportJson :: SdkSourceReport -> String
renderSdkSourceReportJson currentReport =
  jsonObject
    [ jsonField
        "schema"
        (jsonString (sdkSourceReportSchema currentReport))
    , jsonField
        "status"
        (jsonString (if sdkSourceReportReady currentReport then "ready" else "blocked"))
    , jsonField
        "artifact"
        (renderSdkSourceArtifactJson (sdkSourceReportArtifact currentReport))
    , jsonField
        "issues"
        (jsonArray (map renderSourceIssueJson (sdkSourceReportIssues currentReport)))
    ]

-- | Render a provenance-only module for generated SDK trees. The generated
-- module exposes JSON constants; it cannot reconstruct handlers or execute the
-- AST.
renderGeneratedSdkSource :: SdkSourceInput -> SdkGeneratedSource
renderGeneratedSdkSource currentInput =
  SdkGeneratedSource
    { sdkGeneratedSourcePath =
        "src/MyFramework/Generated/SourceArtifact.hs"
    , sdkGeneratedSourceLines =
        [ "module MyFramework.Generated.SourceArtifact"
        , "  ( sdkSourceArtifactJson"
        , "  , sdkSourceReportJson"
        , "  ) where"
        , ""
        , "sdkSourceArtifactJson :: String"
        , "sdkSourceArtifactJson ="
        , "  " ++ show artifactJson
        , ""
        , "sdkSourceReportJson :: String"
        , "sdkSourceReportJson ="
        , "  " ++ show reportJson
        ]
    }
  where
    currentReport =
      buildSdkSourceReport currentInput
    artifactJson =
      renderSdkSourceArtifactJson (sdkSourceReportArtifact currentReport)
    reportJson =
      renderSdkSourceReportJson currentReport

sourceArtifactFrom :: SdkSourceInput -> SdkSourceArtifact
sourceArtifactFrom currentInput =
  SdkSourceArtifact
    { sdkSourceArtifactSchema = sdkSourceArtifactSchemaV1
    , sdkSourceArtifactSdkVersion = sdkSourceSdkVersion currentInput
    , sdkSourceArtifactEffectSystems =
        sdkSourceEffectSystems currentInput
    , sdkSourceArtifactAstSeed =
        sdkSourceAstSeed currentInput
    , sdkSourceArtifactHandlerCoverage =
        sdkSourceHandlerCoverage currentInput
    }

sourceIssues :: SdkSourceInput -> [SdkSourceIssue]
sourceIssues currentInput =
  uniqueSorted
    ( versionIssues
        ++ effectSystemIssues
        ++ handlerCoverageIssues
    )
  where
    versionIssues =
      [ sourceIssue
          SdkSourceBlocker
          EmptySdkSourceVersion
          "sdkVersion"
          "the generated artifact must identify its SDK semantic version"
      | null (sdkSourceSdkVersion currentInput)
      ]
    currentSystems =
      sdkSourceEffectSystems currentInput
    effectSystemIssues =
      [ sourceIssue
          SdkSourceBlocker
          EmptyEffectSystemIdentity
          "effect-system"
          "an erased EffectSystemDecl has an empty stable identity"
      | currentSystem <- currentSystems
      , null
          ( effectSystemNameText
              (effectSystemDeclName currentSystem)
          )
      ]
        ++
      [ sourceIssue
          SdkSourceBlocker
          DuplicateEffectSystemIdentity
          (effectSystemNameText currentName)
          "more than one erased EffectSystemDecl uses this stable identity"
      | currentName <-
          duplicates (map effectSystemDeclName currentSystems)
      ]
    handlerCoverageIssues =
      case sdkSourceHandlerCoverage currentInput of
        Nothing ->
          []
        Just currentCoverage ->
          invalidCoverageIssues currentCoverage
            ++ unknownCoverageIssues currentCoverage
    declaredIds =
      uniqueSorted (concatMap effectSystemDeclHandleIds currentSystems)
    invalidCoverageIssues currentCoverage =
      [ sourceIssue
          SdkSourceWarning
          EmptyHandlerCoverageIdentity
          (renderHandleId currentId)
          "handler coverage metadata contains an empty handle identity"
      | currentId <- handlerCoverageIds currentCoverage
      , not (handleIdentityValid currentId)
      ]
    unknownCoverageIssues currentCoverage =
      [ sourceIssue
          SdkSourceWarning
          UnknownHandlerCoverageIdentity
          (renderHandleId currentId)
          "handler coverage is metadata only and does not match a declared handle"
      | currentId <- handlerCoverageIds currentCoverage
      , currentId `notElem` declaredIds
      ]

sourceIssue ::
  SdkSourceIssueSeverity ->
  SdkSourceIssueCode ->
  String ->
  String ->
  SdkSourceIssue
sourceIssue currentSeverity currentCode currentSubject currentDetail =
  SdkSourceIssue
    { sdkSourceIssueCode = currentCode
    , sdkSourceIssueSeverity = currentSeverity
    , sdkSourceIssueSubject = currentSubject
    , sdkSourceIssueDetail = currentDetail
    }

handleIdentityValid :: HandleId -> Bool
handleIdentityValid currentId =
  not
    ( null
        ( effectSystemNameText
            (handleIdEffectSystem currentId)
        )
    )
    && not (null (handleIdLocalName currentId))

effectSystemCanonicalKey ::
  EffectSystemDecl ->
  (EffectSystemName, EffectSystemDecl)
effectSystemCanonicalKey currentSystem =
  (effectSystemDeclName currentSystem, currentSystem)

renderEffectSystemDeclJson :: EffectSystemDecl -> String
renderEffectSystemDeclJson currentSystem =
  jsonObject
    [ jsonField
        "identity"
        ( jsonString
            ( effectSystemNameText
                (effectSystemDeclName currentSystem)
            )
        )
    , jsonField
        "encoding"
        (jsonString "EffectSystemDecl.ReadShow.v1")
    , jsonField
        "declaration"
        (jsonString (show currentSystem))
    ]

renderAstSeedJson :: AstBlueprintSeed -> String
renderAstSeedJson currentSeed =
  jsonObject
    [ jsonField
        "encoding"
        (jsonString "AstBlueprintSeed.ReadShow.v1")
    , jsonField
        "seed"
        (jsonString (encodeAstBlueprintSeed currentSeed))
    ]

renderHandlerCoverageJson :: SdkHandlerCoverage -> String
renderHandlerCoverageJson currentCoverage =
  jsonObject
    [ jsonField "kind" (jsonString "identity-only")
    , jsonField
        "handles"
        ( jsonArray
            ( map
                (jsonString . renderHandleId)
                (handlerCoverageIds currentCoverage)
            )
        )
    ]

renderSourceIssueJson :: SdkSourceIssue -> String
renderSourceIssueJson currentIssue =
  jsonObject
    [ jsonField
        "code"
        (jsonString (renderSourceIssueCode (sdkSourceIssueCode currentIssue)))
    , jsonField
        "severity"
        ( jsonString
            (renderSourceIssueSeverity (sdkSourceIssueSeverity currentIssue))
        )
    , jsonField
        "subject"
        (jsonString (sdkSourceIssueSubject currentIssue))
    , jsonField
        "detail"
        (jsonString (sdkSourceIssueDetail currentIssue))
    ]

renderSourceIssueCode :: SdkSourceIssueCode -> String
renderSourceIssueCode currentCode =
  case currentCode of
    EmptySdkSourceVersion ->
      "empty-sdk-source-version"
    EmptyEffectSystemIdentity ->
      "empty-effect-system-identity"
    DuplicateEffectSystemIdentity ->
      "duplicate-effect-system-identity"
    EmptyHandlerCoverageIdentity ->
      "empty-handler-coverage-identity"
    UnknownHandlerCoverageIdentity ->
      "unknown-handler-coverage-identity"

renderSourceIssueSeverity :: SdkSourceIssueSeverity -> String
renderSourceIssueSeverity currentSeverity =
  case currentSeverity of
    SdkSourceWarning -> "warning"
    SdkSourceBlocker -> "blocker"

duplicates :: Ord item => [item] -> [item]
duplicates currentItems =
  [ currentItem
  | currentItem : _ : _ <- group (sort currentItems)
  ]

uniqueSorted :: Ord item => [item] -> [item]
uniqueSorted =
  Set.toAscList . Set.fromList

jsonObject :: [String] -> String
jsonObject fields =
  "{" ++ joinWith "," fields ++ "}"

jsonField :: String -> String -> String
jsonField currentName currentValue =
  jsonString currentName ++ ":" ++ currentValue

jsonArray :: [String] -> String
jsonArray values =
  "[" ++ joinWith "," values ++ "]"

jsonMaybe :: (value -> String) -> Maybe value -> String
jsonMaybe _ Nothing =
  "null"
jsonMaybe renderValue (Just currentValue) =
  renderValue currentValue

jsonString :: String -> String
jsonString currentValue =
  "\"" ++ concatMap jsonChar currentValue ++ "\""

jsonChar :: Char -> String
jsonChar currentChar =
  case currentChar of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\b' -> "\\b"
    '\f' -> "\\f"
    _
      | ord currentChar < 32 ->
          unicodeEscape currentChar
      | otherwise ->
          [currentChar]

unicodeEscape :: Char -> String
unicodeEscape currentChar =
  "\\u"
    ++ replicate (4 - length currentDigits) '0'
    ++ currentDigits
  where
    currentDigits =
      showHex (ord currentChar) ""

joinWith :: String -> [String] -> String
joinWith _ [] =
  ""
joinWith _ [currentItem] =
  currentItem
joinWith currentSeparator (currentItem : remainingItems) =
  currentItem
    ++ currentSeparator
    ++ joinWith currentSeparator remainingItems
