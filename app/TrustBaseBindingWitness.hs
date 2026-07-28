module Main (main) where

import Control.Monad
  ( unless )
import Data.Char
  ( ord )
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
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
  , trustBaseBindingClaimCatalog
  , trustBaseBindingEvidenceSchemaV1
  , validateEvidenceClaims
  )
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

data SampleRuntime = SampleRuntime String
  deriving (Eq, Show)

data EmptyTerminal = EmptyTerminal
  deriving (Eq, Ord, Read, Show)

newtype SampleObservation = SampleObservation String
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  validLoadCount <- newIORef 0
  invalidLoadCount <- newIORef 0
  validBinding <-
    bindTrustBase
      (sampleKernel validLoadCount validArtifact)
      previousCoreRef
  validRun <-
    case validBinding of
      Left _ ->
        pure Nothing
      Right currentBound -> do
        currentEvidence <-
          runSelfBootstrap currentBound sampleRound
        pure (Just (currentBound, currentEvidence))

  artifactMismatch <-
    bindTrustBase
      (sampleKernel invalidLoadCount tamperedArtifact)
      previousCoreRef
  manifestMismatch <-
    bindTrustBase
      (sampleKernel invalidLoadCount tamperedManifest)
      previousCoreRef
  identityMismatch <-
    bindTrustBase
      (sampleKernel invalidLoadCount wrongIdentityArtifact)
      previousCoreRef
  schemaMismatch <-
    bindTrustBase
      (sampleKernel invalidLoadCount wrongSchemaArtifact)
      previousCoreRef

  validLoads <- readIORef validLoadCount
  invalidLoads <- readIORef invalidLoadCount

  let validRoundTrip =
        decodeTrustBaseRef (encodeTrustBaseRef previousCoreRef)
          == Right previousCoreRef
      sdkLockRoundTrip =
        decodeSdkCoreLock (encodeSdkCoreLock sdkCoreLock)
          == Right sdkCoreLock
          && null (validateSdkCoreLock sdkCoreLock)
      explicitBinding =
        case validRun of
          Just (currentBound, _) ->
            boundTrustBaseRef currentBound == previousCoreRef
          Nothing ->
            False
      bootstrapRan =
        case validRun of
          Just (_, currentEvidence) ->
            bootstrapEvidencePreviousCore currentEvidence
              == previousCoreRef
              && bootstrapEvidenceCandidateCore currentEvidence
                == candidateCoreRef
              && bootstrapEvidenceObservation currentEvidence
                == SampleObservation
                  "core0:framework-as-business:empty"
          Nothing ->
            False
      mismatchBeforeLoad =
        invalidLoads == 0
      coreEvidence =
        [ currentEvidence
        | (currentClaim, currentPassed, currentObserved) <-
            [ ( "trustbase-ref-is-serializable"
              , validRoundTrip
              , "TrustBaseRef explicit codec round trip"
              )
            , ( "trustbase-binding-is-explicit"
              , explicitBinding && validLoads == 1
              , "bindTrustBase required HostKernel and TrustBaseRef"
              )
            , ( "trustbase-artifact-digest-mismatch-rejected"
              , hasArtifactMismatch artifactMismatch
              , renderBindingResult artifactMismatch
              )
            , ( "trustbase-manifest-digest-mismatch-rejected"
              , hasManifestMismatch manifestMismatch
              , renderBindingResult manifestMismatch
              )
            , ( "trustbase-core-id-mismatch-rejected"
              , hasCoreIdMismatch identityMismatch
              , renderBindingResult identityMismatch
              )
            , ( "trustbase-schema-mismatch-rejected"
              , hasSchemaMismatch schemaMismatch
              , renderBindingResult schemaMismatch
              )
            , ( "trustbase-mismatch-precedes-runtime-load"
              , mismatchBeforeLoad
              , "invalid load count=" ++ show invalidLoads
              )
            , ( "bound-trustbase-runs-bootstrap-round"
              , bootstrapRan
              , maybe "binding failed" (show . snd) validRun
              )
            , ( "sdk-core-lock-is-explicit-and-serializable"
              , sdkLockRoundTrip
              , show sdkCoreLock
              )
            ]
        , let currentEvidence =
                evidenceFor
                  (ClaimName currentClaim)
                  currentPassed
                  "passed"
                  currentObserved
                  witnessArtifact
        ]
      allEvidence =
        completeEvidence
          trustBaseBindingClaimCatalog
          witnessArtifact
          coreEvidence
      catalogValid =
        null
          ( validateEvidenceClaims
              trustBaseBindingClaimCatalog
              allEvidence
          )
      succeeded =
        catalogValid
          && all evidencePassed allEvidence

  putStrLn
    ( renderReport
        allEvidence
        validLoads
        invalidLoads
        succeeded
    )
  unless succeeded exitFailure

witnessArtifact :: ArtifactName
witnessArtifact =
  ArtifactName "trustbase-binding-witness"

coreSchema :: SchemaId
coreSchema =
  SchemaId
    { schemaIdName = SchemaName "myframework-core"
    , schemaIdVersion = SchemaVersion 1
    }

otherSchema :: SchemaId
otherSchema =
  SchemaId
    { schemaIdName = SchemaName "myframework-core"
    , schemaIdVersion = SchemaVersion 2
    }

kernelClaims :: [ClaimName]
kernelClaims =
  [ ClaimName "host-kernel-closed"
  , ClaimName "fixed-bootstrap-entrypoint"
  ]

artifactBytes :: String
artifactBytes =
  "compiled-core0"

manifestBytes :: String
manifestBytes =
  "core0-manifest"

previousCoreRef :: TrustBaseRef
previousCoreRef =
  TrustBaseRef
    { trustBaseCoreId = CoreId "core0"
    , trustBaseArtifactDigest = Digest (sha256 artifactBytes)
    , trustBaseManifestDigest = Digest (sha256 manifestBytes)
    , trustBaseSchema = coreSchema
    , trustBaseKernelClaims = kernelClaims
    }

candidateCoreRef :: TrustBaseRef
candidateCoreRef =
  previousCoreRef
    { trustBaseCoreId = CoreId "core1"
    }

sdkCoreLock :: SdkCoreLock
sdkCoreLock =
  SdkCoreLock
    { sdkCoreRef = previousCoreRef
    , sdkSurfaceDigest = Digest (sha256 "public-sdk-surface")
    , sdkLoweringDigest = Digest (sha256 "sdk-lowering")
    }

validArtifact :: CoreArtifact String
validArtifact =
  coreArtifact
    artifactBytes
    manifestBytes
    (CoreId "core0")
    coreSchema
    kernelClaims

tamperedArtifact :: CoreArtifact String
tamperedArtifact =
  coreArtifact
    "tampered-core0"
    manifestBytes
    (CoreId "core0")
    coreSchema
    kernelClaims

tamperedManifest :: CoreArtifact String
tamperedManifest =
  coreArtifact
    artifactBytes
    "tampered-manifest"
    (CoreId "core0")
    coreSchema
    kernelClaims

wrongIdentityArtifact :: CoreArtifact String
wrongIdentityArtifact =
  coreArtifact
    artifactBytes
    manifestBytes
    (CoreId "other-core")
    coreSchema
    kernelClaims

wrongSchemaArtifact :: CoreArtifact String
wrongSchemaArtifact =
  coreArtifact
    artifactBytes
    manifestBytes
    (CoreId "core0")
    otherSchema
    kernelClaims

sampleRound :: BootstrapRound String EmptyTerminal
sampleRound =
  BootstrapRound
    { bootstrapCandidateCore = candidateCoreRef
    , bootstrapFramework = "framework-as-business"
    , bootstrapTerminal = EmptyTerminal
    }

sampleKernel ::
  IORef Int ->
  CoreArtifact String ->
  HostKernel
    String
    SampleRuntime
    String
    EmptyTerminal
    SampleObservation
sampleKernel currentLoadCount currentArtifact =
  mkHostKernel
    (\_ -> pure (Right currentArtifact))
    (Digest . sha256)
    ( \_ -> do
        modifyIORef' currentLoadCount (+ 1)
        pure (Right (SampleRuntime "core0"))
    )
    ( \(SampleRuntime currentRuntime) currentRound ->
        pure
          ( SampleObservation
              ( currentRuntime
                  ++ ":"
                  ++ bootstrapFramework currentRound
                  ++ ":empty"
              )
          )
    )

hasArtifactMismatch ::
  Either TrustBaseBindingError bound ->
  Bool
hasArtifactMismatch currentResult =
  case currentResult of
    Left (TrustBaseBindingRejected currentViolations) ->
      any isArtifactMismatch currentViolations
    _ ->
      False
  where
    isArtifactMismatch currentViolation =
      case currentViolation of
        BoundArtifactDigestMismatch _ _ -> True
        _ -> False

hasManifestMismatch ::
  Either TrustBaseBindingError bound ->
  Bool
hasManifestMismatch currentResult =
  case currentResult of
    Left (TrustBaseBindingRejected currentViolations) ->
      any isManifestMismatch currentViolations
    _ ->
      False
  where
    isManifestMismatch currentViolation =
      case currentViolation of
        BoundManifestDigestMismatch _ _ -> True
        _ -> False

hasCoreIdMismatch ::
  Either TrustBaseBindingError bound ->
  Bool
hasCoreIdMismatch currentResult =
  case currentResult of
    Left (TrustBaseBindingRejected currentViolations) ->
      any isCoreIdMismatch currentViolations
    _ ->
      False
  where
    isCoreIdMismatch currentViolation =
      case currentViolation of
        BoundCoreIdMismatch _ _ -> True
        _ -> False

hasSchemaMismatch ::
  Either TrustBaseBindingError bound ->
  Bool
hasSchemaMismatch currentResult =
  case currentResult of
    Left (TrustBaseBindingRejected currentViolations) ->
      any isSchemaMismatch currentViolations
    _ ->
      False
  where
    isSchemaMismatch currentViolation =
      case currentViolation of
        BoundSchemaMismatch _ _ -> True
        _ -> False

renderBindingResult ::
  Either TrustBaseBindingError bound ->
  String
renderBindingResult currentResult =
  case currentResult of
    Left currentError -> show currentError
    Right _ -> "unexpected binding success"

renderReport ::
  [Evidence] ->
  Int ->
  Int ->
  Bool ->
  String
renderReport currentEvidence validLoads invalidLoads succeeded =
  jsonObject
    [ ( "schema"
      , jsonString (renderSchemaId trustBaseBindingEvidenceSchemaV1)
      )
    , ("artifact", jsonString "trustbase-binding-witness")
    , ("result", jsonString (if succeeded then "passed" else "failed"))
    , ("validRuntimeLoads", show validLoads)
    , ("invalidRuntimeLoads", show invalidLoads)
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
