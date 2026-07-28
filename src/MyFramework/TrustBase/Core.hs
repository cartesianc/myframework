{-# LANGUAGE GADTs #-}

module MyFramework.TrustBase.Core
  ( BootstrapEvidence
  , BootstrapRound (..)
  , BoundTrustBase
  , CoreArtifact
  , CoreId (..)
  , CoreObservation (..)
  , Digest (..)
  , HostKernel
  , HostKernelError (..)
  , SdkCoreLock (..)
  , SdkCoreLockViolation (..)
  , TrustBaseBindingError (..)
  , TrustBaseBindingViolation (..)
  , TrustBaseDecodeError (..)
  , TrustBaseRef (..)
  , TrustBaseRefViolation (..)
  , bindTrustBase
  , bootstrapEvidenceCandidateCore
  , bootstrapEvidenceObservation
  , bootstrapEvidencePreviousCore
  , boundTrustBaseRef
  , coreArtifact
  , decodeSdkCoreLock
  , decodeTrustBaseRef
  , encodeSdkCoreLock
  , encodeTrustBaseRef
  , mkHostKernel
  , runSelfBootstrap
  , validateSdkCoreLock
  , validateTrustBaseBinding
  , validateTrustBaseRef
  ) where

import Data.Char
  ( isAlphaNum )
import Data.List
  ( group
  , sort
  )
import Text.Read
  ( readMaybe )

import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  , schemaIdValid
  )

newtype CoreId = CoreId
  { unCoreId :: String
  }
  deriving (Eq, Ord, Read, Show)

newtype Digest = Digest
  { unDigest :: String
  }
  deriving (Eq, Ord, Read, Show)

-- | Serializable identity and integrity commitment. It contains no runtime
-- closure, loader, descriptor, or environment.
data TrustBaseRef = TrustBaseRef
  { trustBaseCoreId :: CoreId
  , trustBaseArtifactDigest :: Digest
  , trustBaseManifestDigest :: Digest
  , trustBaseSchema :: SchemaId
  , trustBaseKernelClaims :: [ClaimName]
  }
  deriving (Eq, Ord, Show)

-- | The SDK keeps this lock explicit in its artifact even though the normal
-- business entry point does not ask the user for a core.
data SdkCoreLock = SdkCoreLock
  { sdkCoreRef :: TrustBaseRef
  , sdkSurfaceDigest :: Digest
  , sdkLoweringDigest :: Digest
  }
  deriving (Eq, Ord, Show)

data TrustBaseRefViolation
  = TrustBaseCoreIdMissing
  | TrustBaseCoreIdInvalid CoreId
  | TrustBaseArtifactDigestInvalid Digest
  | TrustBaseManifestDigestInvalid Digest
  | TrustBaseSchemaInvalid SchemaId
  | TrustBaseKernelClaimsMissing
  | TrustBaseKernelClaimInvalid ClaimName
  | TrustBaseKernelClaimsDuplicated [ClaimName]
  deriving (Eq, Ord, Show)

data SdkCoreLockViolation
  = SdkCoreRefInvalid [TrustBaseRefViolation]
  | SdkSurfaceDigestInvalid Digest
  | SdkLoweringDigestInvalid Digest
  deriving (Eq, Ord, Show)

data TrustBaseDecodeError
  = TrustBaseRefSyntaxInvalid
  | TrustBaseRefSemanticInvalid [TrustBaseRefViolation]
  | SdkCoreLockSyntaxInvalid
  | SdkCoreLockSemanticInvalid [SdkCoreLockViolation]
  deriving (Eq, Ord, Show)

-- | Host-observed identity. Digests are computed from the bytes actually
-- read, not copied from the expected TrustBaseRef.
data CoreObservation = CoreObservation
  { observedCoreId :: CoreId
  , observedArtifactDigest :: Digest
  , observedManifestDigest :: Digest
  , observedSchema :: SchemaId
  , observedKernelClaims :: [ClaimName]
  }
  deriving (Eq, Ord, Show)

data TrustBaseBindingViolation
  = BoundCoreIdMismatch CoreId CoreId
  | BoundArtifactDigestMismatch Digest Digest
  | BoundManifestDigestMismatch Digest Digest
  | BoundSchemaMismatch SchemaId SchemaId
  | BoundKernelClaimsMismatch [ClaimName] [ClaimName]
  deriving (Eq, Ord, Show)

newtype HostKernelError = HostKernelError
  { hostKernelErrorMessage :: String
  }
  deriving (Eq, Ord, Show)

data TrustBaseBindingError
  = TrustBaseReferenceRejected [TrustBaseRefViolation]
  | TrustBaseArtifactReadFailed HostKernelError
  | TrustBaseBindingRejected [TrustBaseBindingViolation]
  | TrustBaseCoreLoadFailed HostKernelError
  deriving (Eq, Ord, Show)

-- | Maintenance request. The previous core is deliberately not
-- duplicated here: it is carried by BoundTrustBase.
data BootstrapRound framework terminal = BootstrapRound
  { bootstrapCandidateCore :: TrustBaseRef
  , bootstrapFramework :: framework
  , bootstrapTerminal :: terminal
  }
  deriving (Eq, Ord, Show)

-- | Constructor intentionally hidden. Evidence identities are attached by
-- runSelfBootstrap, not accepted from the caller.
data BootstrapEvidence observation = BootstrapEvidence
  { bootstrapEvidencePreviousCore :: TrustBaseRef
  , bootstrapEvidenceCandidateCore :: TrustBaseRef
  , bootstrapEvidenceObservation :: observation
  }
  deriving (Eq, Ord, Show)

-- | Existential runtime capability. There is intentionally no Read or Show
-- instance and no exported constructor.
data BoundTrustBase framework terminal observation where
  BoundTrustBase ::
    TrustBaseRef ->
    runtime ->
    (runtime -> BootstrapRound framework terminal -> IO observation) ->
    BoundTrustBase framework terminal observation

-- | Bytes and parsed manifest identity returned by the closed host reader.
-- The constructor is hidden; host adapters use coreArtifact.
data CoreArtifact bytes = CoreArtifact
  { coreArtifactBytes :: bytes
  , coreArtifactManifestBytes :: bytes
  , coreArtifactCoreId :: CoreId
  , coreArtifactSchema :: SchemaId
  , coreArtifactKernelClaims :: [ClaimName]
  }

-- | Closed runtime host surface. Its constructor and field selectors are
-- hidden so loading and invocation cannot bypass bindTrustBase.
data HostKernel bytes runtime framework terminal observation =
  HostKernel
    { kernelReadCoreArtifact ::
        TrustBaseRef ->
        IO (Either HostKernelError (CoreArtifact bytes))
    , kernelDigestBytes :: bytes -> Digest
    , kernelLoadVerifiedCore ::
        CoreArtifact bytes ->
        IO (Either HostKernelError runtime)
    , kernelInvokeCore ::
        runtime ->
        BootstrapRound framework terminal ->
        IO observation
    }

coreArtifact ::
  bytes ->
  bytes ->
  CoreId ->
  SchemaId ->
  [ClaimName] ->
  CoreArtifact bytes
coreArtifact =
  CoreArtifact

mkHostKernel ::
  (TrustBaseRef -> IO (Either HostKernelError (CoreArtifact bytes))) ->
  (bytes -> Digest) ->
  (CoreArtifact bytes -> IO (Either HostKernelError runtime)) ->
  (runtime -> BootstrapRound framework terminal -> IO observation) ->
  HostKernel bytes runtime framework terminal observation
mkHostKernel =
  HostKernel

boundTrustBaseRef ::
  BoundTrustBase framework terminal observation ->
  TrustBaseRef
boundTrustBaseRef (BoundTrustBase currentRef _ _) =
  currentRef

runSelfBootstrap ::
  BoundTrustBase framework terminal observation ->
  BootstrapRound framework terminal ->
  IO (BootstrapEvidence observation)
runSelfBootstrap
  (BoundTrustBase previousCore currentRuntime currentInvoke)
  currentRound = do
    currentObservation <-
      currentInvoke currentRuntime currentRound
    pure
      BootstrapEvidence
        { bootstrapEvidencePreviousCore = previousCore
        , bootstrapEvidenceCandidateCore =
            bootstrapCandidateCore currentRound
        , bootstrapEvidenceObservation = currentObservation
        }

bindTrustBase ::
  HostKernel bytes runtime framework terminal observation ->
  TrustBaseRef ->
  IO
    ( Either
        TrustBaseBindingError
        (BoundTrustBase framework terminal observation)
    )
bindTrustBase currentKernel expectedRef =
  case validateTrustBaseRef expectedRef of
    currentViolations@(_ : _) ->
      pure (Left (TrustBaseReferenceRejected currentViolations))
    [] -> do
      currentArtifactResult <-
        kernelReadCoreArtifact currentKernel expectedRef
      case currentArtifactResult of
        Left currentError ->
          pure (Left (TrustBaseArtifactReadFailed currentError))
        Right currentArtifact ->
          case
              validateTrustBaseBinding
                expectedRef
                (observeArtifact currentKernel currentArtifact)
            of
              currentViolations@(_ : _) ->
                pure (Left (TrustBaseBindingRejected currentViolations))
              [] -> do
                currentRuntimeResult <-
                  kernelLoadVerifiedCore currentKernel currentArtifact
                pure
                  ( case currentRuntimeResult of
                      Left currentError ->
                        Left (TrustBaseCoreLoadFailed currentError)
                      Right currentRuntime ->
                        Right
                          ( BoundTrustBase
                              expectedRef
                              currentRuntime
                              (kernelInvokeCore currentKernel)
                          )
                  )

observeArtifact ::
  HostKernel bytes runtime framework terminal observation ->
  CoreArtifact bytes ->
  CoreObservation
observeArtifact currentKernel currentArtifact =
  CoreObservation
    { observedCoreId = coreArtifactCoreId currentArtifact
    , observedArtifactDigest =
        kernelDigestBytes currentKernel
          (coreArtifactBytes currentArtifact)
    , observedManifestDigest =
        kernelDigestBytes currentKernel
          (coreArtifactManifestBytes currentArtifact)
    , observedSchema = coreArtifactSchema currentArtifact
    , observedKernelClaims =
        coreArtifactKernelClaims currentArtifact
    }
encodeTrustBaseRef :: TrustBaseRef -> String
encodeTrustBaseRef currentRef =
  show
    ( unCoreId (trustBaseCoreId currentRef)
    , unDigest (trustBaseArtifactDigest currentRef)
    , unDigest (trustBaseManifestDigest currentRef)
    , unSchemaName (schemaIdName (trustBaseSchema currentRef))
    , schemaVersionMajor
        (schemaIdVersion (trustBaseSchema currentRef))
    , map unClaimName (trustBaseKernelClaims currentRef)
    )

decodeTrustBaseRef ::
  String ->
  Either TrustBaseDecodeError TrustBaseRef
decodeTrustBaseRef currentText =
  case
      ( readMaybe currentText ::
          Maybe
            ( String
            , String
            , String
            , String
            , Int
            , [String]
            )
      )
    of
      Nothing ->
        Left TrustBaseRefSyntaxInvalid
      Just
        ( currentCoreId
          , currentArtifactDigest
          , currentManifestDigest
          , currentSchemaName
          , currentSchemaVersion
          , currentClaims
          ) ->
          let currentRef =
                TrustBaseRef
                  { trustBaseCoreId = CoreId currentCoreId
                  , trustBaseArtifactDigest =
                      Digest currentArtifactDigest
                  , trustBaseManifestDigest =
                      Digest currentManifestDigest
                  , trustBaseSchema =
                      SchemaId
                        { schemaIdName =
                            SchemaName currentSchemaName
                        , schemaIdVersion =
                            SchemaVersion currentSchemaVersion
                        }
                  , trustBaseKernelClaims =
                      map ClaimName currentClaims
                  }
              currentViolations =
                validateTrustBaseRef currentRef
           in if null currentViolations
                then Right currentRef
                else
                  Left
                    (TrustBaseRefSemanticInvalid currentViolations)

encodeSdkCoreLock :: SdkCoreLock -> String
encodeSdkCoreLock currentLock =
  show
    ( encodeTrustBaseRef (sdkCoreRef currentLock)
    , unDigest (sdkSurfaceDigest currentLock)
    , unDigest (sdkLoweringDigest currentLock)
    )

decodeSdkCoreLock ::
  String ->
  Either TrustBaseDecodeError SdkCoreLock
decodeSdkCoreLock currentText =
  case
      (readMaybe currentText :: Maybe (String, String, String))
    of
      Nothing ->
        Left SdkCoreLockSyntaxInvalid
      Just
        ( currentRefText
          , currentSurfaceDigest
          , currentLoweringDigest
          ) ->
          case decodeTrustBaseRef currentRefText of
            Left currentError ->
              Left currentError
            Right currentRef ->
              let currentLock =
                    SdkCoreLock
                      { sdkCoreRef = currentRef
                      , sdkSurfaceDigest =
                          Digest currentSurfaceDigest
                      , sdkLoweringDigest =
                          Digest currentLoweringDigest
                      }
                  currentViolations =
                    validateSdkCoreLock currentLock
               in if null currentViolations
                    then Right currentLock
                    else
                      Left
                        (SdkCoreLockSemanticInvalid currentViolations)

validateTrustBaseRef :: TrustBaseRef -> [TrustBaseRefViolation]
validateTrustBaseRef currentRef =
  concat
    [ coreIdViolations
    , artifactDigestViolations
    , manifestDigestViolations
    , schemaViolations
    , claimViolations
    ]
  where
    currentCoreId =
      trustBaseCoreId currentRef
    currentClaims =
      trustBaseKernelClaims currentRef
    coreIdViolations
      | null (unCoreId currentCoreId) =
          [TrustBaseCoreIdMissing]
      | validCoreId currentCoreId =
          []
      | otherwise =
          [TrustBaseCoreIdInvalid currentCoreId]
    artifactDigestViolations =
      [ TrustBaseArtifactDigestInvalid
          (trustBaseArtifactDigest currentRef)
      | not (validDigest (trustBaseArtifactDigest currentRef))
      ]
    manifestDigestViolations =
      [ TrustBaseManifestDigestInvalid
          (trustBaseManifestDigest currentRef)
      | not (validDigest (trustBaseManifestDigest currentRef))
      ]
    schemaViolations =
      [TrustBaseSchemaInvalid (trustBaseSchema currentRef)
      | not (schemaIdValid (trustBaseSchema currentRef))
      ]
    claimViolations =
      [TrustBaseKernelClaimsMissing | null currentClaims]
        ++ map TrustBaseKernelClaimInvalid
          (filter (null . unClaimName) currentClaims)
        ++ [ TrustBaseKernelClaimsDuplicated currentDuplicates
           | let currentDuplicates = duplicates currentClaims
           , not (null currentDuplicates)
           ]

validateSdkCoreLock :: SdkCoreLock -> [SdkCoreLockViolation]
validateSdkCoreLock currentLock =
  [ SdkCoreRefInvalid currentViolations
  | let currentViolations =
          validateTrustBaseRef (sdkCoreRef currentLock)
  , not (null currentViolations)
  ]
    ++ [ SdkSurfaceDigestInvalid (sdkSurfaceDigest currentLock)
       | not (validDigest (sdkSurfaceDigest currentLock))
       ]
    ++ [ SdkLoweringDigestInvalid (sdkLoweringDigest currentLock)
       | not (validDigest (sdkLoweringDigest currentLock))
       ]

validateTrustBaseBinding ::
  TrustBaseRef ->
  CoreObservation ->
  [TrustBaseBindingViolation]
validateTrustBaseBinding expectedRef currentObservation =
  concat
    [ [ BoundCoreIdMismatch
          (trustBaseCoreId expectedRef)
          (observedCoreId currentObservation)
      | trustBaseCoreId expectedRef
          /= observedCoreId currentObservation
      ]
    , [ BoundArtifactDigestMismatch
          (trustBaseArtifactDigest expectedRef)
          (observedArtifactDigest currentObservation)
      | trustBaseArtifactDigest expectedRef
          /= observedArtifactDigest currentObservation
      ]
    , [ BoundManifestDigestMismatch
          (trustBaseManifestDigest expectedRef)
          (observedManifestDigest currentObservation)
      | trustBaseManifestDigest expectedRef
          /= observedManifestDigest currentObservation
      ]
    , [ BoundSchemaMismatch
          (trustBaseSchema expectedRef)
          (observedSchema currentObservation)
      | trustBaseSchema expectedRef
          /= observedSchema currentObservation
      ]
    , [ BoundKernelClaimsMismatch
          (trustBaseKernelClaims expectedRef)
          (observedKernelClaims currentObservation)
      | trustBaseKernelClaims expectedRef
          /= observedKernelClaims currentObservation
      ]
    ]

validCoreId :: CoreId -> Bool
validCoreId (CoreId currentValue) =
  not (null currentValue)
    && all validCoreIdChar currentValue
  where
    validCoreIdChar currentChar =
      isAlphaNum currentChar
        || currentChar `elem` ("-._" :: String)

validDigest :: Digest -> Bool
validDigest (Digest currentValue) =
  length currentValue == 64
    && all (`elem` ("0123456789abcdef" :: String)) currentValue

duplicates :: Ord value => [value] -> [value]
duplicates =
  map head
    . filter ((> 1) . length)
    . group
    . sort
