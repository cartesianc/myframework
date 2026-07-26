module MyFramework.TrustBase.Evidence
  ( ClaimCatalog (..)
  , ClaimCatalogViolation (..)
  , claimCatalogClaims
  , claimManifestEvidence
  , completeEvidence
  , evidenceFor
  , fixedPointDiffClaimCatalog
  , fixedPointDiffEvidenceSchemaV1
  , schemaCatalogClaimCatalog
  , schemaCatalogClaimName
  , schemaCatalogEvidenceSchemaV1
  , trustBaseManifestClaimCatalog
  , trustBaseManifestEvidenceSchemaV1
  , validateEvidenceClaims
  ) where

import Data.List
  ( group
  , sort
  )

import MyFramework.TrustBase.FixedPoint
  ( fixedPointDiffClaimName
  , fixedPointDiffKeys
  )
import MyFramework.TrustBase.Manifest
  ( SchemaCatalogEntry (..)
  )
import MyFramework.TrustBase.Types
  ( ArtifactName (..)
  , ClaimName (..)
  , Evidence (..)
  , EvidenceStatus (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  , renderSchemaId
  )

data ClaimCatalog = ClaimCatalog
  { claimCatalogName :: String
  , claimCatalogCoreClaims :: [ClaimName]
  , claimCatalogManifestClaim :: ClaimName
  }
  deriving (Eq, Show)

data ClaimCatalogViolation
  = DuplicateCatalogClaims [ClaimName]
  | EvidenceClaimOrderMismatch [ClaimName] [ClaimName]
  deriving (Eq, Show)

trustBaseManifestEvidenceSchemaV1 :: SchemaId
trustBaseManifestEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "trust-base-manifest-evidence"
    , schemaIdVersion = SchemaVersion 1
    }

schemaCatalogEvidenceSchemaV1 :: SchemaId
schemaCatalogEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "schema-catalog-evidence"
    , schemaIdVersion = SchemaVersion 1
    }

fixedPointDiffEvidenceSchemaV1 :: SchemaId
fixedPointDiffEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "fixed-point-diff-evidence"
    , schemaIdVersion = SchemaVersion 1
    }

-- | This catalog retains the stable names for the pure manifest invariants.
-- Operational artifact, executable, gate-command, and publication claims are
-- deliberately outside the minimal TrustBase.
trustBaseManifestClaimCatalog :: ClaimCatalog
trustBaseManifestClaimCatalog =
  ClaimCatalog
    { claimCatalogName = "trust-base-manifest"
    , claimCatalogCoreClaims =
        map
          ClaimName
          [ "trust-base-manifest-schema-version"
          , "trust-base-kernel-modules-exposed"
          , "trust-base-facade-modules-exposed"
          , "trust-base-core-surface-covered"
          , "trust-base-json-schemas-synced"
          , "trust-base-evidence-claims-synced"
          ]
    , claimCatalogManifestClaim =
        ClaimName "trust-base-manifest-claim-manifest"
    }

fixedPointDiffClaimCatalog :: ClaimCatalog
fixedPointDiffClaimCatalog =
  ClaimCatalog
    { claimCatalogName = "fixed-point-diff"
    , claimCatalogCoreClaims =
        map fixedPointDiffClaimName fixedPointDiffKeys
    , claimCatalogManifestClaim =
        ClaimName "fixed-point-diff-claim-manifest"
    }

schemaCatalogClaimCatalog :: [SchemaCatalogEntry] -> ClaimCatalog
schemaCatalogClaimCatalog entries =
  ClaimCatalog
    { claimCatalogName = "schema-catalog"
    , claimCatalogCoreClaims =
        map schemaCatalogClaimName entries
    , claimCatalogManifestClaim =
        ClaimName "schema-catalog-claim-manifest"
    }

schemaCatalogClaimName :: SchemaCatalogEntry -> ClaimName
schemaCatalogClaimName entry =
  ClaimName
    ( "schema-catalog-output:"
        ++ renderSchemaId (schemaCatalogEntrySchema entry)
    )

claimCatalogClaims :: ClaimCatalog -> [ClaimName]
claimCatalogClaims catalog =
  claimCatalogCoreClaims catalog
    ++ [claimCatalogManifestClaim catalog]

evidenceFor ::
  ClaimName ->
  Bool ->
  String ->
  String ->
  ArtifactName ->
  Evidence
evidenceFor claim passed expected observed artifact =
  Evidence
    { evidenceClaim = claim
    , evidenceStatus =
        if passed
          then EvidencePassed
          else EvidenceFailed
    , evidenceExpected = expected
    , evidenceObserved = observed
    , evidenceArtifact = artifact
    }

claimManifestEvidence ::
  ClaimCatalog ->
  ArtifactName ->
  [Evidence] ->
  Evidence
claimManifestEvidence catalog artifact coreEvidence =
  evidenceFor
    (claimCatalogManifestClaim catalog)
    synced
    (claimCatalogName catalog ++ " evidence claims match the stable catalog")
    observed
    artifact
  where
    expected =
      claimCatalogCoreClaims catalog
    actual =
      map evidenceClaim coreEvidence
    synced =
      null (duplicateClaims (claimCatalogClaims catalog))
        && actual == expected
    observed
      | synced =
          "claim catalog synced: "
            ++ show (length actual)
            ++ " core claims"
      | otherwise =
          "expected "
            ++ show expected
            ++ "; actual "
            ++ show actual

completeEvidence ::
  ClaimCatalog ->
  ArtifactName ->
  [Evidence] ->
  [Evidence]
completeEvidence catalog artifact coreEvidence =
  coreEvidence
    ++ [claimManifestEvidence catalog artifact coreEvidence]

validateEvidenceClaims ::
  ClaimCatalog ->
  [Evidence] ->
  [ClaimCatalogViolation]
validateEvidenceClaims catalog payloads =
  duplicateViolations
    ++ orderViolations
  where
    expected =
      claimCatalogClaims catalog
    actual =
      map evidenceClaim payloads
    duplicateViolations =
      case duplicateClaims expected of
        [] -> []
        repeated -> [DuplicateCatalogClaims repeated]
    orderViolations
      | expected == actual = []
      | otherwise = [EvidenceClaimOrderMismatch expected actual]

duplicateClaims :: [ClaimName] -> [ClaimName]
duplicateClaims =
  map head
    . filter ((> 1) . length)
    . group
    . sort
