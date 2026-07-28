module MyFramework.TrustBase.Evidence
  ( ClaimCatalog (..)
  , ClaimCatalogViolation (..)
  , claimCatalogClaims
  , claimManifestEvidence
  , completeEvidence
  , evidenceFor
  , fixedPointDiffClaimCatalog
  , fixedPointDiffEvidenceSchemaV1
  , promotionClaimCatalog
  , promotionEvidenceSchemaV1
  , sdkPackageClaimCatalog
  , sdkPackageEvidenceSchemaV1
  , schemaCatalogClaimCatalog
  , schemaCatalogClaimName
  , schemaCatalogEvidenceSchemaV1
  , selfInterpretClaimCatalog
  , selfInterpretEvidenceSchemaV1
  , trustBaseManifestClaimCatalog
  , trustBaseManifestEvidenceSchemaV1
  , trustBaseBindingClaimCatalog
  , trustBaseBindingEvidenceSchemaV1
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

trustBaseBindingEvidenceSchemaV1 :: SchemaId
trustBaseBindingEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "trustbase-binding-evidence"
    , schemaIdVersion = SchemaVersion 1
    }

promotionEvidenceSchemaV1 :: SchemaId
promotionEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "core-promotion-evidence"
    , schemaIdVersion = SchemaVersion 1
    }

selfInterpretEvidenceSchemaV1 :: SchemaId
selfInterpretEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "core-self-interpret-evidence"
    , schemaIdVersion = SchemaVersion 1
    }

sdkPackageEvidenceSchemaV1 :: SchemaId
sdkPackageEvidenceSchemaV1 =
  SchemaId
    { schemaIdName = SchemaName "sdk-package-evidence"
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

trustBaseBindingClaimCatalog :: ClaimCatalog
trustBaseBindingClaimCatalog =
  ClaimCatalog
    { claimCatalogName = "trustbase-binding"
    , claimCatalogCoreClaims =
        map
          ClaimName
          [ "trustbase-ref-is-serializable"
          , "trustbase-binding-is-explicit"
          , "trustbase-artifact-digest-mismatch-rejected"
          , "trustbase-manifest-digest-mismatch-rejected"
          , "trustbase-core-id-mismatch-rejected"
          , "trustbase-schema-mismatch-rejected"
          , "trustbase-mismatch-precedes-runtime-load"
          , "bound-trustbase-runs-bootstrap-round"
          , "sdk-core-lock-is-explicit-and-serializable"
          ]
    , claimCatalogManifestClaim =
        ClaimName "trustbase-binding-claim-manifest"
    }
selfInterpretClaimCatalog :: ClaimCatalog
selfInterpretClaimCatalog =
  ClaimCatalog
    { claimCatalogName = "core-self-interpret"
    , claimCatalogCoreClaims =
        map
          ClaimName
          [ "previous-core-runs-candidate"
          , "candidate-is-expressed-by-normal-facade"
          , "candidate-runs-as-framework-business"
          , "empty-business-closes-recursion"
          , "empty-business-has-no-curde"
          , "empty-business-has-no-handler"
          , "empty-business-has-no-host-io"
          , "trustbase-not-forwarded-to-terminal-business"
          , "core0-core1-exchangeable"
          , "candidate-has-no-previous-core-runtime-dependency"
          , "previous-core-back-reference-negative-rejected"
          , "semantic-fixed-point-passed"
          ]
    , claimCatalogManifestClaim =
        ClaimName "core-self-interpret-claim-manifest"
    }

promotionClaimCatalog :: ClaimCatalog
promotionClaimCatalog =
  ClaimCatalog
    { claimCatalogName = "core-promotion"
    , claimCatalogCoreClaims =
        map
          ClaimName
          [ "promotion-core-manifest-content-addressed"
          , "promotion-core-manifest-roundtrip"
          , "promotion-evidence-reports-validated"
          , "promotion-evidence-digests-bound"
          , "promotion-record-roundtrip"
          , "promotion-previous-core-back-reference-rejected"
          , "promotion-candidate-manifest-mismatch-rejected"
          , "promotion-invalid-semantic-report-rejected"
          , "promotion-pending-cannot-be-current"
          , "promotion-approved-pointer-matches-candidate"
          , "promotion-current-pointer-roundtrip"
          ]
    , claimCatalogManifestClaim =
        ClaimName "core-promotion-claim-manifest"
    }

sdkPackageClaimCatalog :: ClaimCatalog
sdkPackageClaimCatalog =
  ClaimCatalog
    { claimCatalogName = "sdk-package"
    , claimCatalogCoreClaims =
        map
          ClaimName
          [ "sdk-core-lock-valid"
          , "sdk-surface-digest-tamper-blocked"
          , "sdk-lowering-digest-tamper-blocked"
          , "sdk-pending-core-rejected"
          , "sdk-approved-artifact-digest-bound"
          , "sdk-package-materialized"
          , "sdk-package-verifies"
          , "sdk-package-manifest-binds-core-lock"
          ]
    , claimCatalogManifestClaim =
        ClaimName "sdk-package-claim-manifest"
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
