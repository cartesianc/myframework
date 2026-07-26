module MyFramework.TrustBase.Manifest
  ( ManifestObservation (..)
  , ManifestViolation (..)
  , SchemaCatalogEntry (..)
  , TrustBaseManifest (..)
  , allManifestModules
  , manifestValid
  , missingItems
  , trustBaseManifestSchemaV2
  , validateManifest
  ) where

import Data.List
  ( group
  , sort
  )

import MyFramework.TrustBase.Types
  ( ClaimName
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  , schemaIdValid
  )

data SchemaCatalogEntry = SchemaCatalogEntry
  { schemaCatalogEntrySchema :: SchemaId
  , schemaCatalogEntryProducer :: String
  }
  deriving (Eq, Ord, Show)

data TrustBaseManifest = TrustBaseManifest
  { trustBaseManifestSchema :: SchemaId
  , trustBaseManifestName :: String
  , trustBaseManifestHostBoundary :: [String]
  , trustBaseManifestKernelModules :: [String]
  , trustBaseManifestFacadeModules :: [String]
  , trustBaseManifestSchemas :: [SchemaCatalogEntry]
  , trustBaseManifestEvidenceClaims :: [ClaimName]
  }
  deriving (Eq, Show)

-- | Values observed by build tooling are supplied as data. Validation itself
-- remains deterministic and has no filesystem, process, facade, or runtime IO.
data ManifestObservation = ManifestObservation
  { manifestObservedExposedModules :: [String]
  , manifestObservedCoreSurfaceModules :: [String]
  , manifestObservedSchemas :: [SchemaCatalogEntry]
  , manifestObservedEvidenceClaims :: [ClaimName]
  }
  deriving (Eq, Show)

data ManifestViolation
  = ManifestSchemaMismatch SchemaId SchemaId
  | ManifestNameMissing
  | ManifestHostBoundaryMissing
  | ManifestDuplicateHostBoundary [String]
  | ManifestDuplicateModules [String]
  | ManifestMissingExposedKernelModules [String]
  | ManifestMissingExposedFacadeModules [String]
  | ManifestMissingCoreSurfaceModules [String]
  | ManifestInvalidSchemas [SchemaCatalogEntry]
  | ManifestDuplicateSchemas [SchemaId]
  | ManifestSchemaCatalogMismatch [SchemaCatalogEntry] [SchemaCatalogEntry]
  | ManifestDuplicateClaims [ClaimName]
  | ManifestClaimCatalogMismatch [ClaimName] [ClaimName]
  deriving (Eq, Show)

trustBaseManifestSchemaV2 :: SchemaId
trustBaseManifestSchemaV2 =
  SchemaId
    { schemaIdName = SchemaName "trust-base-manifest"
    , schemaIdVersion = SchemaVersion 2
    }

allManifestModules :: TrustBaseManifest -> [String]
allManifestModules manifest =
  trustBaseManifestKernelModules manifest
    ++ trustBaseManifestFacadeModules manifest

validateManifest :: TrustBaseManifest -> ManifestObservation -> [ManifestViolation]
validateManifest manifest observation =
  concat
    [ schemaViolations
    , identityViolations
    , hostBoundaryViolations
    , moduleViolations
    , schemaCatalogViolations
    , claimCatalogViolations
    ]
  where
    schemaViolations
      | trustBaseManifestSchema manifest == trustBaseManifestSchemaV2 =
          []
      | otherwise =
          [ ManifestSchemaMismatch
              trustBaseManifestSchemaV2
              (trustBaseManifestSchema manifest)
          ]

    identityViolations
      | null (trustBaseManifestName manifest) =
          [ManifestNameMissing]
      | otherwise =
          []

    hostBoundaryViolations =
      emptyViolation
        ++ duplicateViolation
      where
        boundary =
          trustBaseManifestHostBoundary manifest
        emptyViolation
          | null boundary = [ManifestHostBoundaryMissing]
          | otherwise = []
        duplicateViolation =
          case duplicates boundary of
            [] -> []
            repeated -> [ManifestDuplicateHostBoundary repeated]

    moduleViolations =
      duplicateViolation
        ++ missingKernelViolation
        ++ missingFacadeViolation
        ++ missingSurfaceViolation
      where
        modules =
          allManifestModules manifest
        duplicateViolation =
          case duplicates modules of
            [] -> []
            repeated -> [ManifestDuplicateModules repeated]
        missingKernelViolation =
          case
              missingItems
                (manifestObservedExposedModules observation)
                (trustBaseManifestKernelModules manifest)
            of
              [] -> []
              missing -> [ManifestMissingExposedKernelModules missing]
        missingFacadeViolation =
          case
              missingItems
                (manifestObservedExposedModules observation)
                (trustBaseManifestFacadeModules manifest)
            of
              [] -> []
              missing -> [ManifestMissingExposedFacadeModules missing]
        missingSurfaceViolation =
          case
              missingItems
                (manifestObservedCoreSurfaceModules observation)
                modules
            of
              [] -> []
              missing -> [ManifestMissingCoreSurfaceModules missing]

    schemaCatalogViolations =
      invalidViolation
        ++ duplicateViolation
        ++ driftViolation
      where
        expected =
          trustBaseManifestSchemas manifest
        observed =
          manifestObservedSchemas observation
        invalid =
          filter (not . validSchemaCatalogEntry) expected
        invalidViolation
          | null invalid = []
          | otherwise = [ManifestInvalidSchemas invalid]
        duplicateViolation =
          case duplicates (map schemaCatalogEntrySchema expected) of
            [] -> []
            repeated -> [ManifestDuplicateSchemas repeated]
        driftViolation
          | expected == observed = []
          | otherwise = [ManifestSchemaCatalogMismatch expected observed]

    claimCatalogViolations =
      duplicateViolation
        ++ driftViolation
      where
        expected =
          trustBaseManifestEvidenceClaims manifest
        observed =
          manifestObservedEvidenceClaims observation
        duplicateViolation =
          case duplicates expected of
            [] -> []
            repeated -> [ManifestDuplicateClaims repeated]
        driftViolation
          | expected == observed = []
          | otherwise = [ManifestClaimCatalogMismatch expected observed]

manifestValid :: TrustBaseManifest -> ManifestObservation -> Bool
manifestValid manifest =
  null . validateManifest manifest

missingItems :: Eq a => [a] -> [a] -> [a]
missingItems available required =
  [ item
  | item <- required
  , item `notElem` available
  ]

validSchemaCatalogEntry :: SchemaCatalogEntry -> Bool
validSchemaCatalogEntry entry =
  schemaIdValid (schemaCatalogEntrySchema entry)
    && not (null (schemaCatalogEntryProducer entry))

duplicates :: Ord a => [a] -> [a]
duplicates =
  map head
    . filter ((> 1) . length)
    . group
    . sort
