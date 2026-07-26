module MyFramework.TrustBase.Types
  ( ArtifactName (..)
  , ClaimName (..)
  , Evidence (..)
  , EvidenceStatus (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  , evidencePassed
  , mkSchemaId
  , renderSchemaId
  , schemaIdValid
  ) where

newtype SchemaName = SchemaName
  { unSchemaName :: String
  }
  deriving (Eq, Ord, Show)

newtype SchemaVersion = SchemaVersion
  { schemaVersionMajor :: Int
  }
  deriving (Eq, Ord, Show)

data SchemaId = SchemaId
  { schemaIdName :: SchemaName
  , schemaIdVersion :: SchemaVersion
  }
  deriving (Eq, Ord, Show)

newtype ClaimName = ClaimName
  { unClaimName :: String
  }
  deriving (Eq, Ord, Show)

newtype ArtifactName = ArtifactName
  { unArtifactName :: String
  }
  deriving (Eq, Ord, Show)

data EvidenceStatus
  = EvidencePassed
  | EvidenceFailed
  deriving (Eq, Ord, Show)

data Evidence = Evidence
  { evidenceClaim :: ClaimName
  , evidenceStatus :: EvidenceStatus
  , evidenceExpected :: String
  , evidenceObserved :: String
  , evidenceArtifact :: ArtifactName
  }
  deriving (Eq, Show)

mkSchemaId :: String -> Int -> Maybe SchemaId
mkSchemaId name major
  | null name =
      Nothing
  | major <= 0 =
      Nothing
  | otherwise =
      Just
        SchemaId
          { schemaIdName = SchemaName name
          , schemaIdVersion = SchemaVersion major
          }

schemaIdValid :: SchemaId -> Bool
schemaIdValid schema =
  not (null (unSchemaName (schemaIdName schema)))
    && schemaVersionMajor (schemaIdVersion schema) > 0

renderSchemaId :: SchemaId -> String
renderSchemaId schema =
  unSchemaName (schemaIdName schema)
    ++ ".v"
    ++ show (schemaVersionMajor (schemaIdVersion schema))

evidencePassed :: Evidence -> Bool
evidencePassed evidence =
  evidenceStatus evidence == EvidencePassed
