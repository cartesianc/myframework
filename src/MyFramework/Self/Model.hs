{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module MyFramework.Self.Model
  ( SelfModel
  , SelfModelUser
  , canonicalSelfModelJson
  , decodeSelfModel
  , decodeSelfModelJson
  , encodeSelfModel
  , selfAstBlueprint
  , selfCreateUserHandle
  , selfDeleteUserHandle
  , selfEffectSystemDeclarations
  , selfEmitAuditHandle
  , selfModel
  , selfModelAstSeed
  , selfModelEffectSystems
  , selfModelHandlerCoverage
  , selfModelSchema
  , selfModelSdkVersion
  , selfModelSourceInput
  , selfModelJsonRoundTrip
  , selfModelTextRoundTrip
  , selfReadUserHandle
  , selfUpdateUserHandle
  ) where

import GHC.Generics
  ( Generic )
import Text.Read
  ( readMaybe )

import MyFramework.Ast
  ( AstBlueprintSeed (..)
  , AstSeed (..)
  , AstTarget (..)
  )
import MyFramework.CURDE
import MyFramework.SDK.SourceArtifact
  ( SdkSourceInput (..)
  , buildSdkSourceReport
  , mkSdkSourceInput
  , handlerCoverageFromIds
  , renderSdkSourceArtifactJson
  , sdkSourceArtifactSchemaV1
  , sdkSourceReportArtifact
  )
import MyFramework.TrustBase.Json
  ( JsonValue (..)
  , jsonArrayItems
  , jsonObjectField
  , jsonStringValue
  , parseJson
  )

import MyFramework.TrustBase.Core
  ( CoreId (..)
  , Digest (..)
  , TrustBaseRef (..)
  )
import MyFramework.TrustBase.Digest
  ( sha256 )
import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , SchemaId (..)
  , SchemaName (..)
  , SchemaVersion (..)
  )

-- | A witness-only value type used by the framework's own CURDE declaration.
-- It is deliberately not exported from the root facade.
data SelfModelUser

data SourceHandles = SourceHandles
  { createUser :: Handle "createUser" 'C () SelfModelUser
  , readUser :: Handle "readUser" 'R () SelfModelUser
  }
  deriving (Generic)

data SinkHandles = SinkHandles
  { updateUser :: Handle "updateUser" 'U SelfModelUser NoObservation
  , deleteUser :: Handle "deleteUser" 'D () NoObservation
  , emitAudit :: Handle "emitAudit" 'E () NoObservation
  }
  deriving (Generic)

-- | The one serializable description consumed by semantic witnesses and
-- self-artifact generation. It reuses the normal CURDE and AST authoring
-- values; it is not a fourth facade.
data SelfModel = SelfModel
  { selfModelSchema :: String
  , selfModelSdkVersion :: String
  , selfModelEffectSystems :: [EffectSystemDecl]
  , selfModelAstSeed :: AstBlueprintSeed
  , selfModelHandlerCoverage :: [HandleId]
  }
  deriving (Eq, Ord, Read, Show)

selfModelSchemaV1 :: String
selfModelSchemaV1 =
  "myframework-self-model.v1"

selfSourceName :: EffectSystemName
selfSourceName =
  EffectSystemName "source"

selfSinkName :: EffectSystemName
selfSinkName =
  EffectSystemName "sink"

selfUserSchema :: SchemaRef SelfModelUser
selfUserSchema =
  scalarSchema "User"

selfCreateUserHandle :: Handle "createUser" 'C () SelfModelUser
selfCreateUserHandle =
  c @"createUser"
    selfSourceName
    CommandSpec
      { commandArgumentSchema = unitSchema
      , commandObservation = CaptureObservation selfUserSchema
      , commandInput = Nothing
      }

selfReadUserHandle :: Handle "readUser" 'R () SelfModelUser
selfReadUserHandle =
  r @"readUser"
    selfSourceName
    ReadSpec
      { readResultSchema = selfUserSchema
      , readInput = Just (SomeHandleRef selfCreateUserHandle)
      , readSource = ReadFromInputObservation
      }

selfUpdateUserHandle ::
  Handle "updateUser" 'U SelfModelUser NoObservation
selfUpdateUserHandle =
  u @"updateUser"
    selfSinkName
    CommandSpec
      { commandArgumentSchema = selfUserSchema
      , commandObservation = DiscardObservation
      , commandInput = Just (SomeHandleRef selfCreateUserHandle)
      }

selfDeleteUserHandle :: Handle "deleteUser" 'D () NoObservation
selfDeleteUserHandle =
  d @"deleteUser"
    selfSinkName
    CommandSpec
      { commandArgumentSchema = unitSchema
      , commandObservation = DiscardObservation
      , commandInput = Just (SomeHandleRef selfUpdateUserHandle)
      }

selfEmitAuditHandle :: Handle "emitAudit" 'E () NoObservation
selfEmitAuditHandle =
  e @"emitAudit"
    selfSinkName
    CommandSpec
      { commandArgumentSchema = unitSchema
      , commandObservation = DiscardObservation
      , commandInput = Just (SomeHandleRef selfDeleteUserHandle)
      }

selfSourceHandles :: SourceHandles
selfSourceHandles =
  SourceHandles
    { createUser = selfCreateUserHandle
    , readUser = selfReadUserHandle
    }

selfSinkHandles :: SinkHandles
selfSinkHandles =
  SinkHandles
    { updateUser = selfUpdateUserHandle
    , deleteUser = selfDeleteUserHandle
    , emitAudit = selfEmitAuditHandle
    }

selfUpdateUserImplementation :: ImplementationDecl
selfUpdateUserImplementation =
  eraseImplementation
    (implU selfUpdateUserHandle (rRef selfReadUserHandle))

selfEffectSystemDeclarations :: Either [RecordError] [EffectSystemDecl]
selfEffectSystemDeclarations = do
  sourceDeclaration <-
    effectSystemFromRecord
      selfSourceName
      []
      selfSourceHandles
      []
      [handleId selfCreateUserHandle, handleId selfReadUserHandle]
  sinkDeclaration <-
    effectSystemFromRecord
      selfSinkName
      [selfSourceName]
      selfSinkHandles
      []
      [handleId selfEmitAuditHandle]
  pure [sourceDeclaration, sinkDeclaration]

selfAstBlueprint :: AstBlueprintSeed
selfAstBlueprint =
  AstBlueprintSeed
    { astBlueprintSeedBoot =
        SeedWithImplementation
          selfUpdateUserImplementation
          ( SeedLeaf
              (HandleTarget (handleRefFor selfEmitAuditHandle))
          )
    , astBlueprintSeedHanging = []
    }

selfModel :: Either [RecordError] SelfModel
selfModel = do
  currentSystems <- selfEffectSystemDeclarations
  pure
    SelfModel
      { selfModelSchema = selfModelSchemaV1
      , selfModelSdkVersion = "0.1.0.0"
      , selfModelEffectSystems = currentSystems
      , selfModelAstSeed = selfAstBlueprint
      , selfModelHandlerCoverage =
          [ handleId selfCreateUserHandle
          , handleId selfReadUserHandle
          , handleId selfUpdateUserHandle
          , handleId selfDeleteUserHandle
          , handleId selfEmitAuditHandle
          ]
      }

selfModelSourceInput :: SelfModel -> SdkSourceInput
selfModelSourceInput currentModel =
  mkSdkSourceInput
    selfModelCoreRef
    (selfModelSdkVersion currentModel)
    (selfModelEffectSystems currentModel)
    (selfModelAstSeed currentModel)
    ( Just
        ( handlerCoverageFromIds
            (selfModelHandlerCoverage currentModel)
        )
    )

selfModelCoreRef :: TrustBaseRef
selfModelCoreRef =
  TrustBaseRef
    { trustBaseCoreId = CoreId "self-model-fixture"
    , trustBaseArtifactDigest =
        Digest (sha256 "self-model-fixture-artifact")
    , trustBaseManifestDigest =
        Digest (sha256 "self-model-fixture-manifest")
    , trustBaseSchema =
        SchemaId
          (SchemaName "self-model-fixture")
          (SchemaVersion 1)
    , trustBaseKernelClaims =
        [ClaimName "curde-coverage-fixture"]
    }

canonicalSelfModelJson :: SelfModel -> String
canonicalSelfModelJson =
  renderSdkSourceArtifactJson
    . sdkSourceReportArtifact
    . buildSdkSourceReport
    . selfModelSourceInput

decodeSelfModelJson :: String -> Either String SelfModel
decodeSelfModelJson currentText = do
  currentJson <- parseJson currentText
  currentSchema <-
    jsonObjectField "schema" currentJson >>= jsonStringValue
  if currentSchema /= sdkSourceArtifactSchemaV1
    then Left "unsupported SelfModel JSON schema"
    else pure ()
  currentVersion <-
    jsonObjectField "sdkVersion" currentJson >>= jsonStringValue
  currentSystemValues <-
    jsonObjectField "curde" currentJson >>= jsonArrayItems
  currentSystems <- traverse decodeEffectSystem currentSystemValues
  currentAstValue <- jsonObjectField "ast" currentJson
  currentAstEncoding <-
    jsonObjectField "encoding" currentAstValue >>= jsonStringValue
  if currentAstEncoding /= "AstBlueprintSeed.ReadShow.v1"
    then Left "unsupported AstBlueprintSeed JSON encoding"
    else pure ()
  currentAstText <-
    jsonObjectField "seed" currentAstValue >>= jsonStringValue
  currentAst <- readValue "AstBlueprintSeed" currentAstText
  currentCoverageValue <- jsonObjectField "handlerCoverage" currentJson
  currentCoverage <- decodeHandlerCoverage currentSystems currentCoverageValue
  pure
    SelfModel
      { selfModelSchema = selfModelSchemaV1
      , selfModelSdkVersion = currentVersion
      , selfModelEffectSystems = currentSystems
      , selfModelAstSeed = currentAst
      , selfModelHandlerCoverage = currentCoverage
      }

selfModelJsonRoundTrip :: SelfModel -> Bool
selfModelJsonRoundTrip currentModel =
  case decodeSelfModelJson currentJson of
    Left _ ->
      False
    Right decodedModel ->
      canonicalSelfModelJson decodedModel == currentJson
  where
    currentJson =
      canonicalSelfModelJson currentModel

decodeEffectSystem :: JsonValue -> Either String EffectSystemDecl
decodeEffectSystem currentValue = do
  currentEncoding <-
    jsonObjectField "encoding" currentValue >>= jsonStringValue
  if currentEncoding /= "EffectSystemDecl.ReadShow.v1"
    then Left "unsupported EffectSystemDecl JSON encoding"
    else pure ()
  currentIdentity <-
    jsonObjectField "identity" currentValue >>= jsonStringValue
  currentDeclarationText <-
    jsonObjectField "declaration" currentValue >>= jsonStringValue
  currentDeclaration <-
    readValue "EffectSystemDecl" currentDeclarationText
  if effectSystemNameText (effectSystemDeclName currentDeclaration)
      /= currentIdentity
    then Left "EffectSystemDecl identity mismatch"
    else Right currentDeclaration

decodeHandlerCoverage ::
  [EffectSystemDecl] ->
  JsonValue ->
  Either String [HandleId]
decodeHandlerCoverage _ JsonNull =
  Right []
decodeHandlerCoverage currentSystems currentValue = do
  currentKind <-
    jsonObjectField "kind" currentValue >>= jsonStringValue
  if currentKind /= "identity-only"
    then Left "unsupported handler coverage kind"
    else pure ()
  currentHandleValues <-
    jsonObjectField "handles" currentValue >>= jsonArrayItems
  currentHandleNames <- traverse jsonStringValue currentHandleValues
  traverse (resolveHandleId declaredHandles) currentHandleNames
  where
    declaredHandles =
      concatMap effectSystemDeclHandleIds currentSystems

resolveHandleId :: [HandleId] -> String -> Either String HandleId
resolveHandleId currentHandles currentName =
  case
      [ currentHandle
      | currentHandle <- currentHandles
      , renderHandleId currentHandle == currentName
      ] of
    [currentHandle] ->
      Right currentHandle
    [] ->
      Left ("unknown handler coverage identity: " ++ currentName)
    _ ->
      Left ("ambiguous handler coverage identity: " ++ currentName)

readValue :: Read value => String -> String -> Either String value
readValue currentName currentText =
  case readMaybe currentText of
    Nothing ->
      Left ("invalid " ++ currentName)
    Just currentValue ->
      Right currentValue
encodeSelfModel :: SelfModel -> String
encodeSelfModel =
  show

decodeSelfModel :: String -> Either String SelfModel
decodeSelfModel currentText =
  case readMaybe currentText of
    Nothing ->
      Left "invalid SelfModel"
    Just currentModel
      | selfModelSchema currentModel /= selfModelSchemaV1 ->
          Left "unsupported SelfModel schema"
      | otherwise ->
          Right currentModel

selfModelTextRoundTrip :: SelfModel -> Bool
selfModelTextRoundTrip currentModel =
  decodeSelfModel (encodeSelfModel currentModel)
    == Right currentModel
