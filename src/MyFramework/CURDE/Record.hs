{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module MyFramework.CURDE.Record
  ( RecordError (..)
  , RecordHandles (recordHandles)
  , effectSystemFromRecord
  , sortRecordErrors
  ) where

import Data.Proxy
  ( Proxy (..) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
  ( C
  , D
  , Generic
  , K1 (..)
  , M1 (..)
  , Rep
  , S
  , Selector
  , U1 (..)
  , from
  , selName
  , (:*:) (..)
  , (:+:) (..)
  )
import GHC.TypeLits
  ( KnownSymbol
  , symbolVal
  )

import MyFramework.CURDE.Types

-- | Stable errors produced while a typed record is erased into the
-- serializable effect-system declaration.
data RecordError
  = RecordFieldSelectorMissing String HandleId
  | RecordFieldSymbolMismatch String String HandleId
  | RecordFieldNotHandle String
  | RecordSumTypeUnsupported
  | RecordDuplicateHandle HandleId
  | RecordHandleSystemMismatch EffectSystemName HandleId
  | RecordPrivateHandleMissing HandleId
  | RecordExportHandleMissing HandleId
  | RecordDuplicatePrivateHandle HandleId
  | RecordDuplicateExportHandle HandleId
  | RecordPrivateExportOverlap HandleId
  deriving (Eq, Ord, Read, Show)

-- | A user record is authoring sugar only. Every field must be a typed
-- 'Handle', and its selector must exactly match the handle's type-level
-- 'Symbol'. The result is immediately existentially erased.
class RecordHandles record where
  recordHandles :: record -> Either [RecordError] [SomeHandleRef]

instance
  {-# OVERLAPPABLE #-}
  (Generic record, GRecordHandles (Rep record)) =>
  RecordHandles record where
  recordHandles =
    either
      (Left . sortRecordErrors)
      Right
      . gRecordHandles
      . from

class GRecordHandles representation where
  gRecordHandles ::
    representation parameter ->
    Either [RecordError] [SomeHandleRef]

instance
  GRecordHandles fields =>
  GRecordHandles (M1 D metadata fields) where
  gRecordHandles (M1 currentFields) =
    gRecordHandles currentFields

instance
  GRecordHandles fields =>
  GRecordHandles (M1 C metadata fields) where
  gRecordHandles (M1 currentFields) =
    gRecordHandles currentFields

instance
  (GRecordHandles left, GRecordHandles right) =>
  GRecordHandles (left :*: right) where
  gRecordHandles (left :*: right) =
    combineRecordResults
      (gRecordHandles left)
      (gRecordHandles right)

instance GRecordHandles U1 where
  gRecordHandles U1 =
    Right []

instance GRecordHandles (left :+: right) where
  gRecordHandles _ =
    Left [RecordSumTypeUnsupported]

instance
  {-# OVERLAPPING #-}
  (Selector selector, KnownSymbol name) =>
  GRecordHandles
    (M1 S selector (K1 field (Handle name kind args result))) where
  gRecordHandles currentField@(M1 (K1 currentHandle))
    | null selectorName =
        Left
          [ RecordFieldSelectorMissing
              expectedSymbol
              (handleId currentHandle)
          ]
    | selectorName /= expectedSymbol =
        Left
          [ RecordFieldSymbolMismatch
              selectorName
              expectedSymbol
              (handleId currentHandle)
          ]
    | otherwise =
        Right [SomeHandleRef currentHandle]
    where
      selectorName =
        selName currentField
      expectedSymbol =
        symbolVal (Proxy @name)

instance
  {-# OVERLAPPABLE #-}
  Selector selector =>
  GRecordHandles (M1 S selector (K1 field value)) where
  gRecordHandles currentField =
    Left [RecordFieldNotHandle (selName currentField)]

-- | Erase a typed record directly to the serializable frontend value. The
-- argument order mirrors 'effectSystem': name, imports, handles, private,
-- exports, with the record replacing the explicit handle list.
effectSystemFromRecord ::
  RecordHandles record =>
  EffectSystemName ->
  [EffectSystemName] ->
  record ->
  [HandleId] ->
  [HandleId] ->
  Either [RecordError] EffectSystemDecl
effectSystemFromRecord
  currentName
  currentImports
  currentRecord
  currentPrivate
  currentExports = do
    currentHandles <- recordHandles currentRecord
    let currentHandleIds =
          map someHandleId currentHandles
        handleIdSet =
          Set.fromList currentHandleIds
        privateSet =
          Set.fromList currentPrivate
        exportSet =
          Set.fromList currentExports
        currentErrors =
          sortRecordErrors
            ( map RecordDuplicateHandle (duplicates currentHandleIds)
                ++
                  [ RecordHandleSystemMismatch currentName currentId
                  | currentId <- currentHandleIds
                  , handleIdEffectSystem currentId /= currentName
                  ]
                ++
                  [ RecordPrivateHandleMissing currentId
                  | currentId <- currentPrivate
                  , Set.notMember currentId handleIdSet
                  ]
                ++
                  [ RecordExportHandleMissing currentId
                  | currentId <- currentExports
                  , Set.notMember currentId handleIdSet
                  ]
                ++ map RecordDuplicatePrivateHandle (duplicates currentPrivate)
                ++ map RecordDuplicateExportHandle (duplicates currentExports)
                ++
                  map
                    RecordPrivateExportOverlap
                    (Set.toAscList (Set.intersection privateSet exportSet))
            )
    if null currentErrors
      then
        Right
          EffectSystemDecl
            { effectSystemDeclName = currentName
            , effectSystemDeclImports = currentImports
            , effectSystemDeclHandles = map eraseSomeHandle currentHandles
            , effectSystemDeclPrivate = currentPrivate
            , effectSystemDeclExports = currentExports
            }
      else
        Left currentErrors

sortRecordErrors :: [RecordError] -> [RecordError]
sortRecordErrors =
  Set.toAscList . Set.fromList

combineRecordResults ::
  Either [RecordError] [SomeHandleRef] ->
  Either [RecordError] [SomeHandleRef] ->
  Either [RecordError] [SomeHandleRef]
combineRecordResults leftResult rightResult =
  case (leftResult, rightResult) of
    (Right leftHandles, Right rightHandles) ->
      Right (leftHandles ++ rightHandles)
    (Left leftErrors, Left rightErrors) ->
      Left (leftErrors ++ rightErrors)
    (Left leftErrors, Right _) ->
      Left leftErrors
    (Right _, Left rightErrors) ->
      Left rightErrors

duplicates :: Ord value => [value] -> [value]
duplicates currentValues =
  Map.keys
    ( Map.filter
        (> (1 :: Int))
        (Map.fromListWith (+) [(currentValue, 1) | currentValue <- currentValues])
    )
