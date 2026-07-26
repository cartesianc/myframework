{-# LANGUAGE DataKinds #-}

module MyFramework.Runtime.Observation
  ( ObservationError (..)
  , ObservationMergeConflict (..)
  , ObservationStore
  , emptyObservationStore
  , mergeObservationStoresConservative
  , mergeObservationStoresStrict
  , publishObservation
  , publishObservationFor
  , publishObservationFailure
  , publishObservationFailureFor
  , readInputObservation
  , readInputObservationDecl
  , readInputObservationFor
  ) where

import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.CURDE.Types
  ( CURDE (R)
  , Handle
  , HandleDecl (..)
  , HandleId
  , ObservationContract (ObservationCaptured)
  , ReadSource (ReadFromInputObservation)
  , SchemaIdentity
  , SomeHandleRef (..)
  , someHandleId
  , someHandleInput
  , someHandleKind
  , someHandleObservationContract
  , someHandlePublicValueSchemaIdentity
  , someHandleReadSource
  )
import MyFramework.Runtime.Types
  ( RuntimeFailure )
import MyFramework.Runtime.Value
  ( RuntimeData
  , RuntimeValueError
  , validateRuntimeData
  )

data ObservationEntry
  = ObservationValue SchemaIdentity RuntimeData
  | ObservationFailure RuntimeFailure
  deriving (Eq, Show)

newtype ObservationStore = ObservationStore
  { storedObservations :: Map HandleId ObservationEntry
  }
  deriving (Eq, Show)

newtype ObservationMergeConflict = ObservationMergeConflict
  { observationMergeConflictHandle :: HandleId
  }
  deriving (Eq, Ord, Show)

data ObservationError
  = ObservationCaptureDisabled HandleId
  | ObservationValueInvalid HandleId RuntimeValueError
  | ObservationReadSourceMismatch HandleId
  | ObservationInputMissing HandleId
  | ObservationValueMissing HandleId HandleId
  | ObservationSourceFailed HandleId HandleId RuntimeFailure
  | ObservationValueSchemaMismatch
      HandleId
      SchemaIdentity
      SchemaIdentity
  deriving (Eq, Show)

emptyObservationStore :: ObservationStore
emptyObservationStore =
  ObservationStore Map.empty

publishObservation ::
  SomeHandleRef ->
  RuntimeData ->
  ObservationStore ->
  Either ObservationError ObservationStore
publishObservation currentHandle currentValue currentStore =
  case someHandleObservationContract currentHandle of
    Just (ObservationCaptured expectedSchema) ->
      case validateRuntimeData expectedSchema currentValue of
        Left currentError ->
          Left (ObservationValueInvalid currentId currentError)
        Right () ->
          Right
            ( ObservationStore
                ( Map.insert
                    currentId
                    (ObservationValue expectedSchema currentValue)
                    (storedObservations currentStore)
                )
            )
    _ ->
      Left (ObservationCaptureDisabled currentId)
  where
    currentId =
      someHandleId currentHandle

-- | Erased runtime counterpart of 'publishObservation'. Demand evaluation
-- consumes lowered 'HandleDecl' values and never reconstructs typed handles.
publishObservationFor ::
  HandleDecl ->
  RuntimeData ->
  ObservationStore ->
  Either ObservationError ObservationStore
publishObservationFor currentHandle currentValue currentStore =
  case handleDeclObservation currentHandle of
    Just (ObservationCaptured expectedSchema) ->
      case validateRuntimeData expectedSchema currentValue of
        Left currentError ->
          Left (ObservationValueInvalid currentId currentError)
        Right () ->
          Right
            ( ObservationStore
                ( Map.insert
                    currentId
                    (ObservationValue expectedSchema currentValue)
                    (storedObservations currentStore)
                )
            )
    _ ->
      Left (ObservationCaptureDisabled currentId)
  where
    currentId =
      handleDeclId currentHandle

publishObservationFailure ::
  SomeHandleRef ->
  RuntimeFailure ->
  ObservationStore ->
  Either ObservationError ObservationStore
publishObservationFailure currentHandle currentFailure currentStore =
  case someHandleObservationContract currentHandle of
    Just (ObservationCaptured _) ->
      Right
        ( ObservationStore
            ( Map.insert
                currentId
                (ObservationFailure currentFailure)
                (storedObservations currentStore)
            )
        )
    _ ->
      Left (ObservationCaptureDisabled currentId)
  where
    currentId =
      someHandleId currentHandle

publishObservationFailureFor ::
  HandleDecl ->
  RuntimeFailure ->
  ObservationStore ->
  Either ObservationError ObservationStore
publishObservationFailureFor currentHandle currentFailure currentStore =
  case handleDeclObservation currentHandle of
    Just (ObservationCaptured _) ->
      Right
        ( ObservationStore
            ( Map.insert
                currentId
                (ObservationFailure currentFailure)
                (storedObservations currentStore)
            )
        )
    _ ->
      Left (ObservationCaptureDisabled currentId)
  where
    currentId =
      handleDeclId currentHandle

readInputObservationFor ::
  SomeHandleRef ->
  ObservationStore ->
  Either ObservationError RuntimeData
readInputObservationFor currentRead currentStore
  | someHandleKind currentRead /= R
      || someHandleReadSource currentRead
        /= Just ReadFromInputObservation =
      Left (ObservationReadSourceMismatch currentReadId)
  | otherwise =
      case (someHandleInput currentRead, expectedSchema) of
        (Nothing, _) ->
          Left (ObservationInputMissing currentReadId)
        (_, Nothing) ->
          Left (ObservationReadSourceMismatch currentReadId)
        (Just currentInput, Just currentSchema) ->
          readStoredObservation
            currentReadId
            (someHandleId currentInput)
            currentSchema
            currentStore
  where
    currentReadId =
      someHandleId currentRead
    expectedSchema =
      someHandlePublicValueSchemaIdentity currentRead

readInputObservation ::
  Handle name 'R () value ->
  ObservationStore ->
  Either ObservationError RuntimeData
readInputObservation currentRead =
  readInputObservationFor (SomeHandleRef currentRead)

readInputObservationDecl ::
  HandleDecl ->
  ObservationStore ->
  Either ObservationError RuntimeData
readInputObservationDecl currentRead currentStore
  | handleDeclKind currentRead /= R
      || handleDeclReadSource currentRead
        /= Just ReadFromInputObservation =
      Left (ObservationReadSourceMismatch currentReadId)
  | otherwise =
      case
          ( handleDeclInput currentRead
          , handleDeclPublicValueSchema currentRead
          ) of
        (Nothing, _) ->
          Left (ObservationInputMissing currentReadId)
        (_, Nothing) ->
          Left (ObservationReadSourceMismatch currentReadId)
        (Just currentInput, Just currentSchema) ->
          readStoredObservation
            currentReadId
            currentInput
            currentSchema
            currentStore
  where
    currentReadId =
      handleDeclId currentRead

readStoredObservation ::
  HandleId ->
  HandleId ->
  SchemaIdentity ->
  ObservationStore ->
  Either ObservationError RuntimeData
readStoredObservation readId sourceId expectedSchema currentStore =
  case Map.lookup sourceId (storedObservations currentStore) of
    Nothing ->
      Left (ObservationValueMissing readId sourceId)
    Just (ObservationFailure currentFailure) ->
      Left (ObservationSourceFailed readId sourceId currentFailure)
    Just (ObservationValue currentSchema currentValue)
      | currentSchema /= expectedSchema ->
          Left
            ( ObservationValueSchemaMismatch
                readId
                expectedSchema
                currentSchema
            )
      | otherwise ->
          case validateRuntimeData expectedSchema currentValue of
            Left currentError ->
              Left (ObservationValueInvalid readId currentError)
            Right () ->
              Right currentValue

mergeObservationStoresStrict ::
  ObservationStore ->
  ObservationStore ->
  ObservationStore ->
  Either [ObservationMergeConflict] ObservationStore
mergeObservationStoresStrict currentBase currentMerged currentBranch =
  case currentConflicts of
    [] ->
      Right (ObservationStore nextEntries)
    _ ->
      Left currentConflicts
  where
    (nextEntries, currentConflicts) =
      mergeObservationEntries
        False
        (storedObservations currentBase)
        (storedObservations currentMerged)
        (storedObservations currentBranch)

-- | Conservative merge deletes a conflicting private observation. It never
-- changes the public CUDE execution status.
mergeObservationStoresConservative ::
  ObservationStore ->
  ObservationStore ->
  ObservationStore ->
  (ObservationStore, [ObservationMergeConflict])
mergeObservationStoresConservative currentBase currentMerged currentBranch =
  (ObservationStore nextEntries, currentConflicts)
  where
    (nextEntries, currentConflicts) =
      mergeObservationEntries
        True
        (storedObservations currentBase)
        (storedObservations currentMerged)
        (storedObservations currentBranch)

mergeObservationEntries ::
  Bool ->
  Map HandleId ObservationEntry ->
  Map HandleId ObservationEntry ->
  Map HandleId ObservationEntry ->
  (Map HandleId ObservationEntry, [ObservationMergeConflict])
mergeObservationEntries deleteOnConflict currentBase currentMerged currentBranch =
  foldl mergeKey (currentMerged, []) allKeys
  where
    allKeys =
      Set.toAscList
        ( Map.keysSet currentBase
            `Set.union` Map.keysSet currentMerged
            `Set.union` Map.keysSet currentBranch
        )
    mergeKey (nextMap, currentConflicts) currentKey =
      case
        mergeObservationValue
          (Map.lookup currentKey currentBase)
          (Map.lookup currentKey nextMap)
          (Map.lookup currentKey currentBranch) of
        Right nextValue ->
          (setMaybe currentKey nextValue nextMap, currentConflicts)
        Left () ->
          ( if deleteOnConflict
              then Map.delete currentKey nextMap
              else nextMap
          , currentConflicts
              ++ [ObservationMergeConflict currentKey]
          )

mergeObservationValue ::
  Maybe ObservationEntry ->
  Maybe ObservationEntry ->
  Maybe ObservationEntry ->
  Either () (Maybe ObservationEntry)
mergeObservationValue currentBase currentMerged currentBranch
  | currentBranch == currentBase =
      Right currentMerged
  | currentMerged == currentBase =
      Right currentBranch
  | currentMerged == currentBranch =
      Right currentMerged
  | otherwise =
      Left ()

setMaybe ::
  Ord key =>
  key ->
  Maybe value ->
  Map key value ->
  Map key value
setMaybe currentKey currentValue currentMap =
  case currentValue of
    Nothing ->
      Map.delete currentKey currentMap
    Just nextValue ->
      Map.insert currentKey nextValue currentMap
