module MyFramework.Runtime.Demand.Delta
  ( HandleActionDeltaConflict (..)
  , projectHandleActionDelta
  ) where

import Data.List
  ( stripPrefix )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.CURDE.Types
  ( HandleId )
import MyFramework.Runtime.Branch
  ( BranchSnapshot (..)
  , RuntimeStateField (..)
  )
import MyFramework.Runtime.Observation
  ( ObservationMergeConflict (..)
  , mergeObservationStoresStrict
  )
import MyFramework.Runtime.State
  ( RuntimeEvent
  , RuntimeStateParts (..)
  , runtimeStateFromParts
  , runtimeStateParts
  )

data HandleActionDeltaConflict
  = HandleActionValueConflict
      RuntimeStateField
      String
      String
      String
      String
  | HandleActionObservationConflict HandleId
  | HandleActionEventBaseMismatch
      [RuntimeEvent]
      [RuntimeEvent]
  deriving (Eq, Show)

-- | Project only a completed Handle action. Unlike a generic branch merge,
-- the follower may have a different prerequisite-event history. State maps
-- and observations use strict three-way merge, while events append exactly
-- the suffix produced between the leader's action base and final snapshots.
projectHandleActionDelta ::
  BranchSnapshot ->
  BranchSnapshot ->
  BranchSnapshot ->
  Either [HandleActionDeltaConflict] BranchSnapshot
projectHandleActionDelta currentBase currentFollower currentFinal =
  case currentConflicts of
    [] ->
      Right
        BranchSnapshot
          { branchRuntimeState =
              runtimeStateFromParts mergedParts
          , branchObservationStore = mergedObservations
          }
    _ ->
      Left currentConflicts
  where
    baseParts =
      runtimeStateParts (branchRuntimeState currentBase)
    followerParts =
      runtimeStateParts (branchRuntimeState currentFollower)
    finalParts =
      runtimeStateParts (branchRuntimeState currentFinal)

    (mergedExecution, executionConflicts) =
      strictMergeMap
        ExecutionStatusField
        (runtimePartExecutionStatuses baseParts)
        (runtimePartExecutionStatuses followerParts)
        (runtimePartExecutionStatuses finalParts)
    (mergedReads, readConflicts) =
      strictMergeMap
        ReadStatusField
        (runtimePartReadStatuses baseParts)
        (runtimePartReadStatuses followerParts)
        (runtimePartReadStatuses finalParts)
    (mergedValidities, validityConflicts) =
      strictMergeMap
        ValidityField
        (runtimePartValidities baseParts)
        (runtimePartValidities followerParts)
        (runtimePartValidities finalParts)
    (mergedImplementations, implementationConflicts) =
      strictMergeMap
        ImplementationStatusField
        (runtimePartImplementationStatuses baseParts)
        (runtimePartImplementationStatuses followerParts)
        (runtimePartImplementationStatuses finalParts)
    (mergedReadValues, readValueConflicts) =
      strictMergeMap
        ReadValueField
        (runtimePartReadValues baseParts)
        (runtimePartReadValues followerParts)
        (runtimePartReadValues finalParts)

    observationResult =
      mergeObservationStoresStrict
        (branchObservationStore currentBase)
        (branchObservationStore currentFollower)
        (branchObservationStore currentFinal)
    (mergedObservations, observationConflicts) =
      case observationResult of
        Right currentStore ->
          (currentStore, [])
        Left currentErrors ->
          ( branchObservationStore currentFollower
          , map
              ( HandleActionObservationConflict
                  . observationMergeConflictHandle
              )
              currentErrors
          )

    (mergedEvents, eventConflicts) =
      projectActionEvents
        (runtimePartEvents baseParts)
        (runtimePartEvents followerParts)
        (runtimePartEvents finalParts)

    mergedParts =
      RuntimeStateParts
        { runtimePartExecutionStatuses = mergedExecution
        , runtimePartReadStatuses = mergedReads
        , runtimePartValidities = mergedValidities
        , runtimePartImplementationStatuses = mergedImplementations
        , runtimePartReadValues = mergedReadValues
        , runtimePartEvents = mergedEvents
        }

    currentConflicts =
      executionConflicts
        ++ readConflicts
        ++ validityConflicts
        ++ implementationConflicts
        ++ readValueConflicts
        ++ observationConflicts
        ++ eventConflicts

strictMergeMap ::
  (Ord key, Show key, Eq value, Show value) =>
  RuntimeStateField ->
  Map key value ->
  Map key value ->
  Map key value ->
  (Map key value, [HandleActionDeltaConflict])
strictMergeMap
  currentField
  currentBase
  currentFollower
  currentFinal =
    foldl mergeKey (currentFollower, []) allKeys
  where
    allKeys =
      Set.toAscList
        ( Map.keysSet currentBase
            `Set.union` Map.keysSet currentFollower
            `Set.union` Map.keysSet currentFinal
        )
    mergeKey (nextMap, currentConflicts) currentKey =
      let baseValue =
            Map.lookup currentKey currentBase
          followerValue =
            Map.lookup currentKey nextMap
          finalValue =
            Map.lookup currentKey currentFinal
       in case
            mergeValue
              baseValue
              followerValue
              finalValue of
            Right nextValue ->
              (setMaybe currentKey nextValue nextMap, currentConflicts)
            Left () ->
              ( nextMap
              , currentConflicts
                  ++ [ HandleActionValueConflict
                         currentField
                         (show currentKey)
                         (show baseValue)
                         (show followerValue)
                         (show finalValue)
                     ]
              )

mergeValue ::
  Eq value =>
  Maybe value ->
  Maybe value ->
  Maybe value ->
  Either () (Maybe value)
mergeValue currentBase currentFollower currentFinal
  | currentFinal == currentBase =
      Right currentFollower
  | currentFollower == currentBase =
      Right currentFinal
  | currentFollower == currentFinal =
      Right currentFollower
  | otherwise =
      Left ()

projectActionEvents ::
  [RuntimeEvent] ->
  [RuntimeEvent] ->
  [RuntimeEvent] ->
  ([RuntimeEvent], [HandleActionDeltaConflict])
projectActionEvents currentBase currentFollower currentFinal =
  case stripPrefix currentBase currentFinal of
    Just actionSuffix ->
      (currentFollower ++ actionSuffix, [])
    Nothing ->
      ( currentFollower
      , [ HandleActionEventBaseMismatch
            currentBase
            currentFinal
        ]
      )

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
