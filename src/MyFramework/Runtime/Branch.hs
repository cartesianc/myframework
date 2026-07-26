module MyFramework.Runtime.Branch
  ( BranchMergeConflict (..)
  , BranchMergeResult (..)
  , BranchSnapshot (..)
  , RuntimeStateField (..)
  , mergeParallelBranches
  , mergeStrictBranchDelta
  ) where

import Data.List
  ( stripPrefix )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.CURDE.Core
  ( ImplementationId )
import MyFramework.CURDE.Types
  ( HandleId )
import MyFramework.Runtime.Observation
  ( ObservationMergeConflict (..)
  , ObservationStore
  , mergeObservationStoresConservative
  , mergeObservationStoresStrict
  )
import MyFramework.Runtime.State
  ( RuntimeEvent (..)
  , RuntimeState
  , RuntimeStateParts (..)
  , runtimeStateFromParts
  , runtimeStateParts
  )
import MyFramework.Runtime.Types

data BranchSnapshot = BranchSnapshot
  { branchRuntimeState :: RuntimeState
  , branchObservationStore :: ObservationStore
  }
  deriving (Eq, Show)

data RuntimeStateField
  = ExecutionStatusField
  | ReadStatusField
  | ValidityField
  | ImplementationStatusField
  | ReadValueField
  | ObservationField
  deriving (Eq, Ord, Show)

data BranchMergeConflict
  = BranchValueConflict
      Int
      RuntimeStateField
      String
      String
      String
      String
  | BranchObservationConflict Int HandleId
  | BranchEventPrefixConflict
      Int
      [RuntimeEvent]
      [RuntimeEvent]
      [RuntimeEvent]
  deriving (Eq, Show)

data BranchMergeResult = BranchMergeResult
  { branchMergeSnapshot :: BranchSnapshot
  , branchMergeConflicts :: [BranchMergeConflict]
  }
  deriving (Eq, Show)

-- | Strict three-way merge for single-flight and fallback. A conflict returns
-- no replacement state, so the caller can preserve its current session.
mergeStrictBranchDelta ::
  Int ->
  BranchSnapshot ->
  BranchSnapshot ->
  BranchSnapshot ->
  Either [BranchMergeConflict] BranchSnapshot
mergeStrictBranchDelta currentIndex currentBase currentMerged currentBranch =
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
    mergedInputParts =
      runtimeStateParts (branchRuntimeState currentMerged)
    branchParts =
      runtimeStateParts (branchRuntimeState currentBranch)

    (mergedExecution, executionConflicts) =
      strictMergeMap
        currentIndex
        ExecutionStatusField
        (runtimePartExecutionStatuses baseParts)
        (runtimePartExecutionStatuses mergedInputParts)
        (runtimePartExecutionStatuses branchParts)
    (mergedReads, readConflicts) =
      strictMergeMap
        currentIndex
        ReadStatusField
        (runtimePartReadStatuses baseParts)
        (runtimePartReadStatuses mergedInputParts)
        (runtimePartReadStatuses branchParts)
    (mergedValidities, validityConflicts) =
      strictMergeMap
        currentIndex
        ValidityField
        (runtimePartValidities baseParts)
        (runtimePartValidities mergedInputParts)
        (runtimePartValidities branchParts)
    (mergedImplementations, implementationConflicts) =
      strictMergeMap
        currentIndex
        ImplementationStatusField
        (runtimePartImplementationStatuses baseParts)
        (runtimePartImplementationStatuses mergedInputParts)
        (runtimePartImplementationStatuses branchParts)
    (mergedReadValues, readValueConflicts) =
      strictMergeMap
        currentIndex
        ReadValueField
        (runtimePartReadValues baseParts)
        (runtimePartReadValues mergedInputParts)
        (runtimePartReadValues branchParts)

    observationResult =
      mergeObservationStoresStrict
        (branchObservationStore currentBase)
        (branchObservationStore currentMerged)
        (branchObservationStore currentBranch)
    (mergedObservations, observationConflicts) =
      case observationResult of
        Right currentStore ->
          (currentStore, [])
        Left currentErrors ->
          ( branchObservationStore currentMerged
          , map
              (observationConflict currentIndex)
              currentErrors
          )

    (mergedEvents, eventConflicts) =
      strictMergeEvents
        currentIndex
        (runtimePartEvents baseParts)
        (runtimePartEvents mergedInputParts)
        (runtimePartEvents branchParts)

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

-- | Parallel uses declaration order and continues after conflicts. Each
-- conflict is normalized conservatively instead of choosing a branch value.
mergeParallelBranches ::
  BranchSnapshot ->
  [BranchSnapshot] ->
  BranchMergeResult
mergeParallelBranches currentBase =
  foldl mergeParallelOne initialResult . zip [0 ..]
  where
    initialResult =
      BranchMergeResult
        { branchMergeSnapshot = currentBase
        , branchMergeConflicts = []
        }
    mergeParallelOne currentResult (currentIndex, currentBranch) =
      let nextResult =
            mergeConservativeBranch
              currentIndex
              currentBase
              (branchMergeSnapshot currentResult)
              currentBranch
       in nextResult
            { branchMergeConflicts =
                branchMergeConflicts currentResult
                  ++ branchMergeConflicts nextResult
            }

mergeConservativeBranch ::
  Int ->
  BranchSnapshot ->
  BranchSnapshot ->
  BranchSnapshot ->
  BranchMergeResult
mergeConservativeBranch currentIndex currentBase currentMerged currentBranch =
  BranchMergeResult
    { branchMergeSnapshot =
        BranchSnapshot
          { branchRuntimeState =
              runtimeStateFromParts mergedParts
          , branchObservationStore = mergedObservations
          }
    , branchMergeConflicts = currentConflicts
    }
  where
    baseParts =
      runtimeStateParts (branchRuntimeState currentBase)
    mergedInputParts =
      runtimeStateParts (branchRuntimeState currentMerged)
    branchParts =
      runtimeStateParts (branchRuntimeState currentBranch)

    (mergedExecution, executionConflicts, _) =
      conservativeMergeMap
        currentIndex
        ExecutionStatusField
        executionConflictValue
        (runtimePartExecutionStatuses baseParts)
        (runtimePartExecutionStatuses mergedInputParts)
        (runtimePartExecutionStatuses branchParts)
    (mergedReadsBeforeValues, readConflicts, _) =
      conservativeMergeMap
        currentIndex
        ReadStatusField
        readConflictValue
        (runtimePartReadStatuses baseParts)
        (runtimePartReadStatuses mergedInputParts)
        (runtimePartReadStatuses branchParts)
    (mergedValiditiesBeforeForced, validityConflicts, _) =
      conservativeMergeMap
        currentIndex
        ValidityField
        (const (Just Suspect))
        (runtimePartValidities baseParts)
        (runtimePartValidities mergedInputParts)
        (runtimePartValidities branchParts)
    (mergedImplementations, implementationConflicts, _) =
      conservativeMergeMap
        currentIndex
        ImplementationStatusField
        implementationConflictValue
        (runtimePartImplementationStatuses baseParts)
        (runtimePartImplementationStatuses mergedInputParts)
        (runtimePartImplementationStatuses branchParts)
    (mergedReadValues, readValueConflicts, readValueConflictIds) =
      conservativeMergeMap
        currentIndex
        ReadValueField
        (const Nothing)
        (runtimePartReadValues baseParts)
        (runtimePartReadValues mergedInputParts)
        (runtimePartReadValues branchParts)

    mergedReads =
      foldl
        (\currentMap currentId ->
            Map.insert
              currentId
              (ReadFailed (readValueConflictFailure currentIndex currentId))
              currentMap
        )
        mergedReadsBeforeValues
        readValueConflictIds
    mergedValidities =
      foldl
        (\currentMap currentId ->
            Map.insert currentId Suspect currentMap
        )
        mergedValiditiesBeforeForced
        readValueConflictIds

    (mergedObservations, rawObservationConflicts) =
      mergeObservationStoresConservative
        (branchObservationStore currentBase)
        (branchObservationStore currentMerged)
        (branchObservationStore currentBranch)
    observationConflicts =
      map
        (observationConflict currentIndex)
        rawObservationConflicts

    (mergedEventsBeforeForced, eventConflicts) =
      conservativeMergeEvents
        currentIndex
        (runtimePartEvents baseParts)
        (runtimePartEvents mergedInputParts)
        (runtimePartEvents branchParts)
    forcedReadEvents =
      concatMap
        (\currentId ->
            [ ReadStatusChanged
                currentId
                (ReadFailed (readValueConflictFailure currentIndex currentId))
            , ValidityChanged currentId Suspect
            ]
        )
        readValueConflictIds

    mergedParts =
      RuntimeStateParts
        { runtimePartExecutionStatuses = mergedExecution
        , runtimePartReadStatuses = mergedReads
        , runtimePartValidities = mergedValidities
        , runtimePartImplementationStatuses = mergedImplementations
        , runtimePartReadValues = mergedReadValues
        , runtimePartEvents =
            mergedEventsBeforeForced ++ forcedReadEvents
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
  Int ->
  RuntimeStateField ->
  Map key value ->
  Map key value ->
  Map key value ->
  (Map key value, [BranchMergeConflict])
strictMergeMap currentIndex currentField currentBase currentMerged currentBranch =
  (nextMap, currentConflicts)
  where
    (nextMap, currentConflicts, _) =
      mergeMap
        currentIndex
        currentField
        (\_ -> Nothing)
        False
        currentBase
        currentMerged
        currentBranch

conservativeMergeMap ::
  (Ord key, Show key, Eq value, Show value) =>
  Int ->
  RuntimeStateField ->
  (key -> Maybe value) ->
  Map key value ->
  Map key value ->
  Map key value ->
  (Map key value, [BranchMergeConflict], [key])
conservativeMergeMap currentIndex currentField normalizeConflict =
  mergeMap
    currentIndex
    currentField
    normalizeConflict
    True

mergeMap ::
  (Ord key, Show key, Eq value, Show value) =>
  Int ->
  RuntimeStateField ->
  (key -> Maybe value) ->
  Bool ->
  Map key value ->
  Map key value ->
  Map key value ->
  (Map key value, [BranchMergeConflict], [key])
mergeMap currentIndex currentField normalizeConflict isConservative
  currentBase currentMerged currentBranch =
    foldl mergeKey (currentMerged, [], []) allKeys
  where
    allKeys =
      Set.toAscList
        ( Map.keysSet currentBase
            `Set.union` Map.keysSet currentMerged
            `Set.union` Map.keysSet currentBranch
        )
    mergeKey (nextMap, currentConflicts, conflictKeys) currentKey =
      let baseValue = Map.lookup currentKey currentBase
          mergedValue = Map.lookup currentKey nextMap
          branchValue = Map.lookup currentKey currentBranch
       in case mergeValue baseValue mergedValue branchValue of
            Right nextValue ->
              ( setMaybe currentKey nextValue nextMap
              , currentConflicts
              , conflictKeys
              )
            Left () ->
              ( if isConservative
                  then
                    setMaybe
                      currentKey
                      (normalizeConflict currentKey)
                      nextMap
                  else nextMap
              , currentConflicts
                  ++ [ BranchValueConflict
                         currentIndex
                         currentField
                         (show currentKey)
                         (show baseValue)
                         (show mergedValue)
                         (show branchValue)
                     ]
              , conflictKeys ++ [currentKey]
              )

mergeValue ::
  Eq value =>
  Maybe value ->
  Maybe value ->
  Maybe value ->
  Either () (Maybe value)
mergeValue currentBase currentMerged currentBranch
  | currentBranch == currentBase =
      Right currentMerged
  | currentMerged == currentBase =
      Right currentBranch
  | currentMerged == currentBranch =
      Right currentMerged
  | otherwise =
      Left ()

setMaybe :: Ord key => key -> Maybe value -> Map key value -> Map key value
setMaybe currentKey currentValue currentMap =
  case currentValue of
    Nothing ->
      Map.delete currentKey currentMap
    Just nextValue ->
      Map.insert currentKey nextValue currentMap

strictMergeEvents ::
  Int ->
  [RuntimeEvent] ->
  [RuntimeEvent] ->
  [RuntimeEvent] ->
  ([RuntimeEvent], [BranchMergeConflict])
strictMergeEvents currentIndex currentBase currentMerged currentBranch =
  case
    ( stripPrefix currentBase currentMerged
    , stripPrefix currentBase currentBranch
    ) of
    (Just _, Just branchDelta) ->
      (currentMerged ++ branchDelta, [])
    _ ->
      ( currentMerged
      , [ BranchEventPrefixConflict
            currentIndex
            currentBase
            currentMerged
            currentBranch
        ]
      )

conservativeMergeEvents ::
  Int ->
  [RuntimeEvent] ->
  [RuntimeEvent] ->
  [RuntimeEvent] ->
  ([RuntimeEvent], [BranchMergeConflict])
conservativeMergeEvents currentIndex currentBase currentMerged currentBranch =
  case stripPrefix currentBase currentBranch of
    Just branchDelta ->
      (currentMerged ++ branchDelta, [])
    Nothing ->
      ( currentMerged ++ currentBranch
      , [ BranchEventPrefixConflict
            currentIndex
            currentBase
            currentMerged
            currentBranch
        ]
      )

observationConflict ::
  Int ->
  ObservationMergeConflict ->
  BranchMergeConflict
observationConflict currentIndex currentConflict =
  BranchObservationConflict
    currentIndex
    (observationMergeConflictHandle currentConflict)

executionConflictValue :: HandleId -> Maybe ExecutionStatus
executionConflictValue currentId =
  Just
    ( ExecutionOutcomeUnknown
        (stateConflictFailure
          HandlerPhase
          ExternalCommitUnknown
          ExecutionStatusField
          (show currentId)
        )
    )

readConflictValue :: HandleId -> Maybe ReadStatus
readConflictValue currentId =
  Just
    ( ReadFailed
        (stateConflictFailure
          ReadPhase
          NoExternalCommit
          ReadStatusField
          (show currentId)
        )
    )

implementationConflictValue ::
  ImplementationId ->
  Maybe ImplementationStatus
implementationConflictValue currentId =
  Just
    ( ImplementationFailed
        (stateConflictFailure
          ImplementationPhase
          ExternalCommitUnknown
          ImplementationStatusField
          (show currentId)
        )
    )

readValueConflictFailure :: Int -> HandleId -> RuntimeFailure
readValueConflictFailure currentIndex currentId =
  stateConflictFailure
    ReadPhase
    NoExternalCommit
    ReadValueField
    (show currentIndex ++ ":" ++ show currentId)

stateConflictFailure ::
  FailurePhase ->
  CommitState ->
  RuntimeStateField ->
  String ->
  RuntimeFailure
stateConflictFailure currentPhase currentCommit currentField currentKey =
  RuntimeFailure
    { runtimeFailurePhase = currentPhase
    , runtimeFailureCommitState = currentCommit
    , runtimeFailureMessage =
        "parallel branch conflict normalized at "
          ++ show currentField
          ++ " "
          ++ currentKey
    }
