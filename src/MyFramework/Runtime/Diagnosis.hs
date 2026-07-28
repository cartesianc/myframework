module MyFramework.Runtime.Diagnosis
  ( DiagnosisError (..)
  , DiagnosisFailureChannel (..)
  , DiagnosisImpact (..)
  , DiagnosisImpactKind (..)
  , DiagnosisNodeSnapshot (..)
  , DiagnosisReason (..)
  , DiagnosisReasonKind (..)
  , DiagnosisReport (..)
  , DiagnosisResult (..)
  , DiagnosisRoot (..)
  , DiagnosisTraceDirection (..)
  , DiagnosisTraceStep (..)
  , diagnoseDemand
  , diagnoseFailedDemands
  , diagnosisSchemaName
  , renderDiagnosisReportJson
  , renderDiagnosisResultJson
  ) where

import Data.Char
  ( ord )
import Data.List
  ( sortOn )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set
import Numeric
  ( showHex )

import MyFramework.Ast
  ( AstPath (..) )
import MyFramework.Control
  ( ControlNode (..)
  , ControlPlan (..)
  , ControlTree (..)
  )
import MyFramework.CURDE.Core
  ( DemandEdge (..)
  , DemandEdgeKind (..)
  , DemandGraph (..)
  , DemandNode (..)
  , DemandNodeId (..)
  , FieldPath (..)
  , renderImplementationId
  )
import MyFramework.CURDE.Types
  ( CURDE (R)
  , handleDeclKind
  , renderHandleId
  )
import MyFramework.Runtime.State
  ( RuntimeState
  , executionStatuses
  , implementationStatuses
  , readStatuses
  , validityFor
  )
import MyFramework.Runtime.Types
  ( CommitState (..)
  , ExecutionStatus (..)
  , FailurePhase (..)
  , ImplementationStatus (..)
  , ReadStatus (..)
  , RuntimeFailure (..)
  , Validity (..)
  )

-- | Versioned wire name for the deterministic JSON representation.
diagnosisSchemaName :: String
diagnosisSchemaName =
  "myframework.runtime-diagnosis.v1"

-- | Runtime channels remain independent. A failed R channel does not rewrite
-- or reinterpret a succeeded CUDE execution channel.
data DiagnosisFailureChannel
  = ExecutionFailureChannel
  | ReadFailureChannel
  | ImplementationFailureChannel
  deriving (Eq, Ord, Read, Show)

data DiagnosisReasonKind
  = ExecutionFailedReason
  | ExecutionOutcomeUnknownReason
  | ReadObservationUnavailableReason
  | ReadFailedReason
  | ImplementationFailedReason
  deriving (Eq, Ord, Read, Show)

data DiagnosisReason = DiagnosisReason
  { diagnosisReasonKind :: DiagnosisReasonKind
  , diagnosisReasonFailure :: RuntimeFailure
  }
  deriving (Eq, Show)

data DiagnosisRoot = DiagnosisRoot
  { diagnosisRootNode :: DemandNodeId
  , diagnosisRootChannel :: DiagnosisFailureChannel
  , diagnosisRootReason :: DiagnosisReason
  }
  deriving (Eq, Show)

data DiagnosisTraceDirection
  = TowardPrerequisite
  | TowardDependent
  deriving (Eq, Ord, Read, Show)

-- | Each trace step is a real DemandGraph edge. The optional AST path is
-- attribution metadata only and is never traversed as a dependency.
data DiagnosisTraceStep = DiagnosisTraceStep
  { diagnosisTraceDirection :: DiagnosisTraceDirection
  , diagnosisTraceFrom :: DemandNodeId
  , diagnosisTraceTo :: DemandNodeId
  , diagnosisTraceEdgeKind :: DemandEdgeKind
  , diagnosisTraceSourcePath :: Maybe AstPath
  }
  deriving (Eq, Ord, Show)

data DiagnosisNodeSnapshot = DiagnosisNodeSnapshot
  { diagnosisSnapshotNode :: DemandNodeId
  , diagnosisSnapshotExecutionStatus :: Maybe ExecutionStatus
  , diagnosisSnapshotReadStatus :: Maybe ReadStatus
  , diagnosisSnapshotValidity :: Maybe Validity
  , diagnosisSnapshotImplementationStatus ::
      Maybe ImplementationStatus
  }
  deriving (Eq, Show)

data DiagnosisImpactKind
  = DiagnosisRootImpact
  | DiagnosisSuspectImpact
  | DiagnosisPollutedImpact
  deriving (Eq, Ord, Read, Show)

-- | Control paths locate an impacted demand in the serializable control
-- projection. They do not contribute graph reachability.
data DiagnosisImpact = DiagnosisImpact
  { diagnosisImpactKind :: DiagnosisImpactKind
  , diagnosisImpactSnapshot :: DiagnosisNodeSnapshot
  , diagnosisImpactControlPaths :: [AstPath]
  }
  deriving (Eq, Show)

data DiagnosisReport = DiagnosisReport
  { diagnosisReportRoot :: DiagnosisRoot
  , diagnosisReportImpacts :: [DiagnosisImpact]
  , diagnosisReportTrace :: [DiagnosisTraceStep]
  }
  deriving (Eq, Show)

data DiagnosisError
  = DiagnosisRootMissing
      DemandNodeId
      DiagnosisFailureChannel
  | DiagnosisRootChannelMismatch
      DemandNodeId
      DiagnosisFailureChannel
  | DiagnosisRootNotFailed
      DemandNodeId
      DiagnosisFailureChannel
  deriving (Eq, Ord, Show)

data DiagnosisResult = DiagnosisResult
  { diagnosisResultReports :: [DiagnosisReport]
  , diagnosisResultErrors :: [DiagnosisError]
  }
  deriving (Eq, Show)

data RootFailure = RootFailure
  { rootFailureNode :: DemandNodeId
  , rootFailureChannel :: DiagnosisFailureChannel
  , rootFailureReason :: DiagnosisReason
  }

diagnoseFailedDemands ::
  DemandGraph ->
  ControlPlan ->
  RuntimeState ->
  DiagnosisResult
diagnoseFailedDemands currentGraph currentPlan currentState =
  DiagnosisResult
    { diagnosisResultReports =
        [ currentReport
        | Right currentReport <- currentResults
        ]
    , diagnosisResultErrors =
        [ currentError
        | Left currentError <- currentResults
        ]
    }
  where
    currentLocations =
      controlLocations currentPlan
    currentResults =
      [ diagnoseResolved
          currentGraph
          currentLocations
          currentState
          currentRoot
      | currentRoot <- failedRoots currentState
      ]

diagnoseDemand ::
  DemandGraph ->
  ControlPlan ->
  RuntimeState ->
  DiagnosisFailureChannel ->
  DemandNodeId ->
  Either DiagnosisError DiagnosisReport
diagnoseDemand
  currentGraph
  currentPlan
  currentState
  currentChannel
  currentNode = do
    currentRoot <-
      resolveRootFailure
        currentState
        currentChannel
        currentNode
    diagnoseResolved
      currentGraph
      (controlLocations currentPlan)
      currentState
      currentRoot

diagnoseResolved ::
  DemandGraph ->
  Map DemandNodeId [AstPath] ->
  RuntimeState ->
  RootFailure ->
  Either DiagnosisError DiagnosisReport
diagnoseResolved
  currentGraph
  currentLocations
  currentState
  currentRoot
    | Map.notMember
        (rootFailureNode currentRoot)
        (demandGraphNodes currentGraph) =
        Left
          ( DiagnosisRootMissing
              (rootFailureNode currentRoot)
              (rootFailureChannel currentRoot)
          )
    | otherwise =
        Right
          DiagnosisReport
            { diagnosisReportRoot =
                DiagnosisRoot
                  { diagnosisRootNode =
                      rootFailureNode currentRoot
                  , diagnosisRootChannel =
                      rootFailureChannel currentRoot
                  , diagnosisRootReason =
                      rootFailureReason currentRoot
                  }
            , diagnosisReportImpacts =
                rootImpact : suspectImpacts ++ pollutedImpacts
            , diagnosisReportTrace =
                uniqueSorted
                  (relevantUpstreamTrace ++ downstreamTrace)
            }
  where
    rootNode =
      rootFailureNode currentRoot
    (upstreamNodes, upstreamTrace) =
      walkDemandGraph
        TowardPrerequisite
        prerequisiteAdjacency
        currentGraph
        rootNode
    suspectNodes =
      [ currentNode
      | currentNode <- upstreamNodes
      , currentNode /= rootNode
      , nodeIsSuspect currentGraph currentState currentNode
      ]
    suspectSet =
      Set.fromList suspectNodes
    relevantUpstreamTrace =
      traceTowardTargets
        currentGraph
        prerequisiteAdjacency
        suspectSet
        upstreamTrace
    pollutionSources =
      Set.toAscList (Set.insert rootNode suspectSet)
    sourceDownstreamWalks =
      [ walkDemandGraph
          TowardDependent
          dependentAdjacency
          currentGraph
          currentSource
      | currentSource <- pollutionSources
      ]
    pollutedNodeSet =
      Set.unions
        [ Set.fromList currentNodes
        | (currentNodes, _) <- sourceDownstreamWalks
        ]
    pollutedNodes =
      Set.toAscList
        ( Set.delete rootNode pollutedNodeSet
            `Set.difference` suspectSet
        )
    downstreamTrace =
      uniqueSorted
        (concatMap snd sourceDownstreamWalks)
    rootImpact =
      impactFor
        currentLocations
        currentState
        DiagnosisRootImpact
        rootNode
    suspectImpacts =
      map
        ( impactFor
            currentLocations
            currentState
            DiagnosisSuspectImpact
        )
        suspectNodes
    pollutedImpacts =
      map
        ( impactFor
            currentLocations
            currentState
            DiagnosisPollutedImpact
        )
        pollutedNodes

resolveRootFailure ::
  RuntimeState ->
  DiagnosisFailureChannel ->
  DemandNodeId ->
  Either DiagnosisError RootFailure
resolveRootFailure currentState currentChannel currentNode =
  case (currentChannel, currentNode) of
    (ExecutionFailureChannel, HandleNode currentId) ->
      case Map.lookup currentId (executionStatuses currentState) of
        Just (ExecutionFailed currentFailure) ->
          succeeded
            (DiagnosisReason ExecutionFailedReason currentFailure)
        Just (ExecutionOutcomeUnknown currentFailure) ->
          succeeded
            ( DiagnosisReason
                ExecutionOutcomeUnknownReason
                currentFailure
            )
        _ ->
          notFailed
    (ReadFailureChannel, HandleNode currentId) ->
      case Map.lookup currentId (readStatuses currentState) of
        Just (ReadObservationUnavailable currentFailure) ->
          succeeded
            ( DiagnosisReason
                ReadObservationUnavailableReason
                currentFailure
            )
        Just (ReadFailed currentFailure) ->
          succeeded
            (DiagnosisReason ReadFailedReason currentFailure)
        _ ->
          notFailed
    (ImplementationFailureChannel, ImplementationNode currentId) ->
      case Map.lookup currentId (implementationStatuses currentState) of
        Just (ImplementationFailed currentFailure) ->
          succeeded
            ( DiagnosisReason
                ImplementationFailedReason
                currentFailure
            )
        _ ->
          notFailed
    _ ->
      Left
        (DiagnosisRootChannelMismatch currentNode currentChannel)
  where
    succeeded currentReason =
      Right
        RootFailure
          { rootFailureNode = currentNode
          , rootFailureChannel = currentChannel
          , rootFailureReason = currentReason
          }
    notFailed =
      Left (DiagnosisRootNotFailed currentNode currentChannel)

failedRoots :: RuntimeState -> [RootFailure]
failedRoots currentState =
  sortOn rootFailureSortKey
    (executionRoots ++ readRoots ++ implementationRoots)
  where
    executionRoots =
      [ RootFailure
          { rootFailureNode = HandleNode currentId
          , rootFailureChannel = ExecutionFailureChannel
          , rootFailureReason = currentReason
          }
      | (currentId, currentStatus) <-
          Map.toAscList (executionStatuses currentState)
      , currentReason <- executionFailureReason currentStatus
      ]
    readRoots =
      [ RootFailure
          { rootFailureNode = HandleNode currentId
          , rootFailureChannel = ReadFailureChannel
          , rootFailureReason = currentReason
          }
      | (currentId, currentStatus) <-
          Map.toAscList (readStatuses currentState)
      , currentReason <- readFailureReason currentStatus
      ]
    implementationRoots =
      [ RootFailure
          { rootFailureNode = ImplementationNode currentId
          , rootFailureChannel = ImplementationFailureChannel
          , rootFailureReason =
              DiagnosisReason
                ImplementationFailedReason
                currentFailure
          }
      | (currentId, ImplementationFailed currentFailure) <-
          Map.toAscList (implementationStatuses currentState)
      ]

executionFailureReason :: ExecutionStatus -> [DiagnosisReason]
executionFailureReason currentStatus =
  case currentStatus of
    ExecutionFailed currentFailure ->
      [DiagnosisReason ExecutionFailedReason currentFailure]
    ExecutionOutcomeUnknown currentFailure ->
      [ DiagnosisReason
          ExecutionOutcomeUnknownReason
          currentFailure
      ]
    _ ->
      []

readFailureReason :: ReadStatus -> [DiagnosisReason]
readFailureReason currentStatus =
  case currentStatus of
    ReadObservationUnavailable currentFailure ->
      [ DiagnosisReason
          ReadObservationUnavailableReason
          currentFailure
      ]
    ReadFailed currentFailure ->
      [DiagnosisReason ReadFailedReason currentFailure]
    _ ->
      []

rootFailureSortKey :: RootFailure -> (String, Int)
rootFailureSortKey currentRoot =
  ( renderDemandNodeId (rootFailureNode currentRoot)
  , channelOrder (rootFailureChannel currentRoot)
  )

channelOrder :: DiagnosisFailureChannel -> Int
channelOrder currentChannel =
  case currentChannel of
    ExecutionFailureChannel -> 0
    ReadFailureChannel -> 1
    ImplementationFailureChannel -> 2

impactFor ::
  Map DemandNodeId [AstPath] ->
  RuntimeState ->
  DiagnosisImpactKind ->
  DemandNodeId ->
  DiagnosisImpact
impactFor currentLocations currentState currentKind currentNode =
  DiagnosisImpact
    { diagnosisImpactKind = currentKind
    , diagnosisImpactSnapshot =
        snapshotNode currentState currentNode
    , diagnosisImpactControlPaths =
        Map.findWithDefault [] currentNode currentLocations
    }

snapshotNode ::
  RuntimeState ->
  DemandNodeId ->
  DiagnosisNodeSnapshot
snapshotNode currentState currentNode =
  case currentNode of
    HandleNode currentId ->
      DiagnosisNodeSnapshot
        { diagnosisSnapshotNode = currentNode
        , diagnosisSnapshotExecutionStatus =
            Map.lookup currentId (executionStatuses currentState)
        , diagnosisSnapshotReadStatus =
            Map.lookup currentId (readStatuses currentState)
        , diagnosisSnapshotValidity =
            Just (validityFor currentId currentState)
        , diagnosisSnapshotImplementationStatus =
            Nothing
        }
    ImplementationNode currentId ->
      DiagnosisNodeSnapshot
        { diagnosisSnapshotNode = currentNode
        , diagnosisSnapshotExecutionStatus = Nothing
        , diagnosisSnapshotReadStatus = Nothing
        , diagnosisSnapshotValidity = Nothing
        , diagnosisSnapshotImplementationStatus =
            Map.lookup currentId (implementationStatuses currentState)
        }

nodeIsSuspect ::
  DemandGraph ->
  RuntimeState ->
  DemandNodeId ->
  Bool
nodeIsSuspect currentGraph currentState currentNode =
  not (nodeIsHealthy currentGraph currentState currentNode)

nodeIsHealthy ::
  DemandGraph ->
  RuntimeState ->
  DemandNodeId ->
  Bool
nodeIsHealthy currentGraph currentState currentNode =
  case
      ( currentNode
      , Map.lookup currentNode (demandGraphNodes currentGraph)
      ) of
    (HandleNode currentId, Just (DemandHandleNode currentHandle))
      | handleDeclKind currentHandle == R ->
          handleValidityIsTrusted currentId
            && Map.lookup currentId (readStatuses currentState)
              == Just ReadAvailable
      | otherwise ->
          handleValidityIsTrusted currentId
            && Map.lookup currentId (executionStatuses currentState)
              == Just ExecutionSucceeded
    ( ImplementationNode currentId
      , Just (DemandImplementationNode _ _)
      ) ->
        Map.lookup currentId (implementationStatuses currentState)
          == Just ImplementationSucceeded
    _ ->
      False
  where
    handleValidityIsTrusted currentId =
      validityFor currentId currentState == Trusted

type DemandAdjacency =
  Map DemandNodeId [(DemandNodeId, DemandEdge)]

-- | InputDependency and ArgumentUse are the real dependency relations.
-- InvokeImplementation is retained only as the structural bridge between an
-- implementation node and the CUDE handle whose status it realizes.
diagnosticEdges :: DemandGraph -> [DemandEdge]
diagnosticEdges =
  filter isDiagnosticEdge . demandGraphEdges

isDiagnosticEdge :: DemandEdge -> Bool
isDiagnosticEdge currentEdge =
  case demandEdgeKind currentEdge of
    InputDependency -> True
    ArgumentUse _ -> True
    InvokeImplementation -> True

prerequisiteAdjacency :: DemandGraph -> DemandAdjacency
prerequisiteAdjacency currentGraph =
  Map.fromListWith
    (++)
    [ ( demandEdgeDependent currentEdge
      , [(demandEdgePrerequisite currentEdge, currentEdge)]
      )
    | currentEdge <- diagnosticEdges currentGraph
    ]

dependentAdjacency :: DemandGraph -> DemandAdjacency
dependentAdjacency currentGraph =
  Map.fromListWith
    (++)
    [ ( demandEdgePrerequisite currentEdge
      , [(demandEdgeDependent currentEdge, currentEdge)]
      )
    | currentEdge <- diagnosticEdges currentGraph
    ]

walkDemandGraph ::
  DiagnosisTraceDirection ->
  (DemandGraph -> DemandAdjacency) ->
  DemandGraph ->
  DemandNodeId ->
  ([DemandNodeId], [DiagnosisTraceStep])
walkDemandGraph
  currentDirection
  buildAdjacency
  currentGraph
  currentRoot =
    visit Set.empty [currentRoot] []
  where
    currentAdjacency =
      fmap
        (sortOn (renderDemandNodeId . fst))
        (buildAdjacency currentGraph)
    visit ::
      Set DemandNodeId ->
      [DemandNodeId] ->
      [DiagnosisTraceStep] ->
      ([DemandNodeId], [DiagnosisTraceStep])
    visit visited [] currentTrace =
      (Set.toAscList visited, uniqueSorted currentTrace)
    visit visited (currentNode : remaining) currentTrace
      | currentNode `Set.member` visited =
          visit visited remaining currentTrace
      | otherwise =
          visit
            (Set.insert currentNode visited)
            (map fst currentNextNodes ++ remaining)
            (map (traceStep currentNode) currentNextNodes ++ currentTrace)
      where
        currentNextNodes =
          Map.findWithDefault [] currentNode currentAdjacency
    traceStep currentNode (nextNode, currentEdge) =
      DiagnosisTraceStep
        { diagnosisTraceDirection = currentDirection
        , diagnosisTraceFrom = currentNode
        , diagnosisTraceTo = nextNode
        , diagnosisTraceEdgeKind =
            demandEdgeKind currentEdge
        , diagnosisTraceSourcePath =
            demandEdgeSourcePath currentEdge
        }

traceTowardTargets ::
  DemandGraph ->
  (DemandGraph -> DemandAdjacency) ->
  Set DemandNodeId ->
  [DiagnosisTraceStep] ->
  [DiagnosisTraceStep]
traceTowardTargets
  currentGraph
  buildAdjacency
  currentTargets =
    filter reachesTarget
  where
    reachesTarget currentStep =
      not
        ( Set.null
            ( Set.intersection
                currentTargets
                ( reachableNodes
                    buildAdjacency
                    currentGraph
                    (diagnosisTraceTo currentStep)
                )
            )
        )

reachableNodes ::
  (DemandGraph -> DemandAdjacency) ->
  DemandGraph ->
  DemandNodeId ->
  Set DemandNodeId
reachableNodes buildAdjacency currentGraph currentRoot =
  Set.fromList
    ( fst
        ( walkDemandGraph
            TowardPrerequisite
            buildAdjacency
            currentGraph
            currentRoot
        )
    )

controlLocations :: ControlPlan -> Map DemandNodeId [AstPath]
controlLocations currentPlan =
  fmap uniqueSorted
    ( Map.fromListWith
        (++)
        (controlLocationsFromTree (controlPlanBoot currentPlan))
    )

controlLocationsFromTree ::
  ControlTree ->
  [(DemandNodeId, [AstPath])]
controlLocationsFromTree currentTree =
  currentLocation ++ childLocations
  where
    currentPath =
      controlTreePath currentTree
    currentNode =
      controlTreeNode currentTree
    currentLocation =
      case currentNode of
        ControlDemand currentDemand ->
          [(currentDemand, [currentPath])]
        ControlWithImplementation currentImplementation _ ->
          [(ImplementationNode currentImplementation, [currentPath])]
        _ ->
          []
    childLocations =
      concatMap controlLocationsFromTree (controlChildren currentNode)

controlChildren :: ControlNode -> [ControlTree]
controlChildren currentNode =
  case currentNode of
    ControlDemand _ -> []
    ControlWithImplementation _ currentChild -> [currentChild]
    ControlSequence currentChildren -> currentChildren
    ControlParallel currentChildren -> currentChildren
    ControlFallback currentChildren -> currentChildren
    ControlRace currentChildren -> currentChildren
    ControlChoice _ currentBranches -> map snd currentBranches
    ControlWait _ currentChild -> [currentChild]
    ControlLoop currentChild -> [currentChild]
    ControlMiddleware _ currentChild -> [currentChild]
    ControlCallback _ currentChild -> [currentChild]
    ControlSuspense _ -> []

renderDiagnosisResultJson :: DiagnosisResult -> String
renderDiagnosisResultJson currentResult =
  jsonObject
    [ jsonField "schema" (jsonString diagnosisSchemaName)
    , jsonField
        "reports"
        ( jsonArray
            ( map
                renderDiagnosisReportJson
                (diagnosisResultReports currentResult)
            )
        )
    , jsonField
        "errors"
        ( jsonArray
            ( map
                renderDiagnosisErrorJson
                (diagnosisResultErrors currentResult)
            )
        )
    ]

renderDiagnosisReportJson :: DiagnosisReport -> String
renderDiagnosisReportJson currentReport =
  jsonObject
    [ jsonField
        "schema"
        (jsonString diagnosisSchemaName)
    , jsonField
        "root"
        (renderDiagnosisRootJson (diagnosisReportRoot currentReport))
    , jsonField
        "impacts"
        ( jsonArray
            ( map
                renderDiagnosisImpactJson
                (diagnosisReportImpacts currentReport)
            )
        )
    , jsonField
        "trace"
        ( jsonArray
            ( map
                renderDiagnosisTraceJson
                (diagnosisReportTrace currentReport)
            )
        )
    ]

renderDiagnosisRootJson :: DiagnosisRoot -> String
renderDiagnosisRootJson currentRoot =
  jsonObject
    [ jsonField
        "node"
        (renderDemandNodeJson (diagnosisRootNode currentRoot))
    , jsonField
        "channel"
        ( jsonString
            (renderFailureChannel (diagnosisRootChannel currentRoot))
        )
    , jsonField
        "reason"
        (renderDiagnosisReasonJson (diagnosisRootReason currentRoot))
    ]

renderDiagnosisReasonJson :: DiagnosisReason -> String
renderDiagnosisReasonJson currentReason =
  jsonObject
    [ jsonField
        "kind"
        (jsonString (renderReasonKind (diagnosisReasonKind currentReason)))
    , jsonField
        "failure"
        (renderRuntimeFailureJson (diagnosisReasonFailure currentReason))
    ]

renderDiagnosisImpactJson :: DiagnosisImpact -> String
renderDiagnosisImpactJson currentImpact =
  jsonObject
    [ jsonField
        "kind"
        ( jsonString
            (renderImpactKind (diagnosisImpactKind currentImpact))
        )
    , jsonField
        "snapshot"
        ( renderDiagnosisSnapshotJson
            (diagnosisImpactSnapshot currentImpact)
        )
    , jsonField
        "controlPaths"
        ( jsonArray
            ( map
                renderAstPathJson
                (diagnosisImpactControlPaths currentImpact)
            )
        )
    ]

renderDiagnosisSnapshotJson :: DiagnosisNodeSnapshot -> String
renderDiagnosisSnapshotJson currentSnapshot =
  jsonObject
    [ jsonField
        "node"
        (renderDemandNodeJson (diagnosisSnapshotNode currentSnapshot))
    , jsonField
        "execution"
        ( jsonMaybe
            renderExecutionStatusJson
            (diagnosisSnapshotExecutionStatus currentSnapshot)
        )
    , jsonField
        "read"
        ( jsonMaybe
            renderReadStatusJson
            (diagnosisSnapshotReadStatus currentSnapshot)
        )
    , jsonField
        "validity"
        ( jsonMaybe
            renderValidityJson
            (diagnosisSnapshotValidity currentSnapshot)
        )
    , jsonField
        "implementation"
        ( jsonMaybe
            renderImplementationStatusJson
            (diagnosisSnapshotImplementationStatus currentSnapshot)
        )
    ]

renderDiagnosisTraceJson :: DiagnosisTraceStep -> String
renderDiagnosisTraceJson currentStep =
  jsonObject
    [ jsonField
        "direction"
        ( jsonString
            (renderTraceDirection (diagnosisTraceDirection currentStep))
        )
    , jsonField
        "from"
        (renderDemandNodeJson (diagnosisTraceFrom currentStep))
    , jsonField
        "to"
        (renderDemandNodeJson (diagnosisTraceTo currentStep))
    , jsonField
        "dependency"
        (renderDemandEdgeKindJson (diagnosisTraceEdgeKind currentStep))
    , jsonField
        "sourcePath"
        ( jsonMaybe
            renderAstPathJson
            (diagnosisTraceSourcePath currentStep)
        )
    ]

renderDiagnosisErrorJson :: DiagnosisError -> String
renderDiagnosisErrorJson currentError =
  case currentError of
    DiagnosisRootMissing currentNode currentChannel ->
      errorObject "root-missing" currentNode currentChannel
    DiagnosisRootChannelMismatch currentNode currentChannel ->
      errorObject "root-channel-mismatch" currentNode currentChannel
    DiagnosisRootNotFailed currentNode currentChannel ->
      errorObject "root-not-failed" currentNode currentChannel
  where
    errorObject currentKind currentNode currentChannel =
      jsonObject
        [ jsonField "kind" (jsonString currentKind)
        , jsonField "node" (renderDemandNodeJson currentNode)
        , jsonField
            "channel"
            (jsonString (renderFailureChannel currentChannel))
        ]

renderDemandNodeJson :: DemandNodeId -> String
renderDemandNodeJson currentNode =
  case currentNode of
    HandleNode currentId ->
      jsonObject
        [ jsonField "kind" (jsonString "handle")
        , jsonField "id" (jsonString (renderHandleId currentId))
        ]
    ImplementationNode currentId ->
      jsonObject
        [ jsonField "kind" (jsonString "implementation")
        , jsonField
            "id"
            (jsonString (renderImplementationId currentId))
        ]

renderDemandNodeId :: DemandNodeId -> String
renderDemandNodeId currentNode =
  case currentNode of
    HandleNode currentId ->
      "handle:" ++ renderHandleId currentId
    ImplementationNode currentId ->
      renderImplementationId currentId

renderDemandEdgeKindJson :: DemandEdgeKind -> String
renderDemandEdgeKindJson currentKind =
  case currentKind of
    InvokeImplementation ->
      jsonObject
        [ jsonField "kind" (jsonString "implementation-binding")
        ]
    InputDependency ->
      jsonObject
        [ jsonField "kind" (jsonString "input")
        ]
    ArgumentUse (FieldPath currentSegments) ->
      jsonObject
        [ jsonField "kind" (jsonString "argument-use")
        , jsonField
            "fieldPath"
            (jsonArray (map jsonString currentSegments))
        ]

renderAstPathJson :: AstPath -> String
renderAstPathJson (AstPath currentSegments) =
  jsonArray (map jsonString currentSegments)

renderRuntimeFailureJson :: RuntimeFailure -> String
renderRuntimeFailureJson currentFailure =
  jsonObject
    [ jsonField
        "phase"
        (jsonString (renderFailurePhase (runtimeFailurePhase currentFailure)))
    , jsonField
        "commit"
        ( jsonString
            (renderCommitState (runtimeFailureCommitState currentFailure))
        )
    , jsonField
        "message"
        (jsonString (runtimeFailureMessage currentFailure))
    ]

renderExecutionStatusJson :: ExecutionStatus -> String
renderExecutionStatusJson currentStatus =
  case currentStatus of
    ExecutionUnused -> statusObject "unused" []
    ExecutionPending -> statusObject "pending" []
    ExecutionRunning -> statusObject "running" []
    ExecutionSucceeded -> statusObject "succeeded" []
    ExecutionFailed currentFailure ->
      statusObject
        "failed"
        [jsonField "failure" (renderRuntimeFailureJson currentFailure)]
    ExecutionOutcomeUnknown currentFailure ->
      statusObject
        "outcome-unknown"
        [jsonField "failure" (renderRuntimeFailureJson currentFailure)]

renderReadStatusJson :: ReadStatus -> String
renderReadStatusJson currentStatus =
  case currentStatus of
    ReadUnused -> statusObject "unused" []
    ReadPending -> statusObject "pending" []
    ReadRunning -> statusObject "running" []
    ReadAvailable -> statusObject "available" []
    ReadObservationUnavailable currentFailure ->
      statusObject
        "observation-unavailable"
        [jsonField "failure" (renderRuntimeFailureJson currentFailure)]
    ReadFailed currentFailure ->
      statusObject
        "failed"
        [jsonField "failure" (renderRuntimeFailureJson currentFailure)]

renderImplementationStatusJson :: ImplementationStatus -> String
renderImplementationStatusJson currentStatus =
  case currentStatus of
    ImplementationUnused -> statusObject "unused" []
    ImplementationPending -> statusObject "pending" []
    ImplementationRunning -> statusObject "running" []
    ImplementationSucceeded -> statusObject "succeeded" []
    ImplementationFailed currentFailure ->
      statusObject
        "failed"
        [jsonField "failure" (renderRuntimeFailureJson currentFailure)]

renderValidityJson :: Validity -> String
renderValidityJson currentValidity =
  case currentValidity of
    Trusted ->
      statusObject "trusted" []
    Suspect ->
      statusObject "suspect" []
    Invalidated ->
      statusObject "invalidated" []
    TaintedBy currentId ->
      statusObject
        "tainted-by"
        [jsonField "handle" (jsonString (renderHandleId currentId))]

statusObject :: String -> [String] -> String
statusObject currentStatus currentFields =
  jsonObject
    (jsonField "status" (jsonString currentStatus) : currentFields)

renderFailureChannel :: DiagnosisFailureChannel -> String
renderFailureChannel currentChannel =
  case currentChannel of
    ExecutionFailureChannel -> "execution"
    ReadFailureChannel -> "read"
    ImplementationFailureChannel -> "implementation"

renderReasonKind :: DiagnosisReasonKind -> String
renderReasonKind currentKind =
  case currentKind of
    ExecutionFailedReason -> "execution-failed"
    ExecutionOutcomeUnknownReason -> "execution-outcome-unknown"
    ReadObservationUnavailableReason -> "read-observation-unavailable"
    ReadFailedReason -> "read-failed"
    ImplementationFailedReason -> "implementation-failed"

renderImpactKind :: DiagnosisImpactKind -> String
renderImpactKind currentKind =
  case currentKind of
    DiagnosisRootImpact -> "root"
    DiagnosisSuspectImpact -> "suspect"
    DiagnosisPollutedImpact -> "polluted"

renderTraceDirection :: DiagnosisTraceDirection -> String
renderTraceDirection currentDirection =
  case currentDirection of
    TowardPrerequisite -> "prerequisite"
    TowardDependent -> "dependent"

renderFailurePhase :: FailurePhase -> String
renderFailurePhase currentPhase =
  case currentPhase of
    DependencyPhase -> "dependency"
    ArgumentBindingPhase -> "argument-binding"
    HandlerPhase -> "handler"
    ObservationPhase -> "observation"
    ReadPhase -> "read"
    ImplementationPhase -> "implementation"
    RuntimeInvariantPhase -> "runtime-invariant"

renderCommitState :: CommitState -> String
renderCommitState currentCommit =
  case currentCommit of
    NoExternalCommit -> "none"
    ExternalCommitUnknown -> "unknown"
    ExternalCommitted -> "committed"

jsonMaybe :: (value -> String) -> Maybe value -> String
jsonMaybe _ Nothing =
  "null"
jsonMaybe renderValue (Just currentValue) =
  renderValue currentValue

jsonObject :: [String] -> String
jsonObject currentFields =
  "{" ++ joinWith "," currentFields ++ "}"

jsonField :: String -> String -> String
jsonField currentName currentValue =
  jsonString currentName ++ ":" ++ currentValue

jsonArray :: [String] -> String
jsonArray currentValues =
  "[" ++ joinWith "," currentValues ++ "]"

jsonString :: String -> String
jsonString currentValue =
  "\"" ++ concatMap jsonChar currentValue ++ "\""

jsonChar :: Char -> String
jsonChar currentChar =
  case currentChar of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _
      | ord currentChar < 32 ->
          "\\u" ++ padFour (showHex (ord currentChar) "")
      | otherwise ->
          [currentChar]

padFour :: String -> String
padFour currentText =
  replicate (max 0 (4 - length currentText)) '0' ++ currentText

joinWith :: String -> [String] -> String
joinWith _ [] =
  ""
joinWith _ [currentItem] =
  currentItem
joinWith currentSeparator (currentItem : remaining) =
  currentItem
    ++ currentSeparator
    ++ joinWith currentSeparator remaining

uniqueSorted :: Ord item => [item] -> [item]
uniqueSorted =
  Set.toAscList . Set.fromList
