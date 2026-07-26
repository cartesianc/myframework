module MyFramework.Control
  ( ControlNode (..)
  , ControlOccurrenceExpectation (..)
  , ControlPlan (..)
  , ControlTree (..)
  , ControlValidationError (..)
  , StatusPlan (..)
  , compileControlPlan
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.Ast
import MyFramework.CURDE.Core
  ( CURDECore
  , DemandGraph
  , DemandNodeId (..)
  , ImplementationId
  , implementationIdFor
  , curdeCoreAst
  , curdeCoreDemandGraph
  , curdeCoreEffectSystems
  , demandGraphOccurrences
  )
import MyFramework.CURDE.Expression
  ( ImplementationDecl )
import MyFramework.CURDE.Types
  ( EffectSystemDecl
  , HandleId
  , effectSystemDeclHandles
  , handleDeclId
  , renderHandleId
  )
import MyFramework.Recursion
  ( Algebra
  , cata
  )

-- | Static control metadata. Only 'controlPlanBoot' is the boot root.
-- Hanging trees remain addressable metadata for later listeners or runtime
-- installation; compiling the plan does not start them.
data ControlPlan = ControlPlan
  { controlPlanBoot :: ControlTree
  , controlPlanHanging :: [ControlTree]
  }
  deriving (Eq, Show)

data ControlTree = ControlTree
  { controlTreePath :: AstPath
  , controlTreeNode :: ControlNode
  }
  deriving (Eq, Show)

-- | A structural projection of every 'AstF' constructor. These nodes contain
-- no handler invocation, scheduling, waiting, retry, or other runtime action.
data ControlNode
  = ControlDemand DemandNodeId
  | ControlWithImplementation ImplementationId ControlTree
  | ControlSequence [ControlTree]
  | ControlParallel [ControlTree]
  | ControlFallback [ControlTree]
  | ControlRace [ControlTree]
  | ControlChoice ChoiceKey [(ChoiceKey, ControlTree)]
  | ControlWait StatusPlan ControlTree
  | ControlLoop ControlTree
  | ControlMiddleware MiddlewareRef ControlTree
  | ControlCallback HandleId ControlTree
  | ControlSuspense HandleId
  | ControlContext ContextRef ControlTree
  deriving (Eq, Show)

data StatusPlan
  = StatusHandle HandleId
  | StatusAllPlan [StatusPlan]
  | StatusAnyPlan [StatusPlan]
  deriving (Eq, Ord, Show)

data ControlOccurrenceExpectation
  = ExpectedHandleOccurrence HandleRef
  | ExpectedImplementationOccurrence ImplementationId
  deriving (Eq, Ord, Show)

data ControlValidationError
  = ControlOccurrenceMissing AstPath ControlOccurrenceExpectation
  | ControlOccurrenceAmbiguous
      AstPath
      ControlOccurrenceExpectation
      [DemandNodeId]
  | ControlHandleIdentityUnresolved
      AstPath
      HandleRef
      [DemandNodeId]
  | ControlImplementationIdentityUnresolved
      AstPath
      ImplementationId
      [DemandNodeId]
  | ControlStatusHandleUnresolved AstPath HandleRef
  | ControlCallbackHandleUnresolved AstPath HandleRef
  | ControlSuspenseHandleUnresolved AstPath HandleRef
  | ControlDuplicateChoiceKey AstPath ChoiceKey
  | ControlSelectedChoiceMissing AstPath ChoiceKey [ChoiceKey]
  | ControlPlanTreeUnavailable AstPath
  deriving (Eq, Ord, Show)

type HandleIndex = Map.Map String HandleId

type OccurrenceIndex = Map.Map AstPath [DemandNodeId]

data ControlEnvironment = ControlEnvironment
  { controlHandleIndex :: HandleIndex
  , controlOccurrenceIndex :: OccurrenceIndex
  }

type ControlFold =
  ControlEnvironment ->
  AstPath ->
  ControlBuild

data ControlBuild = ControlBuild
  { controlBuildTree :: Maybe ControlTree
  , controlBuildErrors :: [ControlValidationError]
  }

data StatusBuild = StatusBuild
  { statusBuildPlan :: Maybe StatusPlan
  , statusBuildErrors :: [ControlValidationError]
  }

compileControlPlan ::
  CURDECore ->
  Either [ControlValidationError] ControlPlan
compileControlPlan currentCore =
  case semanticErrors of
    [] ->
      case
          ( controlBuildTree bootBuild
          , traverse controlBuildTree hangingBuilds
          ) of
        (Just currentBoot, Just currentHanging) ->
          Right
            ControlPlan
              { controlPlanBoot = currentBoot
              , controlPlanHanging = currentHanging
              }
        _ ->
          Left (sortErrors unavailableErrors)
    _ ->
      Left semanticErrors
  where
    currentBlueprint =
      curdeCoreAst currentCore
    currentEnvironment =
      ControlEnvironment
        { controlHandleIndex =
            catalogHandleIndex
              (curdeCoreEffectSystems currentCore)
        , controlOccurrenceIndex =
            occurrenceIndex (curdeCoreDemandGraph currentCore)
        }
    bootPath =
      AstPath ["blueprint", "boot"]
    bootBuild =
      compileOne
        currentEnvironment
        bootPath
        (astBlueprintBoot currentBlueprint)
    hangingBuilds =
      [ compileOne
          currentEnvironment
          (hangingPath currentIndexValue)
          currentTree
      | (currentIndexValue, currentTree) <-
          indexedItems (astBlueprintHanging currentBlueprint)
      ]
    semanticErrors =
      sortErrors
        ( controlBuildErrors bootBuild
            ++ concatMap controlBuildErrors hangingBuilds
        )
    unavailableErrors =
      [ ControlPlanTreeUnavailable bootPath
      | controlBuildTree bootBuild == Nothing
      ]
        ++ [ ControlPlanTreeUnavailable
               (hangingPath currentIndexValue)
           | (currentIndexValue, currentBuild) <-
               indexedItems hangingBuilds
           , controlBuildTree currentBuild == Nothing
           ]
    hangingPath currentIndexValue =
      AstPath
        [ "blueprint"
        , "hanging"
        , "item:" ++ show currentIndexValue
        ]

compileOne ::
  ControlEnvironment ->
  AstPath ->
  Ast ->
  ControlBuild
compileOne currentEnvironment currentPath currentAst =
  cata controlAlgebra currentAst currentEnvironment currentPath

controlAlgebra :: Algebra AstF ControlFold
controlAlgebra currentLayer currentEnvironment currentPath =
  case currentLayer of
    Leaf currentTarget ->
      case currentTarget of
        HandleTarget currentHandle ->
          resolvedBuild
            currentPath
            ( resolveHandleOccurrence
                currentOccurrences
                currentPath
                currentHandle
            )
        ImplementationTarget currentImplementation ->
          resolvedBuild
            currentPath
            ( fmap
                ImplementationNode
                ( resolveImplementationOccurrence
                    currentOccurrences
                    currentPath
                    currentImplementation
                )
            )
    WithImplementation currentImplementation child ->
      buildWithImplementation
        currentPath
        ( resolveImplementationOccurrence
            currentOccurrences
            currentPath
            currentImplementation
        )
        ( child
            currentEnvironment
            (appendAstPath currentPath "body")
        )
    Chain children ->
      buildMany
        currentPath
        ControlSequence
        (compileIndexedChildren "step" children)
    Parallel children ->
      buildMany
        currentPath
        ControlParallel
        (compileIndexedChildren "branch" children)
    Fallback children ->
      buildMany
        currentPath
        ControlFallback
        (compileIndexedChildren "branch" children)
    Race children ->
      buildMany
        currentPath
        ControlRace
        (compileIndexedChildren "branch" children)
    Choice currentSelected branches ->
      buildChoice
        currentPath
        currentSelected
        [ ( currentKey
          , child
              currentEnvironment
              ( appendAstPath
                  currentPath
                  ( choiceBranchSegment
                      currentBranchIndex
                      currentKey
                  )
              )
          )
        | (currentBranchIndex, (currentKey, child)) <-
            indexedItems branches
        ]
    Wait currentStatus child ->
      buildWait
        currentPath
        (compileStatus currentHandles currentPath currentStatus)
        ( child
            currentEnvironment
            (appendAstPath currentPath "body")
        )
    Loop child ->
      buildWrapped
        currentPath
        ControlLoop
        ( child
            currentEnvironment
            (appendAstPath currentPath "body")
        )
    Middleware currentMiddleware child ->
      buildWrapped
        currentPath
        (ControlMiddleware currentMiddleware)
        ( child
            currentEnvironment
            (appendAstPath currentPath "body")
        )
    Callback currentTarget child ->
      buildResolvedHandleWrapped
        currentPath
        ( resolveCatalogHandle
            currentHandles
            currentPath
            ControlCallbackHandleUnresolved
            currentTarget
        )
        ControlCallback
        ( child
            currentEnvironment
            (appendAstPath currentPath "body")
        )
    Suspense currentTarget ->
      case
          resolveCatalogHandle
            currentHandles
            currentPath
            ControlSuspenseHandleUnresolved
            currentTarget of
        Left currentError ->
          failedBuild currentError
        Right currentHandle ->
          successfulBuild
            currentPath
            (ControlSuspense currentHandle)
    Context currentContext child ->
      buildWrapped
        currentPath
        (ControlContext currentContext)
        ( child
            currentEnvironment
            (appendAstPath currentPath "body")
        )
  where
    currentHandles =
      controlHandleIndex currentEnvironment
    currentOccurrences =
      controlOccurrenceIndex currentEnvironment
    compileIndexedChildren currentPrefix children =
      [ child
          currentEnvironment
          ( appendAstPath
              currentPath
              (currentPrefix ++ ":" ++ show currentChildIndex)
          )
      | (currentChildIndex, child) <- indexedItems children
      ]

choiceBranchSegment :: Int -> ChoiceKey -> String
choiceBranchSegment currentIndex currentKey =
  "branch:"
    ++ show currentIndex
    ++ ":"
    ++ choiceKeyText currentKey

compileStatus ::
  HandleIndex ->
  AstPath ->
  StatusExpr ->
  StatusBuild
compileStatus currentHandles currentPath currentStatus =
  case currentStatus of
    StatusOf currentReference ->
      case
          resolveCatalogHandle
            currentHandles
            currentPath
            ControlStatusHandleUnresolved
            currentReference of
        Left currentError ->
          StatusBuild
            { statusBuildPlan = Nothing
            , statusBuildErrors = [currentError]
            }
        Right currentHandle ->
          StatusBuild
            { statusBuildPlan =
                Just (StatusHandle currentHandle)
            , statusBuildErrors = []
            }
    StatusAll currentStatuses ->
      buildStatusMany
        StatusAllPlan
        (map (compileStatus currentHandles currentPath) currentStatuses)
    StatusAny currentStatuses ->
      buildStatusMany
        StatusAnyPlan
        (map (compileStatus currentHandles currentPath) currentStatuses)

buildStatusMany ::
  ([StatusPlan] -> StatusPlan) ->
  [StatusBuild] ->
  StatusBuild
buildStatusMany currentConstructor currentStatuses =
  StatusBuild
    { statusBuildPlan =
        fmap currentConstructor
          (traverse statusBuildPlan currentStatuses)
    , statusBuildErrors =
        concatMap statusBuildErrors currentStatuses
    }

resolvedBuild ::
  AstPath ->
  Either ControlValidationError DemandNodeId ->
  ControlBuild
resolvedBuild currentPath currentResult =
  case currentResult of
    Left currentError ->
      failedBuild currentError
    Right currentNode ->
      successfulBuild currentPath (ControlDemand currentNode)

successfulBuild :: AstPath -> ControlNode -> ControlBuild
successfulBuild currentPath currentNode =
  ControlBuild
    { controlBuildTree =
        Just
          ControlTree
            { controlTreePath = currentPath
            , controlTreeNode = currentNode
            }
    , controlBuildErrors = []
    }

failedBuild :: ControlValidationError -> ControlBuild
failedBuild currentError =
  ControlBuild
    { controlBuildTree = Nothing
    , controlBuildErrors = [currentError]
    }

buildWrapped ::
  AstPath ->
  (ControlTree -> ControlNode) ->
  ControlBuild ->
  ControlBuild
buildWrapped currentPath currentConstructor currentChild =
  ControlBuild
    { controlBuildTree =
        fmap
          ( \currentTree ->
              ControlTree
                { controlTreePath = currentPath
                , controlTreeNode = currentConstructor currentTree
                }
          )
          (controlBuildTree currentChild)
    , controlBuildErrors =
        controlBuildErrors currentChild
    }

buildResolvedHandleWrapped ::
  AstPath ->
  Either ControlValidationError HandleId ->
  (HandleId -> ControlTree -> ControlNode) ->
  ControlBuild ->
  ControlBuild
buildResolvedHandleWrapped
  currentPath
  currentResolution
  currentConstructor
  currentChild =
    case currentResolution of
      Left currentError ->
        ControlBuild
          { controlBuildTree = Nothing
          , controlBuildErrors =
              currentError : controlBuildErrors currentChild
          }
      Right currentHandle ->
        buildWrapped
          currentPath
          (currentConstructor currentHandle)
          currentChild

buildWait ::
  AstPath ->
  StatusBuild ->
  ControlBuild ->
  ControlBuild
buildWait currentPath currentStatus currentChild =
  ControlBuild
    { controlBuildTree =
        case
            ( statusBuildPlan currentStatus
            , controlBuildTree currentChild
            ) of
          (Just currentPlan, Just currentTree) ->
            Just
              ControlTree
                { controlTreePath = currentPath
                , controlTreeNode =
                    ControlWait currentPlan currentTree
                }
          _ ->
            Nothing
    , controlBuildErrors =
        statusBuildErrors currentStatus
          ++ controlBuildErrors currentChild
    }

buildMany ::
  AstPath ->
  ([ControlTree] -> ControlNode) ->
  [ControlBuild] ->
  ControlBuild
buildMany currentPath currentConstructor currentChildren =
  ControlBuild
    { controlBuildTree =
        fmap
          ( \currentTrees ->
              ControlTree
                { controlTreePath = currentPath
                , controlTreeNode = currentConstructor currentTrees
                }
          )
          (traverse controlBuildTree currentChildren)
    , controlBuildErrors =
        concatMap controlBuildErrors currentChildren
    }

buildChoice ::
  AstPath ->
  ChoiceKey ->
  [(ChoiceKey, ControlBuild)] ->
  ControlBuild
buildChoice currentPath currentSelected currentBranches =
  ControlBuild
    { controlBuildTree =
        case currentChoiceErrors of
          [] ->
            buildChoiceTree
          _ ->
            Nothing
    , controlBuildErrors =
        currentChoiceErrors
          ++ concatMap (controlBuildErrors . snd) currentBranches
    }
  where
    currentKeys =
      map fst currentBranches
    currentChoiceErrors =
      [ ControlDuplicateChoiceKey currentPath currentKey
      | currentKey <- duplicateItems currentKeys
      ]
        ++ [ ControlSelectedChoiceMissing
               currentPath
               currentSelected
               currentKeys
           | currentSelected `notElem` currentKeys
           ]
    buildChoiceTree =
      fmap
        ( \currentTrees ->
            ControlTree
              { controlTreePath = currentPath
              , controlTreeNode =
                  ControlChoice
                    currentSelected
                    (zip currentKeys currentTrees)
              }
        )
        (traverse (controlBuildTree . snd) currentBranches)

buildWithImplementation ::
  AstPath ->
  Either ControlValidationError ImplementationId ->
  ControlBuild ->
  ControlBuild
buildWithImplementation currentPath currentResolution currentChild =
  case currentResolution of
    Left currentError ->
      ControlBuild
        { controlBuildTree = Nothing
        , controlBuildErrors =
            currentError : controlBuildErrors currentChild
        }
    Right currentImplementation ->
      buildWrapped
        currentPath
        (ControlWithImplementation currentImplementation)
        currentChild

resolveCatalogHandle ::
  HandleIndex ->
  AstPath ->
  (AstPath -> HandleRef -> ControlValidationError) ->
  HandleRef ->
  Either ControlValidationError HandleId
resolveCatalogHandle
  currentHandles
  currentPath
  unresolvedError
  currentReference =
    case
        Map.lookup
          (handleRefText currentReference)
          currentHandles of
      Just currentHandle ->
        Right currentHandle
      Nothing ->
        Left (unresolvedError currentPath currentReference)

resolveHandleOccurrence ::
  OccurrenceIndex ->
  AstPath ->
  HandleRef ->
  Either ControlValidationError DemandNodeId
resolveHandleOccurrence currentIndex currentPath currentHandle =
  resolveOccurrence
    currentIndex
    currentPath
    (ExpectedHandleOccurrence currentHandle)
    matchesExpected
    ( ControlHandleIdentityUnresolved
        currentPath
        currentHandle
    )
  where
    matchesExpected currentNode =
      case currentNode of
        HandleNode currentId ->
          renderHandleId currentId == handleRefText currentHandle
        ImplementationNode _ ->
          False

resolveImplementationOccurrence ::
  OccurrenceIndex ->
  AstPath ->
  ImplementationDecl ->
  Either ControlValidationError ImplementationId
resolveImplementationOccurrence currentIndex currentPath currentImplementation =
  case
      resolveOccurrence
        currentIndex
        currentPath
        (ExpectedImplementationOccurrence expectedId)
        matchesExpected
        ( ControlImplementationIdentityUnresolved
            currentPath
            expectedId
        ) of
    Left currentError ->
      Left currentError
    Right (ImplementationNode currentId) ->
      Right currentId
    Right currentNode ->
      Left
        ( ControlImplementationIdentityUnresolved
            currentPath
            expectedId
            [currentNode]
        )
  where
    expectedId =
      implementationIdFor currentImplementation
    matchesExpected currentNode =
      case currentNode of
        HandleNode _ ->
          False
        ImplementationNode currentId ->
          currentId == expectedId

resolveOccurrence ::
  OccurrenceIndex ->
  AstPath ->
  ControlOccurrenceExpectation ->
  (DemandNodeId -> Bool) ->
  ([DemandNodeId] -> ControlValidationError) ->
  Either ControlValidationError DemandNodeId
resolveOccurrence
  currentIndex
  currentPath
  currentExpectation
  matchesExpected
  unresolvedError =
    case currentMatches of
      [] ->
        case currentCandidates of
          [] ->
            Left
              ( ControlOccurrenceMissing
                  currentPath
                  currentExpectation
              )
          _ ->
            Left (unresolvedError currentCandidates)
      [currentMatch] ->
        Right currentMatch
      _ ->
        Left
          ( ControlOccurrenceAmbiguous
              currentPath
              currentExpectation
              currentMatches
          )
  where
    currentCandidates =
      uniqueSorted
        (Map.findWithDefault [] currentPath currentIndex)
    currentMatches =
      filter matchesExpected currentCandidates

catalogHandleIndex :: [EffectSystemDecl] -> HandleIndex
catalogHandleIndex currentSystems =
  Map.fromListWith
    min
    [ (renderHandleId currentId, currentId)
    | currentSystem <- currentSystems
    , currentHandle <- effectSystemDeclHandles currentSystem
    , let currentId = handleDeclId currentHandle
    ]

-- | Invert only the occurrence map. Control compilation intentionally never
-- reads demand edges, roots, or closure, so AST control cannot be duplicated
-- as a second demand-graph control language.
occurrenceIndex :: DemandGraph -> OccurrenceIndex
occurrenceIndex currentGraph =
  fmap uniqueSorted
    ( Map.fromListWith
        (++)
        [ (currentPath, [currentNode])
        | (currentNode, currentPaths) <-
            Map.toAscList (demandGraphOccurrences currentGraph)
        , currentPath <- currentPaths
        ]
    )

duplicateItems :: Ord item => [item] -> [item]
duplicateItems currentItems =
  Map.keys
    ( Map.filter
        (> (1 :: Int))
        ( Map.fromListWith
            (+)
            [(currentItem, 1) | currentItem <- currentItems]
        )
    )

sortErrors :: [ControlValidationError] -> [ControlValidationError]
sortErrors =
  Set.toAscList . Set.fromList

uniqueSorted :: Ord item => [item] -> [item]
uniqueSorted =
  Set.toAscList . Set.fromList

indexedItems :: [item] -> [(Int, item)]
indexedItems =
  zip [0 ..]
