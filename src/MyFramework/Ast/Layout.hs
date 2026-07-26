module MyFramework.Ast.Layout
  ( AstBlueprintLayout (..)
  , AstLayoutEdge (..)
  , AstLayoutModel (..)
  , AstLayoutNode (..)
  , AstNodeKind (..)
  , AstPath (..)
  , AstSummary (..)
  , astLayoutNodeAtPath
  , astPaths
  , astSummaryKindCount
  , layoutAst
  , layoutAstAt
  , layoutAstBlueprint
  , summarizeAst
  , summarizeAstBlueprint
  ) where

import Data.List
  ( find )
import MyFramework.Ast
import MyFramework.CURDE.Core
  ( implementationIdFor
  , renderImplementationId
  )
import MyFramework.Recursion
  ( Algebra
  , cata
  )

data AstNodeKind
  = AstLeaf
  | AstWithImplementation
  | AstChain
  | AstParallel
  | AstFallback
  | AstRace
  | AstChoice
  | AstWait
  | AstLoop
  | AstMiddleware
  | AstCallback
  | AstSuspense
  | AstContext
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data AstLayoutNode = AstLayoutNode
  { astLayoutNodePath :: AstPath
  , astLayoutNodeKind :: AstNodeKind
  , astLayoutNodeLabel :: String
  , astLayoutNodeMetadata :: [(String, String)]
  }
  deriving (Eq, Ord, Read, Show)

data AstLayoutEdge = AstLayoutEdge
  { astLayoutEdgeFrom :: AstPath
  , astLayoutEdgeTo :: AstPath
  }
  deriving (Eq, Ord, Read, Show)

-- | A read-only projection. No layout value can mutate the source AST or
-- influence runtime scheduling.
data AstLayoutModel = AstLayoutModel
  { astLayoutRootPath :: AstPath
  , astLayoutNodes :: [AstLayoutNode]
  , astLayoutEdges :: [AstLayoutEdge]
  }
  deriving (Eq, Ord, Read, Show)

data AstBlueprintLayout = AstBlueprintLayout
  { astBlueprintBootLayout :: AstLayoutModel
  , astBlueprintHangingLayouts :: [AstLayoutModel]
  }
  deriving (Eq, Ord, Read, Show)

data AstSummary = AstSummary
  { astSummaryNodeCount :: Int
  , astSummaryLeafCount :: Int
  , astSummaryMaximumDepth :: Int
  , astSummaryKindCounts :: [(AstNodeKind, Int)]
  }
  deriving (Eq, Ord, Read, Show)

data Projection = Projection
  { projectionLayout :: AstPath -> AstLayoutModel
  , projectionSummary :: AstSummary
  }

layoutAst :: Ast -> AstLayoutModel
layoutAst =
  layoutAstAt (AstPath ["ast", "root"])

layoutAstAt :: AstPath -> Ast -> AstLayoutModel
layoutAstAt currentRoot currentAst =
  projectionLayout (cata layoutAlgebra currentAst) currentRoot

layoutAstBlueprint :: AstBlueprint -> AstBlueprintLayout
layoutAstBlueprint currentBlueprint =
  AstBlueprintLayout
    { astBlueprintBootLayout =
        layoutAstAt
          (AstPath ["blueprint", "boot"])
          (astBlueprintBoot currentBlueprint)
    , astBlueprintHangingLayouts =
        [ layoutAstAt
            ( AstPath
                [ "blueprint"
                , "hanging"
                , "item:" ++ show currentIndex
                ]
            )
            currentAst
        | (currentIndex, currentAst) <-
            indexedItems (astBlueprintHanging currentBlueprint)
        ]
    }

astPaths :: Ast -> [AstPath]
astPaths =
  map astLayoutNodePath . astLayoutNodes . layoutAst

astLayoutNodeAtPath :: AstPath -> AstLayoutModel -> Maybe AstLayoutNode
astLayoutNodeAtPath currentPath =
  find ((== currentPath) . astLayoutNodePath) . astLayoutNodes

summarizeAst :: Ast -> AstSummary
summarizeAst =
  projectionSummary . cata layoutAlgebra

summarizeAstBlueprint :: AstBlueprint -> AstSummary
summarizeAstBlueprint currentBlueprint =
  combineSummaries
    ( summarizeAst (astBlueprintBoot currentBlueprint)
        : map summarizeAst (astBlueprintHanging currentBlueprint)
    )

astSummaryKindCount :: AstNodeKind -> AstSummary -> Int
astSummaryKindCount currentKind currentSummary =
  case lookup currentKind (astSummaryKindCounts currentSummary) of
    Just currentCount ->
      currentCount
    Nothing ->
      0

layoutAlgebra :: Algebra AstF Projection
layoutAlgebra currentLayer =
  case currentLayer of
    Leaf currentTarget ->
      nodeProjection
        AstLeaf
        (targetLabel currentTarget)
        (targetMetadata currentTarget)
        []
    WithImplementation currentImplementation child ->
      nodeProjection
        AstWithImplementation
        (renderImplementationId (implementationIdFor currentImplementation))
        [("implementation", renderImplementationId (implementationIdFor currentImplementation))]
        [("body", child)]
    Chain children ->
      nodeProjection
        AstChain
        "chain"
        [("arity", show (length children))]
        (indexedChildren "step" children)
    Parallel children ->
      nodeProjection
        AstParallel
        "parallel"
        [("arity", show (length children))]
        (indexedChildren "branch" children)
    Fallback children ->
      nodeProjection
        AstFallback
        "fallback"
        [("arity", show (length children))]
        (indexedChildren "branch" children)
    Race children ->
      nodeProjection
        AstRace
        "race"
        [("arity", show (length children))]
        (indexedChildren "branch" children)
    Choice currentSelection branches ->
      nodeProjection
        AstChoice
        (choiceKeyText currentSelection)
        [ ("selected", choiceKeyText currentSelection)
        , ("arity", show (length branches))
        ]
        [ ( "branch:"
              ++ show currentIndex
              ++ ":"
              ++ choiceKeyText currentKey
          , child
          )
        | (currentIndex, (currentKey, child)) <- indexedItems branches
        ]
    Wait currentStatus child ->
      nodeProjection
        AstWait
        "wait"
        [("status", show currentStatus)]
        [("body", child)]
    Loop child ->
      nodeProjection
        AstLoop
        "loop"
        []
        [("body", child)]
    Middleware currentMiddleware child ->
      nodeProjection
        AstMiddleware
        (middlewareRefText currentMiddleware)
        [("middleware", middlewareRefText currentMiddleware)]
        [("body", child)]
    Callback currentHandle child ->
      nodeProjection
        AstCallback
        (handleRefText currentHandle)
        [("callback", handleRefText currentHandle)]
        [("body", child)]
    Suspense currentTarget ->
      nodeProjection
        AstSuspense
        (handleRefText currentTarget)
        [("target", handleRefText currentTarget)]
        []
    Context currentContext child ->
      nodeProjection
        AstContext
        (contextRefText currentContext)
        [("context", contextRefText currentContext)]
        [("body", child)]

nodeProjection ::
  AstNodeKind ->
  String ->
  [(String, String)] ->
  [(String, Projection)] ->
  Projection
nodeProjection currentKind currentLabel currentMetadata children =
  Projection
    { projectionLayout = \currentPath ->
        let
          childLayouts =
            [ projectionLayout child (appendAstPath currentPath currentSegment)
            | (currentSegment, child) <- children
            ]
          currentNode =
            AstLayoutNode
              { astLayoutNodePath = currentPath
              , astLayoutNodeKind = currentKind
              , astLayoutNodeLabel = currentLabel
              , astLayoutNodeMetadata = currentMetadata
              }
          currentEdges =
            [ AstLayoutEdge
                { astLayoutEdgeFrom = currentPath
                , astLayoutEdgeTo = astLayoutRootPath childLayout
                }
            | childLayout <- childLayouts
            ]
        in
          AstLayoutModel
            { astLayoutRootPath = currentPath
            , astLayoutNodes =
                currentNode : concatMap astLayoutNodes childLayouts
            , astLayoutEdges =
                currentEdges ++ concatMap astLayoutEdges childLayouts
            }
    , projectionSummary =
        summaryFor
          currentKind
          (map (projectionSummary . snd) children)
    }

summaryFor :: AstNodeKind -> [AstSummary] -> AstSummary
summaryFor currentKind childSummaries =
  AstSummary
    { astSummaryNodeCount =
        1 + sum (map astSummaryNodeCount childSummaries)
    , astSummaryLeafCount =
        (if currentKind == AstLeaf then 1 else 0)
          + sum (map astSummaryLeafCount childSummaries)
    , astSummaryMaximumDepth =
        1 + maximumOrZero (map astSummaryMaximumDepth childSummaries)
    , astSummaryKindCounts =
        [ ( currentCandidate
          , (if currentCandidate == currentKind then 1 else 0)
              + sum
                [ astSummaryKindCount currentCandidate childSummary
                | childSummary <- childSummaries
                ]
          )
        | currentCandidate <- allNodeKinds
        ]
    }

combineSummaries :: [AstSummary] -> AstSummary
combineSummaries currentSummaries =
  AstSummary
    { astSummaryNodeCount =
        sum (map astSummaryNodeCount currentSummaries)
    , astSummaryLeafCount =
        sum (map astSummaryLeafCount currentSummaries)
    , astSummaryMaximumDepth =
        maximumOrZero (map astSummaryMaximumDepth currentSummaries)
    , astSummaryKindCounts =
        [ ( currentKind
          , sum
              [ astSummaryKindCount currentKind currentSummary
              | currentSummary <- currentSummaries
              ]
          )
        | currentKind <- allNodeKinds
        ]
    }

targetLabel :: AstTarget -> String
targetLabel currentTarget =
  case currentTarget of
    HandleTarget currentHandle ->
      handleRefText currentHandle
    ImplementationTarget currentImplementation ->
      renderImplementationId (implementationIdFor currentImplementation)

targetMetadata :: AstTarget -> [(String, String)]
targetMetadata currentTarget =
  case currentTarget of
    HandleTarget currentHandle ->
      [ ("target-kind", "handle")
      , ("target", handleRefText currentHandle)
      ]
    ImplementationTarget currentImplementation ->
      [ ("target-kind", "implementation")
      , ("target", renderImplementationId (implementationIdFor currentImplementation))
      ]

indexedChildren :: String -> [Projection] -> [(String, Projection)]
indexedChildren currentPrefix children =
  [ (currentPrefix ++ ":" ++ show currentIndex, child)
  | (currentIndex, child) <- indexedItems children
  ]

indexedItems :: [item] -> [(Int, item)]
indexedItems =
  zip [0 ..]

allNodeKinds :: [AstNodeKind]
allNodeKinds =
  [minBound .. maxBound]

maximumOrZero :: [Int] -> Int
maximumOrZero [] =
  0
maximumOrZero currentValues =
  maximum currentValues
