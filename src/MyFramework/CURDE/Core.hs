module MyFramework.CURDE.Core
  ( AstPath (..)
  , appendAstPath
  , renderAstPath
  , FieldPath (..)
  , renderFieldPath
  , ImplementationId (..)
  , implementationIdFor
  , renderImplementationId
  , DemandNodeId (..)
  , DemandNode (..)
  , demandNodeId
  , DemandEdgeKind (..)
  , DemandEdge (..)
  , RootDemandKind (..)
  , RootDemand (..)
  , DemandGraph (..)
  , emptyDemandGraph
  , normalizeDemandGraph
  , demandClosure
  , CURDECore (..)
  ) where

import Data.List
  ( intercalate )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyFramework.Ast
  ( AstBlueprint
  , AstPath (..)
  , appendAstPath
  , renderAstPath
  )
import MyFramework.CURDE.Expression
  ( ImplementationDecl
  , implementationDeclTargetId
  )
import MyFramework.CURDE.Types
  ( EffectSystemDecl
  , HandleDecl
  , HandleId
  , handleDeclId
  , renderHandleId
  )


newtype FieldPath = FieldPath
  { fieldPathSegments :: [String]
  }
  deriving (Eq, Ord, Show)

renderFieldPath :: FieldPath -> String
renderFieldPath (FieldPath currentPath) =
  intercalate "." currentPath

-- | A lowered binding has one stable identity per CUDE handle. Explicit
-- bindings are derived from AST occurrences; Unit bindings may be normalized.
newtype ImplementationId = ImplementationId
  { implementationIdTarget :: HandleId
  }
  deriving (Eq, Ord, Show)

implementationIdFor :: ImplementationDecl -> ImplementationId
implementationIdFor =
  ImplementationId . implementationDeclTargetId

renderImplementationId :: ImplementationId -> String
renderImplementationId =
  ("implementation:" ++) . renderHandleId . implementationIdTarget

data DemandNodeId
  = HandleNode HandleId
  | ImplementationNode ImplementationId
  deriving (Eq, Ord, Show)

data DemandNode
  = DemandHandleNode HandleDecl
  | DemandImplementationNode ImplementationId ImplementationDecl
  deriving (Eq, Ord, Show)

demandNodeId :: DemandNode -> DemandNodeId
demandNodeId currentNode =
  case currentNode of
    DemandHandleNode currentHandle ->
      HandleNode (handleDeclId currentHandle)
    DemandImplementationNode currentId _ ->
      ImplementationNode currentId

-- | Binding, explicit input, and R-value-use dependencies only. All sequence,
-- choice, wait, parallel, race, and fallback behavior remains exclusively in
-- the AST control interpreter.
data DemandEdgeKind
  = InvokeImplementation
  | InputDependency
  | ArgumentUse FieldPath
  deriving (Eq, Ord, Show)

data DemandEdge = DemandEdge
  { demandEdgeDependent :: DemandNodeId
  , demandEdgePrerequisite :: DemandNodeId
  , demandEdgeKind :: DemandEdgeKind
  , demandEdgeSourcePath :: Maybe AstPath
  }
  deriving (Eq, Ord, Show)

data RootDemandKind
  = BootRoot
  | HangingRoot
  deriving (Eq, Ord, Show)

data RootDemand = RootDemand
  { rootDemandKind :: RootDemandKind
  , rootDemandPath :: AstPath
  , rootDemandNode :: DemandNodeId
  }
  deriving (Eq, Ord, Show)

data DemandGraph = DemandGraph
  { demandGraphNodes :: Map.Map DemandNodeId DemandNode
  , demandGraphEdges :: [DemandEdge]
  , demandGraphRoots :: [RootDemand]
  , demandGraphOccurrences :: Map.Map DemandNodeId [AstPath]
  , demandGraphClosure :: [DemandNodeId]
  }
  deriving (Eq, Show)

emptyDemandGraph :: DemandGraph
emptyDemandGraph =
  DemandGraph
    { demandGraphNodes = Map.empty
    , demandGraphEdges = []
    , demandGraphRoots = []
    , demandGraphOccurrences = Map.empty
    , demandGraphClosure = []
    }

normalizeDemandGraph :: DemandGraph -> DemandGraph
normalizeDemandGraph currentGraph =
  normalized
    { demandGraphClosure = demandClosure normalized
    }
  where
    normalized =
      currentGraph
        { demandGraphEdges = uniqueSorted (demandGraphEdges currentGraph)
        , demandGraphRoots = uniqueSorted (demandGraphRoots currentGraph)
        , demandGraphOccurrences =
            fmap uniqueSorted (demandGraphOccurrences currentGraph)
        }

demandClosure :: DemandGraph -> [DemandNodeId]
demandClosure currentGraph =
  Set.toAscList (visit Set.empty initialNodes)
  where
    initialNodes =
      map rootDemandNode (demandGraphRoots currentGraph)
    prerequisitesByNode =
      Map.fromListWith
        (++)
        [ (demandEdgeDependent currentEdge, [demandEdgePrerequisite currentEdge])
        | currentEdge <- demandGraphEdges currentGraph
        ]
    visit visited [] =
      visited
    visit visited (currentNode : remaining)
      | currentNode `Set.member` visited =
          visit visited remaining
      | otherwise =
          visit
            (Set.insert currentNode visited)
            (Map.findWithDefault [] currentNode prerequisitesByNode ++ remaining)

data CURDECore = CURDECore
  { curdeCoreEffectSystems :: [EffectSystemDecl]
  -- | Deduplicated lowered IR derived from AST declarations plus demanded
  -- implicit Unit implementations. It is not a frontend registry.
  , curdeCoreImplementations :: [ImplementationDecl]
  , curdeCoreAst :: AstBlueprint
  , curdeCoreDemandGraph :: DemandGraph
  }

uniqueSorted :: Ord item => [item] -> [item]
uniqueSorted =
  Set.toAscList . Set.fromList
