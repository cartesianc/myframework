{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module MyFramework.Ast
  ( Ast
  , AstBlueprint (..)
  , AstBlueprintSeed (..)
  , AstF (..)
  , AstPath (..)
  , AstSeed (..)
  , AstTarget (..)
  , ChoiceKey (..)
  , ContextRef (..)
  , HandleRef (..)
  , MiddlewareRef (..)
  , StatusExpr (..)
  , appendAstPath
  , decodeAstBlueprintSeed
  , decodeAstSeed
  , encodeAstBlueprintSeed
  , encodeAstSeed
  , lowerAstBlueprintSeed
  , lowerAstSeed
  , renderAstPath
  ) where

import Data.List
  ( intercalate )
import MyFramework.CURDE.Expression
  ( ImplementationDecl )
import MyFramework.Recursion
  ( Fix (..) )
import Text.Read
  ( readMaybe )

newtype AstPath = AstPath
  { astPathSegments :: [String]
  }
  deriving (Eq, Ord, Read, Show)

appendAstPath :: AstPath -> String -> AstPath
appendAstPath (AstPath currentSegments) currentSegment =
  AstPath (currentSegments ++ [currentSegment])

renderAstPath :: AstPath -> String
renderAstPath =
  intercalate "/" . astPathSegments

-- | Stable handle references keep control independent from executable
-- handlers. Implementations are serializable declarations, not registry keys.
newtype HandleRef = HandleRef
  { handleRefText :: String
  }
  deriving (Eq, Ord, Read, Show)


newtype MiddlewareRef = MiddlewareRef
  { middlewareRefText :: String
  }
  deriving (Eq, Ord, Read, Show)

newtype ContextRef = ContextRef
  { contextRefText :: String
  }
  deriving (Eq, Ord, Read, Show)

newtype ChoiceKey = ChoiceKey
  { choiceKeyText :: String
  }
  deriving (Eq, Ord, Read, Show)

data AstTarget
  = HandleTarget HandleRef
  | ImplementationTarget ImplementationDecl
  deriving (Eq, Ord, Read, Show)

-- | A declarative status predicate. It names observable status only; it does
-- not contain polling, retry, scheduling, or any other operation.
data StatusExpr
  = StatusOf HandleRef
  | StatusAll [StatusExpr]
  | StatusAny [StatusExpr]
  deriving (Eq, Ord, Read, Show)

-- | The complete control vocabulary. Payloads are stable references or
-- serializable values, never functions or effectful actions.
data AstF next
  = Leaf AstTarget
  | WithImplementation ImplementationDecl next
  | Chain [next]
  | Parallel [next]
  | Fallback [next]
  | Race [next]
  | Choice ChoiceKey [(ChoiceKey, next)]
  | Wait StatusExpr next
  | Loop next
  | Middleware MiddlewareRef next
  | Callback HandleRef next
  | Suspense HandleRef
  | Context ContextRef next
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type Ast = Fix AstF

data AstBlueprint = AstBlueprint
  { astBlueprintBoot :: Ast
  , astBlueprintHanging :: [Ast]
  }

-- | Serializable authoring form. It deliberately mirrors the control
-- vocabulary without exposing 'Fix' or accepting executable values.
data AstSeed
  = SeedLeaf AstTarget
  | SeedWithImplementation ImplementationDecl AstSeed
  | SeedChain [AstSeed]
  | SeedParallel [AstSeed]
  | SeedFallback [AstSeed]
  | SeedRace [AstSeed]
  | SeedChoice ChoiceKey [(ChoiceKey, AstSeed)]
  | SeedWait StatusExpr AstSeed
  | SeedLoop AstSeed
  | SeedMiddleware MiddlewareRef AstSeed
  | SeedCallback HandleRef AstSeed
  | SeedSuspense HandleRef
  | SeedContext ContextRef AstSeed
  deriving (Eq, Ord, Read, Show)

data AstBlueprintSeed = AstBlueprintSeed
  { astBlueprintSeedBoot :: AstSeed
  , astBlueprintSeedHanging :: [AstSeed]
  }
  deriving (Eq, Ord, Read, Show)

-- | A zero-dependency textual codec for configuration round trips. JSON-RPC
-- or another wire codec may encode the same seed ADT at a higher boundary.
encodeAstSeed :: AstSeed -> String
encodeAstSeed =
  show

decodeAstSeed :: String -> Either String AstSeed
decodeAstSeed input =
  case readMaybe input of
    Just currentSeed ->
      Right currentSeed
    Nothing ->
      Left "invalid AstSeed"

encodeAstBlueprintSeed :: AstBlueprintSeed -> String
encodeAstBlueprintSeed =
  show

decodeAstBlueprintSeed :: String -> Either String AstBlueprintSeed
decodeAstBlueprintSeed input =
  case readMaybe input of
    Just currentSeed ->
      Right currentSeed
    Nothing ->
      Left "invalid AstBlueprintSeed"

-- | Explicit, total lowering from serializable configuration into the
-- framework's recursive control representation.
lowerAstSeed :: AstSeed -> Ast
lowerAstSeed currentSeed =
  case currentSeed of
    SeedLeaf currentTarget ->
      Fix (Leaf currentTarget)
    SeedWithImplementation currentImplementation child ->
      Fix
        ( WithImplementation
            currentImplementation
            (lowerAstSeed child)
        )
    SeedChain children ->
      Fix (Chain (map lowerAstSeed children))
    SeedParallel children ->
      Fix (Parallel (map lowerAstSeed children))
    SeedFallback children ->
      Fix (Fallback (map lowerAstSeed children))
    SeedRace children ->
      Fix (Race (map lowerAstSeed children))
    SeedChoice currentSelection branches ->
      Fix
        ( Choice
            currentSelection
            [ (currentKey, lowerAstSeed child)
            | (currentKey, child) <- branches
            ]
        )
    SeedWait currentStatus child ->
      Fix (Wait currentStatus (lowerAstSeed child))
    SeedLoop child ->
      Fix (Loop (lowerAstSeed child))
    SeedMiddleware currentMiddleware child ->
      Fix (Middleware currentMiddleware (lowerAstSeed child))
    SeedCallback currentCallback child ->
      Fix (Callback currentCallback (lowerAstSeed child))
    SeedSuspense currentTarget ->
      Fix (Suspense currentTarget)
    SeedContext currentContext child ->
      Fix (Context currentContext (lowerAstSeed child))

lowerAstBlueprintSeed :: AstBlueprintSeed -> AstBlueprint
lowerAstBlueprintSeed currentSeed =
  AstBlueprint
    { astBlueprintBoot =
        lowerAstSeed (astBlueprintSeedBoot currentSeed)
    , astBlueprintHanging =
        map lowerAstSeed (astBlueprintSeedHanging currentSeed)
    }
