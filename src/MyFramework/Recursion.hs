module MyFramework.Recursion
  ( Fix (..)
  , Algebra
  , cata
  ) where

-- | The least fixed point used by the framework's internal recursive models.
newtype Fix f = Fix
  { unFix :: f (Fix f)
  }

-- | One pure fold step over a recursive layer.
type Algebra f result = f result -> result

-- | Collapse a fixed point with a pure algebra.
cata :: Functor f => Algebra f result -> Fix f -> result
cata algebra =
  algebra . fmap (cata algebra) . unFix
