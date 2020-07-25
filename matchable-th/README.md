## matchable-th

This package provides TemplateHaskell functions to
generate instances of `Matchable` and `Bimatchable` type classes,
which are from `matchable` package.

### Example

``` haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Functor.Classes (Eq1(..))
import Data.Matchable
import Data.Matchable.TH

newtype G a = G [(a, Int, a)]
  deriving (Show, Eq, Functor)

$(deriveMatchable ''G)

-- @deriveMatchable@ generates a @Matchable@ instance only,
-- so you also have to declare @Functor G@ and @Eq1 G@.
-- There is a handy @DeriveFunctor@ extension.
-- Also, you can use @liftEqDefault@ to easily implement @liftEq@.
instance Eq1 G where
  liftEq = liftEqDefault
```

``` haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Functor.Classes (Eq2(..))
import Data.Bimatchable
import Data.Matchable.TH

-- Most simple case
data BiF a b = BiF0 | BiF1 a b

instance Eq2 BiF where
  liftEq2 = liftEq2Default

instance Bifunctor BiF where
  bimap = bimapRecovered

$(deriveBimatchable ''BiF)

-- Test case for using [], tuple, and another Bimatchable instance
data BiG a b = BiG0 | BiG1 [a] [b] | BiG2 (Int, BiF a b)

instance Eq2 BiG where
  liftEq2 = liftEq2Default

instance Bifunctor BiG where
  bimap = bimapRecovered

$(deriveBimatchable ''BiG)
```
