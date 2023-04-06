## matchable-th

This package provides TemplateHaskell functions to
generate instances of `Matchable` and `Bimatchable` type classes,
which are from `matchable` package.

### Examples

``` haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Functor.Classes (Eq1(..))
import Data.Matchable
import Data.Matchable.TH ( deriveInstances )

newtype G a = G [(a, Int, a)]
  deriving (Show, Eq, Functor)

deriveInstances [d|
  deriving instance Eq1 G
  deriving instance Matchable G
  |]

```

``` haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Functor.Classes ( Eq1, Eq2 )
import Data.Bifunctor ( Bifunctor(..) )
import Data.Bimatchable ( Bimatchable )
import Data.Matchable ( Matchable )
import Data.Matchable.TH ( deriveInstances )

-- Test case for using [], tuple, and another Bimatchable instance
data BiG a b = BiG0 | BiG1 [a] [b] | BiG2 (Int, BiF a b)
  deriving (Show, Eq)

deriveInstances [d|
  deriving instance Bifunctor BiG
  deriving instance Eq a => Eq1 (BiG a)
  deriving instance Eq a => Matchable (BiG a)
  deriving instance Eq2 BiG
  deriving instance Bimatchable BiG
  |]
```
