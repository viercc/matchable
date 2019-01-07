{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Main(main) where

import           Data.Functor.Classes
import           Data.Bifunctor
import           Data.Bimatchable
import           Data.Matchable
import           Data.Matchable.TH

main :: IO ()
main = putStrLn "compiles"

data F a = F0 | F1 a | F2 a a
  deriving (Show, Eq, Functor)

instance Eq1 F where
  liftEq = liftEqDefault

$(deriveMatchable ''F)

newtype G a = G [(a, Int, a)]
  deriving (Show, Eq, Functor)

instance Eq1 G where
  liftEq = liftEqDefault

$(deriveMatchable ''G)

data H a b = H0 a | H1 a b | H2 [Either a b]
  deriving (Show, Eq, Functor)

instance (Eq a) => Eq1 (H a) where
  liftEq = liftEqDefault

$(deriveMatchable ''H)

{-

@$(deriveMatchable ''H)@ expands like below:

  instance (Eq a, Matchable (Either a)) => Matchable (H a) where ...

This requires UndecidableInstances extension, and warned by GHC
as it exibits worse type inference.

Mitigating this problem might need manually implement constraint
solver in TH side, so it's not an easy target.

-}

data I a b = I a b (Either [b] (a,b))
  deriving (Show, Eq)

instance (Eq a) => Eq1 (I a) where
  liftEq = liftEqDefault

instance (Eq a) => Functor (I a) where
  fmap = fmapRecovered

$(deriveMatchable ''I)

-------------------------------

data BiF a b = BiF0 | BiF1 a b

instance Eq2 BiF where
  liftEq2 = liftEq2Default

instance Bifunctor BiF where
  bimap = bimapRecovered

$(deriveBimatchable ''BiF)

data BiG a b = BiG0 | BiG1 [a] [b] | BiG2 (Int, BiF a b)

instance Eq2 BiG where
  liftEq2 = liftEq2Default

instance Bifunctor BiG where
  bimap = bimapRecovered

$(deriveBimatchable ''BiG)
