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

-- Most simple case
data F a = F0 | F1 a | F2 a a
  deriving (Show, Eq, Functor)

instance Eq1 F where
  liftEq = liftEqDefault

$(deriveMatchable ''F)

-- Test case for using [] and tuples
newtype G a = G [(a, Int, a)]
  deriving (Show, Eq, Functor)

instance Eq1 G where
  liftEq = liftEqDefault

$(deriveMatchable ''G)

-- Test case for extra type variable
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

-- Test case for using Matchable and Bimatchable
data I a b = I a (F b) (Either [b] (a,b))
  deriving (Show, Eq)

instance (Eq a) => Eq1 (I a) where
  liftEq = liftEqDefault

instance (Eq a) => Functor (I a) where
  fmap = fmapRecovered

$(deriveMatchable ''I)

-- Test case for recursive type
data J a = J0 | J1 (J a, Int) a (Int, J a)
  deriving (Show, Eq, Functor)

instance Eq1 J where
  liftEq = liftEqDefault

$(deriveMatchable ''J)

-------------------------------

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

-- Test case for recursive type
data BiH a b = BiH1 a b | BiH2 (BiH b a) (BiH a String)

instance Eq2 BiH where
  liftEq2 = liftEq2Default

instance Bifunctor BiH where
  bimap = bimapRecovered

$(deriveBimatchable ''BiH)
