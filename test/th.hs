{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import           Data.Functor.Classes
import           Data.Bifunctor
import           Data.Bimatchable
import           Data.Matchable
import           Data.Matchable.TH

main :: IO ()
main = putStrLn "compiles"

data F a = F0 | F1 a | F2 a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Eq1 F where
  liftEq = liftEqDefault

$(deriveMatchable ''F)

newtype G a = G [(Int, a)]
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Eq1 G where
  liftEq = liftEqDefault

$(deriveMatchable ''G)

-------------------------------

data BiF a b = BiF0 | BiF1 a b

instance Eq2 BiF where
  liftEq2 = liftEq2Default

instance Bifunctor BiF where
  bimap = bimapRecovered

$(deriveBimatchable ''BiF)

data BiG a b = BiG0 | BiG1 [a] [b]

instance Eq2 BiG where
  liftEq2 = liftEq2Default

instance Bifunctor BiG where
  bimap = bimapRecovered

$(deriveBimatchable ''BiG)
