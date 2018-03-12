{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Matchable.PatternMatch(
  Pattern, match
) where

import qualified Data.Foldable       (fold)

import           Control.Monad.Free
import           Data.Matchable

import           Data.Functor.Foldable

type Pattern t a = Free (Base t) a

match :: (Recursive t, Foldable (Base t), Matchable (Base t)) =>
   Pattern t a -> t -> Maybe [(a, t)]
match  (Pure a)  t = Just [(a, t)]
match (Free fma) t =
  Data.Foldable.fold <$> zipMatchWith match fma (project t)
