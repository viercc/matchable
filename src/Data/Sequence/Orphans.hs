{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Sequence.Orphans where

#if !MIN_VERSION_containers(0,5,9)
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq

import qualified Data.Foldable        as F

import           Data.Functor.Classes

instance Eq1 Seq where
  liftEq eqV ma mb =
    Seq.length ma == Seq.length mb &&
    liftEq eqV (F.toList ma) (F.toList mb)
#endif
