{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Matchable.Orphans() where

#if MIN_VERSION_free(5,0,0)
#else
import           Control.Monad.Free
import           Control.Comonad.Cofree

import           Data.Functor.Classes

instance (Eq1 f) => Eq1 (Free f) where
  liftEq eq = go
    where
      go (Pure a)   (Pure b)   = eq a b
      go (Free fma) (Free fmb) = liftEq go fma fmb
      go _          _          = False


instance (Eq1 f) => Eq1 (Cofree f) where
  liftEq eq = go
    where
      go (a :< fwa) (b :< fwb) = eq a b && liftEq go fwa fwb
#endif
