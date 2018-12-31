{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Map.Orphans where

#if !MIN_VERSION_containers(0,5,9)
import           Data.Map             (Map)
import qualified Data.Map             as Map

import           Data.Functor.Classes

instance Eq2 Map where
  liftEq2 eqK eqV ma mb =
    Map.size ma == Map.size mb &&
    liftEq (liftEq2 eqK eqV) (Map.toAscList ma) (Map.toAscList mb)

instance (Eq k) => Eq1 (Map k) where
  liftEq = liftEq2 (==)
#endif
