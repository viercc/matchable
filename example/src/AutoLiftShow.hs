{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AutoLiftShow(
  autoLiftShowsPrec, autoLiftShowList,
  autoLiftShowsPrec2, autoLiftShowList2,
) where

import Data.Reflection

import Data.Proxy
import Data.Coerce

import Data.Bifunctor

-- | AdHoc's instance is defined ad-hoc manner
newtype AdHoc s a = AdHoc a

-- | Type plumbing utility function
using :: Proxy s -> a -> AdHoc s a
using _ = AdHoc

-- * Automatic Show1 and Show2

-- | Injected dictionary of Show
data ShowDict a = ShowDict
  { _showsPrec :: Int -> a -> ShowS
  , _showList :: [a] -> ShowS
  }

instance (Reifies s (ShowDict a)) => Show (AdHoc s a) where
  showsPrec = coerce $ _showsPrec (reflect (Proxy @s))
  showList = coerce $ _showList (reflect (Proxy @s))

-- | Automatic Show1(liftShowsPrec) 
autoLiftShowsPrec :: (Functor f)
                  => (forall a. Show a => Int -> f a -> ShowS)
                  -> (Int -> b -> ShowS)
                  -> ([b] -> ShowS)
                  -> Int -> f b -> ShowS
autoLiftShowsPrec showsPrecFa showsPrecB showListB p fb =
  reify (ShowDict showsPrecB showListB)
    (\proxy -> showsPrecFa p (fmap (using proxy) fb))

-- | Automatic Show1(liftShowList) 
autoLiftShowList :: (Functor f)
                 =>  (forall a. Show a => [f a] -> ShowS)
                 -> (Int -> b -> ShowS)
                 -> ([b] -> ShowS)
                 -> [f b] -> ShowS
autoLiftShowList showListFa showsPrecB showListB fbs =
  reify (ShowDict showsPrecB showListB)
    (\proxy -> showListFa (fmap (fmap (using proxy)) fbs))

-- | Automatic Show2(liftShowsPrec2)
autoLiftShowsPrec2 :: (Bifunctor f)
                   => (forall a b. (Show a, Show b) => Int -> f a b -> ShowS)
                   -> (Int -> c -> ShowS)
                   -> ([c] -> ShowS)
                   -> (Int -> d -> ShowS)
                   -> ([d] -> ShowS)
                   -> Int -> f c d -> ShowS
autoLiftShowsPrec2 showsPrecFab
  showsPrecC showListC
  showsPrecD showListD
  p fcd =
  reify (ShowDict showsPrecC showListC) $ \proxyC ->
    reify (ShowDict showsPrecD showListD) $ \proxyD ->
      showsPrecFab p (bimap (using proxyC) (using proxyD) fcd)

-- | Automatic Show2(liftShowList2)
autoLiftShowList2 :: (Bifunctor f)
                  => (forall a b. (Show a, Show b) => [f a b] -> ShowS)
                  -> (Int -> c -> ShowS)
                  -> ([c] -> ShowS)
                  -> (Int -> d -> ShowS)
                  -> ([d] -> ShowS)
                  -> [f c d] -> ShowS
autoLiftShowList2 showListFab
  showsPrecC showListC
  showsPrecD showListD
  fcds =
  reify (ShowDict showsPrecC showListC) $ \proxyC ->
    reify (ShowDict showsPrecD showListD) $ \proxyD ->
      showListFab (fmap (bimap (using proxyC) (using proxyD)) fcds)
