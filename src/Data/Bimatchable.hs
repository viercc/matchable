module Data.Bimatchable(
  -- * Bimatchable class
  Bimatchable(..),
  bimapRecovered,
  eq2Default,
  liftEq2Default
) where

import           Control.Applicative

import           Data.Bifunctor
import           Data.Functor.Classes

import           Data.Tagged

class (Eq2 t, Bifunctor t) => Bimatchable t where
  {- |
  ==== Example
  >>> bizipMatch (Left 1) (Left 'a')
  Just (Left (1,'a'))
  >>> bizipMatch (Right 1) (Right False)
  Just (Right (1,False))
  >>> bizipMatch (Left 1) (Right False)
  Nothing
  -}
  bizipMatch :: t a b -> t a' b' -> Maybe (t (a,a') (b,b'))
  bizipMatch = bizipMatchWith (curry Just) (curry Just)

  bizipMatchWith :: (a -> a' -> Maybe a'')
                 -> (b -> b' -> Maybe b'')
                 -> t a b -> t a' b' -> Maybe (t a'' b'')

  {-# MINIMAL bizipMatchWith #-}

instance Bimatchable Either where
  bizipMatchWith u _ (Left a)  (Left a')  = Left <$> u a a'
  bizipMatchWith _ v (Right b) (Right b') = Right <$> v b b'
  bizipMatchWith _ _ _         _          = Nothing

instance Bimatchable (,) where
  bizipMatch (a, b) (a', b') = Just ((a, a'), (b, b'))
  bizipMatchWith u v (a, b) (a', b') = (,) <$> u a a' <*> v b b'

instance Bimatchable Const where
  bizipMatch (Const a) (Const a') = Just (Const (a, a'))
  bizipMatchWith u _ (Const a) (Const a') = Const <$> u a a'

instance Bimatchable Tagged where
  bizipMatch (Tagged b) (Tagged b') = Just (Tagged (b, b'))
  bizipMatchWith _ v (Tagged b) (Tagged b') = Tagged <$> v b b'

bimapRecovered :: (Bimatchable t)
               => (a -> a') -> (b -> b') -> t a b -> t a' b'
bimapRecovered f g tab =
  case bizipMatchWith (const (Just . f)) (const (Just . g)) tab tab of
    Nothing -> error "bimapRecovered: Unlawful instance of Bimatchable"
    Just r  -> r

eq2Default :: (Bimatchable t, Eq a, Eq b)
           => t a b -> t a b -> Bool
eq2Default = liftEq2Default (==) (==)

liftEq2Default :: (Bimatchable t)
               => (a -> a' -> Bool)
               -> (b -> b' -> Bool)
               -> t a b -> t a' b' -> Bool
liftEq2Default pa pb tab tab' =
  case bizipMatchWith u v tab tab' of
    Nothing -> False
    Just _ -> True
  where u a a' = if pa a a' then Just () else Nothing
        v b b' = if pb b b' then Just () else Nothing
