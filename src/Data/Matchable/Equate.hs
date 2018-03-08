{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
module Data.Matchable.Equate(
  Equate(..), swapDirection, equate
) where

import           Control.Applicative
import           Data.Foldable       (fold)

import           Control.Monad.Free
import           Data.Matchable

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable

data Equate f a b =
    a :<=> b
  | a :==> f b
  | f a :<== b
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance (Functor f) => Bifunctor (Equate f) where
  bimap f g e = case e of
    a :<=> b  -> f a :<=> g b
    a :==> fb -> f a :==> fmap g fb
    fa :<== b -> fmap f fa :<== g b

instance (Foldable f) => Bifoldable (Equate f) where
  bifoldMap f g e = case e of
    a :<=> b  -> f a `mappend` g b
    a :==> fb -> f a `mappend` foldMap g fb
    fa :<== b -> foldMap f fa `mappend` g b

instance (Traversable f) => Bitraversable (Equate f) where
  bitraverse f g e = case e of
    a :<=> b  -> liftA2 (:<=>) (f a) (g b)
    a :==> fb -> liftA2 (:==>) (f a) (traverse g fb)
    fa :<== b -> liftA2 (:<==) (traverse f fa) (g b)

swapDirection :: Equate f a b -> Equate f b a
swapDirection (a :<=> b)  = b :<=> a
swapDirection (a :==> fb) = fb :<== a
swapDirection (fa :<== b) = b :==> fa

equate :: (Foldable f, Matchable f) => Free f a -> Free f b -> Maybe [Equate (Free f) a b]
equate (Pure a) (Pure b)     = Just [a :<=> b]
equate (Pure a) (Free fmb)   = Just [a :==> Free fmb]
equate (Free fma) (Pure b)   = Just [Free fma :<== b]
equate (Free fma) (Free fmb) = fold <$> zipMatchWith equate fma fmb
