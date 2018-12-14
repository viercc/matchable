{- |

This module shows how to implement an unification algorithm
generically, using @Matchable@ type class.

This module assumes terms to be unified are represented by
@Free f a@, where @f@ is some @Matchable@ functor and @a@
is a type for variable.

The relation between matching and unification is previously posed in "unification-fd" package:

<http://hackage.haskell.org/package/unification-fd-0.10.0.1/docs/Control-Unification.html>

This can be thought of @Matchable@ version of unification-fd.

-}
module Unification(
  Subst(..), applySubst,
  UnificationError(..),
  unify
) where

import           Data.Foldable       (toList)
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup

import           Control.Monad.Free
import           Data.Matchable

import qualified Data.Map            as Map

-- | A result of unification is either error or 'Subst' as a solution.
type UnifyResult f a = Either (UnificationError f a) (Subst f a)

-- | Perform unification on two terms.
unify :: (Foldable f, Matchable f, Ord a) => Free f a -> Free f a -> UnifyResult f a
unify t u = unifyLoop [(t,u)] mempty

unifyLoop :: (Foldable f, Matchable f, Ord a)
          => [(Free f a, Free f a)]
          -> Subst f a
          -> UnifyResult f a
unifyLoop [] s = Right s
unifyLoop ((t,u):equs) s =
  case (applySubst s t, applySubst s u) of
    (Pure a, Pure b)
      | a == b    -> unifyLoop equs s
      | otherwise -> unifyLoop equs ((a ==> pure b) <> s)
    (Pure a, u') ->
      if a `occursIn` u'
        then Left $ OccursCheckFail a u'
        else unifyLoop equs ((a ==> u') <> s)
    (t', Pure b) ->
      if b `occursIn` t'
        then Left $ OccursCheckFail b t'
        else unifyLoop equs ((b ==> t') <> s)
    (Free ft, Free fu) ->
      case zipMatch ft fu of
        Nothing  -> Left $ DoNotMatch (Free ft) (Free fu)
        Just ftu -> unifyLoop (toList ftu ++ equs) s

occursIn :: (Eq a, Foldable f) => a -> Free f a -> Bool
occursIn a t = getAny $ foldMap (\b -> Any (a == b)) t

-- | Variable to term substitution. Solution of unification is represented
--   by this type.
newtype Subst f a = Subst { getSubst :: Map.Map a (Free f a) }
  deriving (Show)

-- | Apply a substitution to a term.
applySubst :: (Functor f, Ord a) => Subst f a -> Free f a -> Free f a
applySubst (Subst m) t = t >>= \a -> fromMaybe (pure a) (Map.lookup a m)

-- | @(a ==> t)@ is a substitution which replaces @a@ to @t@, and
--   do not modify other variables.
(==>) :: a -> Free f a -> Subst f a
a ==> t = Subst $ Map.singleton a t

instance (Functor f, Ord a) => Semigroup (Subst f a) where
  Subst m2 <> Subst m1 =
    let m1' = Map.map (applySubst (Subst m2)) m1
    in Subst $ Map.union m2 m1'

instance (Functor f, Ord a) => Monoid (Subst f a) where
  mempty = Subst Map.empty

-- | Cause of failure in unification algorithm.
data UnificationError f a =
    DoNotMatch (Free f a) (Free f a)
  | OccursCheckFail a (Free f a)
  deriving (Show)

