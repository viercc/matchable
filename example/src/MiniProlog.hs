{- |

This module implements a very tiny logic programming DSL,
as a stress test for 'Unification' module.

-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module MiniProlog(
  Sig(..),

  VarName, Term,

  lit, nil, cons, var,

  Query,
  (===), (.&.), append,
  query
) where

import           Control.Monad
import           Data.Functor.Classes
import           Data.Matchable
import           GHC.Generics

import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set

import           Control.Monad.Free

import           Unification

-- | Signature of terms.
data Sig a r = Lit a | Nil | Cons r r
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic1)

data Showable = forall a. Showable (Int -> a -> ShowS) a

instance Show Showable where
  showsPrec p (Showable showsp a) = showsp p a

liftShowsPrecDefault ::
  (Functor f)
  => (forall x. Show x => Int -> f x -> ShowS)
  -> (Int -> y -> ShowS) -> ([y] -> ShowS)
  -> Int -> f y -> ShowS
liftShowsPrecDefault showsPrec' showsp _ p fy = showsPrec' p (fmap (Showable showsp) fy)

instance (Show a) => Show1 (Sig a) where
  liftShowsPrec = liftShowsPrecDefault showsPrec

instance (Eq a) => Eq1 (Sig a) where
  liftEq = liftEqDefault

instance (Eq a) => Matchable (Sig a) where
  zipMatchWith = genericZipMatchWith

type VarName = String

-- | A term is several nest of @Sig Int@ functor or a variable as base case.
--   That is exactly @Free@.
type Term = Free (Sig Int) VarName

-- * Constructor for @Term@ values.

lit :: Int -> Term
lit = Free . Lit

nil :: Term
nil = Free Nil

cons :: Term -> Term -> Term
cons x xs = Free $ Cons x xs

var :: VarName -> Term
var = pure

-- | A solution is a substitution from variables to another terms.
type Solution = Subst (Sig Int) VarName

-- | A query is a function which takes current solution and returns
--   solutions satisfying added query.
type Query = Solution -> [Solution]

-- | @t === u@ is a predicate: @t@ and @u@ unifies.
(===) :: Term -> Term -> Query
(t === u) s0 = case unify (applySubst s0 t) (applySubst s0 u) of
  Left _  -> []
  Right s -> [s <> s0]

infix 2 ===

-- | Combine two queries. Returned query is conjunction of two.
(.&.) :: Query -> Query -> Query
(.&.) = (>=>)

infixr 1 .&.

-- | @append x y z@ is a predicate. It means if we append @x@ and @y@, the result is @z@. In pseudo-Prolog,
--
-- > append(nil, XS, XS).
-- > append([X|XS],[YS],[X|ZS]) :- append(XS, YS, ZS).
append :: Term -> Term -> Term -> Query
append t u v s0 =
  (t' === nil .&. u' === var xs .&. v' === var xs) s0 ++
  (t' === var x `cons` var xs .&.
   u' === var ys .&.
   v' === var x `cons` var zs .&.
   append (var xs) (var ys) (var zs)) s0
  where
    f = applySubst s0
    t' = f t
    u' = f u
    v' = f v

    usedVars = Set.unions
      [ varsOfTerm t'
      , varsOfTerm u'
      , varsOfTerm v'
      , varsOfSolution s0 ]

    (x : xs : ys : zs : _) = freshVars usedVars

varsOfTerm :: Term -> Set VarName
varsOfTerm = foldMap Set.singleton

varsOfSolution :: Solution -> Set VarName
varsOfSolution s =
  Map.keysSet (getSubst s) `Set.union` foldMap (foldMap Set.singleton) (getSubst s)

freshVars :: Set VarName -> [VarName]
freshVars usedVars = freshVars0 `eliminate` usedVars
  where
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    freshVars0 = [ "?" ++ c : show n | n <- [1..] :: [Int], c <- alphabet ]

eliminate :: (Ord a) => [a] -> Set a -> [a]
eliminate [] _ = []
eliminate as bs
  | Set.null bs = as
eliminate (a:as) bs
  | Set.member a bs = eliminate as (Set.delete a bs)
  | otherwise       = a : eliminate as bs

{- |

@query@ runs given @Query@, and prints out solutions for given variables.

Example 1: X ++ [] == [1]

>>> query ["X"] $ append (var "X") nil (cons (lit 1) nil)
X = Free (Cons (Free (Lit 1)) (Free Nil))

Example 2: X ++ X == [1,1]

>>> query ["X"] $ append (var "X") (var "X") (cons (lit 1) (cons (lit 1) nil))
X = Free (Cons (Free (Lit 1)) (Free Nil))

-}
query :: [VarName] -> Query -> IO ()
query vars solver =
  case solver mempty of
    []    -> putStrLn "No solution"
    (s:_) -> forM_ vars $ \x ->
      let t = applySubst s (var x)
      in putStrLn $ x ++ " = " ++ show t
