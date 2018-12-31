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

  lit, nil, (.:), var,
  prettyTerm,
  
  Query,
  freshVar, (===),
  query,
  
  append
) where

import           Data.Semigroup(Semigroup(..))

import           Control.Monad
import           Data.Functor.Classes
import           Data.Matchable
import           GHC.Generics

import           Control.Monad.State
import           Control.Monad.Free

import           Unification

import           AutoLiftShow

-- | Signature of terms.
data Sig a r = Lit a | Nil | Cons r r
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic1)

instance (Show a) => Show1 (Sig a) where
  liftShowsPrec = autoLiftShowsPrec showsPrec
  liftShowList = autoLiftShowList showList

instance (Eq a) => Eq1 (Sig a) where
  liftEq = liftEqDefault

instance (Eq a) => Matchable (Sig a) where
  zipMatchWith = genericZipMatchWith

data VarName = UserVar String | Temporary Int
  deriving (Eq, Ord, Show)

-- | A term is several nest of @Sig Int@ functor or a variable as base case.
--   That is exactly @Free@.
type Term = Free (Sig Int) VarName

-- * Constructor for @Term@ values.
lit :: Int -> Term
lit = Free . Lit

nil :: Term
nil = Free Nil

(.:) :: Term -> Term -> Term
x .: xs = Free $ Cons x xs

infixr 5 .:

var :: String -> Term
var = pure . UserVar

-- | Pretty printing a term.
prettyTerm :: Term -> String
prettyTerm t = prettyTermP 0 t []
  where
    prettyTermP :: Int -> Term -> ShowS
    prettyTermP p (Pure v) = prettyVarNameP p v
    prettyTermP p (Free ft) = case ft of
      Lit a -> showParen (p > 10) $ ("lit " ++) . showsPrec 11 a
      Nil   -> ("nil" ++)
      Cons u v -> showParen (p > 5) $
        prettyTermP 6 u .
        (" .: " ++) .
        prettyTermP 5 v

    prettyVarNameP :: Int -> VarName -> ShowS
    prettyVarNameP p (UserVar v) = showParen (p > 10) $ ("var " ++) . showsPrec 11 v
    prettyVarNameP _ (Temporary v) = ("?" ++) . shows v

-- | A solution is a substitution.
type Solution = Subst (Sig Int) VarName

-- | A query is a function which takes current solution and returns
--   solutions satisfying added query.
--   
--   Thats:
--   > Solution -> [Solution]
--
--   But some query needs to generate temporary fresh variable.
--   To track how many fresh variables we used, it needs to be:
--   
--   > (Solution, Int) -> [(Solution, Int)]
--
--   And it is useful to be able to use it as a Monad. So we use
--   the final form below.
--
--   > type Query a = (Solution, Int) -> [(a, (Solution, Int))]
--   > type Query = StateT (Solution, Int) []
type Query = StateT (Solution, Int) []

-- | Generate a fresh variable.
freshVar :: Query Term
freshVar = StateT $ \(s,fresh) -> fresh `seq` [(pure (Temporary fresh), (s, fresh + 1))]

-- | @t === u@ is a predicate: @t@ and @u@ unifies.
(===) :: Term -> Term -> Query ()
t === u = StateT $ \(s0, fresh) ->
  case unify (applySubst s0 t) (applySubst s0 u) of
    Left _  -> []
    Right s -> [((), (s <> s0, fresh))]

infix 2 ===

{- |

@query@ runs given @Query@, and prints out solutions for given variables.

Example 1: X ++ [] == [1]

>>> query ["X"] $ append (var "X") nil (lit 1 .: nil)
X = lit 1 .: nil

Example 2: X ++ X == [1,1]

>>> query ["X"] $ append (var "X") (var "X") (lit 1 .: lit 1 .: nil)
X = lit 1 .: nil

-}
query :: [String] -> Query () -> IO ()
query vars solver =
  case execStateT solver (mempty, 0) of
    [] -> putStrLn "No Solution"
    ((s,_):_) -> forM_ vars $ \x ->
      putStrLn $ x ++ " = " ++ prettyTerm (applySubst s (var x))

-- | @append x y z@ is a predicate. It means if we append @x@ and @y@,
--   the result is @z@.
--
--   In pseudo-Prolog,
--   > append(nil, XS, XS).
--   > append([X|XS],[YS],[X|ZS]) :- append(XS, YS, ZS).
--   
--   The implemenation of @append@ below shows how above pseudo-Prolog
--   translates this DSL pretty straitforwardly.
append :: Term -> Term -> Term -> Query ()
append t u v = appendNil `mplus` appendCons
  where
    appendNil =
      do xs <- freshVar
         t === nil
         u === xs
         v === xs

    appendCons =
      do x <- freshVar
         xs <- freshVar
         ys <- freshVar
         zs <- freshVar
         t === x .: xs
         u === ys
         v === x .: zs
         append xs ys zs
