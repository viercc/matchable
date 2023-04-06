{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Matchable.TH.Matcher(
    Matcher(..),
    addInfo, 
    matcherToFun,
    
    matcherExpr, 
    funMatcher, liftMatcher, liftMatcher2, combineMatchers
) where

import           Language.Haskell.TH

data Matcher u = Matcher
  { leftPat        :: PatQ
  , rightPat       :: PatQ
  , bodyExp        :: ExpQ
  , additionalInfo :: u }
  deriving Functor

addInfo :: Semigroup a => a -> Matcher a -> Matcher a
addInfo a = fmap (a <>)

matcherToFun :: Matcher a -> ExpQ
matcherToFun matcher = lamE [leftPat matcher, rightPat matcher] (bodyExp matcher)

matcherExpr :: (ExpQ -> ExpQ -> ExpQ) -> a -> Q (Matcher a)
matcherExpr expr a = do
  l <- newName "l"
  r <- newName "r"
  return $ Matcher
    { leftPat = varP l
    , rightPat = varP r
    , bodyExp = expr (varE l) (varE r)
    , additionalInfo = a }

funMatcher :: ExpQ -> a -> Q (Matcher a)
funMatcher f = matcherExpr (\l r -> [| $f $l $r |])

liftMatcher :: ExpQ -> Matcher a -> Q (Matcher a)
liftMatcher lifter matcher = funMatcher [| $lifter $fun |] (additionalInfo matcher)
  where
    fun = matcherToFun matcher

liftMatcher2 :: (Semigroup a) => ExpQ -> Matcher a -> Matcher a -> Q (Matcher a)
liftMatcher2 lifter matcher1 matcher2 = funMatcher [| $lifter $fun1 $fun2 |] info'
  where
    fun1 = matcherToFun matcher1
    fun2 = matcherToFun matcher2
    info' = additionalInfo matcher1 <> additionalInfo matcher2

combineMatchers :: (Monoid a) => ([PatQ] -> PatQ) -> ([ExpQ] -> ExpQ) -> [Matcher a] -> Matcher a
combineMatchers patCombiner expCombiner matchers =
  Matcher {
    leftPat = patCombiner (leftPat <$> matchers)
  , rightPat = patCombiner (rightPat <$> matchers)
  , bodyExp = expCombiner (bodyExp <$> matchers)
  , additionalInfo = foldMap additionalInfo matchers
  }