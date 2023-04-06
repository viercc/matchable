{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main(main) where

import Data.Functor.Classes ( Eq2, Eq1 )
import Data.Bifunctor ( Bifunctor(..) )
import Data.Bimatchable ( Bimatchable )
import Data.Matchable ( Matchable )
import Data.Matchable.TH ( deriveInstances )

main :: IO ()
main = putStrLn "compiles"

-- Most simple case
data F a = F0 | F1 a | F2 a a
  deriving (Show, Eq, Functor)

deriveInstances [d|
  deriving instance Eq1 F
  deriving instance Matchable F
  |]

-- Test case for using [] and tuples
newtype G a = G [(a, Int, a)]
  deriving (Show, Eq, Functor)

deriveInstances [d|
  deriving instance Eq1 G
  deriving instance Matchable G
  |]

-- Test case for extra type variable
data H a b = H0 a | H1 a b | H2 [Either a b]
  deriving (Show, Eq, Functor)

deriveInstances [d|
  deriving instance Eq a => Eq1 (H a)
  deriving instance Eq a => Matchable (H a)
  |]

-- Test case for using Matchable and Bimatchable
data I a b = I a (F b) (Either [b] (a,b))
  deriving (Show, Eq)

instance Functor (I a) where
  fmap f (I a fb e) = I a (f <$> fb) (bimap (fmap f) (fmap f) e)

deriveInstances [d|
  deriving instance Eq a => Eq1 (I a)
  deriving instance Eq a => Matchable (I a)
  |]

-- Test case for recursive type
data J a = J0 | J1 (J a, Int) a (Int, J a)
  deriving (Show, Eq, Functor)

deriveInstances [d|
  deriving instance Eq1 J
  deriving instance Matchable J
  |]

-------------------------------

-- Most simple case
data BiF a b = BiF0 | BiF1 a b
  deriving (Show, Eq, Functor)

deriveInstances [d|
  deriving instance Bifunctor BiF
  deriving instance Eq a => Eq1 (BiF a)
  deriving instance Eq a => Matchable (BiF a)
  deriving instance Eq2 BiF
  deriving instance Bimatchable BiF
  |]

-- Test case for using [], tuple, and another Bimatchable instance
data BiG a b = BiG0 | BiG1 [a] [b] | BiG2 (Int, BiF a b)
  deriving (Show, Eq, Functor)

deriveInstances [d|
  deriving instance Bifunctor BiG
  deriving instance Eq a => Eq1 (BiG a)
  deriving instance Eq a => Matchable (BiG a)
  deriving instance Eq2 BiG
  deriving instance Bimatchable BiG
  |]

-- Test case for recursive type
data BiH a b = BiH1 a b | BiH2 (BiH b a) (BiH a String)
  deriving (Show, Eq)

deriveInstances [d|
  deriving instance Bifunctor BiH
  deriving instance Eq a => Eq1 (BiH a)
  deriving instance Eq a => Matchable (BiH a)
  deriving instance Eq2 BiH
  deriving instance Bimatchable BiH
  |]

instance Functor (BiH a) where
  fmap = second
