{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Classes.Orphans where

import Data.Functor.Classes
import Data.Kind (Type)
import GHC.Generics

---- Orphan Eq1 instances

instance Eq1 U1 where
  liftEq _ _ _ = True

instance Eq1 V1 where
  liftEq _ v _ = case v of {}

instance Eq c => Eq1 (K1 i c) where
  liftEq _ (K1 x) (K1 y) = x == y

instance Eq1 Par1 where
  liftEq eq (Par1 x) (Par1 y) = eq x y

deriving via (f :: Type -> Type) instance Eq1 f => Eq1 (Rec1 f)

deriving via (f :: Type -> Type) instance Eq1 f => Eq1 (M1 i c f)

instance (Eq1 f, Eq1 g) => Eq1 (f :*: g) where
  liftEq eq (f1 :*: g1) (f2 :*: g2) = liftEq eq f1 f2 && liftEq eq g1 g2

instance (Eq1 f, Eq1 g) => Eq1 (f :+: g) where
  liftEq eq (L1 f1) (L1 f2) = liftEq eq f1 f2
  liftEq eq (R1 g1) (R1 g2) = liftEq eq g1 g2
  liftEq _ _ _ = False

instance (Eq1 f, Eq1 g) => Eq1 (f :.: g) where
  liftEq eq (Comp1 fg1) (Comp1 fg2) = liftEq (liftEq eq) fg1 fg2
