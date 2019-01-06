{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Matchable.TH (deriveMatchable, deriveBimatchable) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
  (DatatypeInfo(..), ConstructorInfo(..), reifyDatatype)

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | Build an instance of 'Matchable' for a data type.
--
-- /e.g./
--
-- @
-- data Exp a = Plus a a | Times a a
-- 'deriveMatchable' ''Exp
-- @
--
-- will create
--
-- @
-- instance Matchable Exp where
--   zipMatchWith f (Plus  l1 l2) (Plus  r1 r2) = pure Plus  <*> f l1 r1 <*> f l2 r2
--   zipMatchWith f (Times l1 l2) (Times r1 r2) = pure Times <*> f l1 r1 <*> f l2 r2
-- @
deriveMatchable :: Name -> Q [Dec]
deriveMatchable name = do
  info <- reifyDatatype name
  let DatatypeInfo { datatypeVars = dtVars , datatypeCons = cons } = info
      SigT tyA _ : _ = reverse dtVars

  dec <- instanceD (pure []) (appT (conT (mkName "Matchable")) (conT name))
    [ funD (mkName "zipMatchWith") $
      let matchClauses = cons <&>
            \(ConstructorInfo ctrName _ _ fields _ _) ->
              let mkFields side = zipWith3
                    (\a b _ -> mkName (a : show b)) (repeat side)
                    [1 :: Int ..]
                    fields
                  (body, bodyUsesF) = foldl
                    (\(con, _usesF) (ty, i) ->
                      let usesF' = if
                            | ty == tyA -> True
                            | otherwise -> unexpectedType ty "Matchable"
                          li = dyn $ "l" ++ show i
                          ri = dyn $ "r" ++ show i
                      in ([| $con <*> f $li $ri |], usesF')
                    )
                    ([| Just $(conE ctrName) |], False)
                    (zip fields [1 :: Int ..])
              in clause
                [ if bodyUsesF then varP $ mkName "f" else wildP
                , conP ctrName $ varP <$> mkFields 'l'
                , conP ctrName $ varP <$> mkFields 'r'
                ]
                (normalB body)
                []

          mismatchClause = clause [ wildP, wildP, wildP ]
            (normalB [| Nothing |])
            []

      -- If we have more than one constructor, emit a clause (at the end) for
      -- two different constructors
      in case cons of
           []  -> []
           [_] -> matchClauses
           _   -> matchClauses ++ [mismatchClause]
    ]
  pure [dec]

-- | Build an instance of 'Bimatchable' for a data type.
--
-- /e.g./
--
-- @
-- data Sum a b = InL a | InR b
-- 'deriveMatchable' ''Sum
-- @
--
-- will create
--
-- @
-- instance Matchable Sum where
--   bizipMatchWith f _ (InL l1) (InL r1) = pure InL <$> f l1 r1
--   bizipMatchWith _ g (InR l1) (InR r1) = pure InR <$> g l1 r1
-- @
deriveBimatchable :: Name -> Q [Dec]
deriveBimatchable name = do
  info <- reifyDatatype name
  let DatatypeInfo { datatypeVars = dtVars , datatypeCons = cons } = info
      SigT tyB _ : SigT tyA _ : _ = reverse dtVars

  dec <- instanceD (pure []) (appT (conT (mkName "Bimatchable")) (conT name))
    [ funD (mkName "bizipMatchWith") $
      let matchClauses = cons <&> \(ConstructorInfo ctrName _ _ fields _ _) ->
            let mkFields side = zipWith3
                  (\a b _ -> mkName (a : show b)) (repeat side)
                  [1 :: Int ..]
                  fields
                (body, bodyUsesF, bodyUsesG) = foldl
                  (\(con, usesF, usesG) (ty, i) ->
                    let (fg, usesF', usesG') = if
                          | ty == tyB -> (dyn "g", usesF, True)
                          | ty == tyA -> (dyn "f", True, usesG)
                          | otherwise -> unexpectedType ty "Bimatchable"
                        li = dyn $ "l" ++ show i
                        ri = dyn $ "r" ++ show i
                    in ([| $con <*> $fg $li $ri |], usesF', usesG')
                  )
                  ([| Just $(conE ctrName) |], False, False)
                  (zip fields [1 :: Int ..])
            in clause
              [ if bodyUsesF then varP $ mkName "f" else wildP
              , if bodyUsesG then varP $ mkName "g" else wildP
              , conP ctrName $ varP <$> mkFields 'l'
              , conP ctrName $ varP <$> mkFields 'r'
              ]
              (normalB body)
              []
          mismatchClause = clause [ wildP, wildP, wildP, wildP ]
            (normalB [| Nothing |])
            []

      -- If we have more than one constructor, emit a clause (at the end) for
      -- two different constructors
      in case cons of
           []  -> []
           [_] -> matchClauses
           _   -> matchClauses ++ [mismatchClause]
    ]
  pure [dec]

unexpectedType :: Type -> String -> a
unexpectedType ty cls = error $
  "unexpected type " ++ show ty ++ " in derivation of " ++ cls ++
  " (it's only possible to implement " ++ cls ++
  " genericaly when all subterms are traversable)"
