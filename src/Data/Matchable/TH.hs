{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Matchable.TH (deriveMatchable, makeZipMatchWith, deriveBimatchable) where

import Data.Matchable (Matchable)

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
--   zipMatchWith _ _ _ = Nothing
-- @
deriveMatchable :: Name -> Q [Dec]
deriveMatchable name = do
  ((ctx, f), zipMatchWithE) <- makeZipMatchWith' name
  
  dec <- instanceD ctx (appT (conT (mkName "Matchable")) (pure f))
           [ funD (mkName "zipMatchWith") [clause [] (normalB zipMatchWithE) []] ]
  
  pure [dec]

makeZipMatchWith :: Name -> ExpQ
makeZipMatchWith name = makeZipMatchWith' name >>= snd

makeZipMatchWith' :: Name -> Q ((Q Cxt, Type), ExpQ)
makeZipMatchWith' name = do
  info <- reifyDatatype name
  let DatatypeInfo { datatypeVars = dtVars , datatypeCons = cons } = info
      tyA : rest' = reverse (removeSig <$> dtVars)
      dtFunctor = foldr (flip AppT) (ConT name) rest'
      
      removeSig (SigT a _) = a
      removeSig a = a

  f <- newName "f"
  
  let mkMatchClause (ConstructorInfo ctrName _ _ fields _ _) =
        do matchers <- mapM (dMatchField tyA f) fields
           let lFieldsP = leftPat <$> matchers
               rFieldsP = rightPat <$> matchers
               bodyUsesF = any usesF matchers
               body = foldl (\x y -> [| $x <*> $y |])
                            [| pure $(conE ctrName) |]
                            (bodyExp <$> matchers)
               ctx = concatMap requiredCtx matchers
               fPat = if bodyUsesF then varP f else wildP
               lPat = conP ctrName lFieldsP
               rPat = conP ctrName rFieldsP
           return (clause [fPat, lPat, rPat] (normalB body) [], ctx)
  
  matchClausesAndCtxs <- mapM mkMatchClause cons
  
  let matchClauses = map fst matchClausesAndCtxs
      ctx = concatMap snd matchClausesAndCtxs
      mismatchClause = clause [ wildP, wildP, wildP ] (normalB [| Nothing |]) []
      finalClauses = case cons of
        []  -> []
        [_] -> matchClauses
        _   -> matchClauses ++ [mismatchClause]

  zmw <- newName "zmw"
  return ((sequenceA ctx, dtFunctor), letE [ funD zmw finalClauses ] (varE zmw))

data Matcher = Matcher
  { leftPat :: PatQ
  , rightPat :: PatQ
  , usesF :: Bool
  , bodyExp :: ExpQ
  , requiredCtx :: [TypeQ] }

dMatchField :: Type -> Name -> Type -> Q Matcher
dMatchField tyA fName ty = case spine ty of
  _ | ty == tyA -> do
        l <- newName "l"
        r <- newName "r"
        return $ Matcher
          { leftPat = varP l
          , rightPat = varP r
          , usesF = True
          , bodyExp = [| $(varE fName) $(varE l) $(varE r) |]
          , requiredCtx = [] }
    | not (occurs tyA ty) -> do
        l <- newName "l"
        r <- newName "r"
        let ctx = [ pure (AppT (ConT ''Eq) ty) | hasTyVar ty ]
        return $ Matcher
          { leftPat = varP l
          , rightPat = varP r
          , usesF = False
          , bodyExp = [| if $(varE l) == $(varE r)
                           then Just $(varE l)
                           else Nothing |]
          , requiredCtx = ctx }
  (ListT, ty':_) -> do
     l <- newName "l"
     r <- newName "r"
     (usesF', ctx, fun) <- do
        matcher <- dMatchField tyA fName ty'
        let fun = lamE [leftPat matcher, rightPat matcher] (bodyExp matcher)
        return (usesF matcher, requiredCtx matcher, fun)
     return $ Matcher
       { leftPat = varP l
       , rightPat = varP r
       , usesF = usesF'
       , bodyExp = [| zipMatchWith $fun $(varE l) $(varE r) |]
       , requiredCtx = ctx }
  (TupleT n, subtys) -> do
     matchers <- mapM (dMatchField tyA fName) (reverse subtys)
     let lP = tupP (leftPat <$> matchers)
         rP = tupP (rightPat <$> matchers)
         tupcon = [| pure $(conE (tupleDataName n)) |]
         anyUsesF = any usesF matchers
         body = foldl (\x y -> [| $x <*> $y |]) tupcon (bodyExp <$> matchers)
         ctx = concatMap requiredCtx matchers
     return $ Matcher
       { leftPat = lP
       , rightPat = rP
       , usesF = anyUsesF
       , bodyExp = body
       , requiredCtx = ctx }
  (ForallT _ _ _, _) -> unexpectedType ty "Matchable"
  (ParensT _, _) -> error "Never reach here"
  (AppT _ _, _) -> error "Never reach here"
  (SigT _ _, _) -> error "Never reach here"
  (ConT tcon, ty' : rest) | all (not . occurs tyA) rest -> do
     l <- newName "l"
     r <- newName "r"
     (usesF', ctx, fun) <- do
        matcher <- dMatchField tyA fName ty'
        let fun = lamE [leftPat matcher, rightPat matcher] (bodyExp matcher)
        return (usesF matcher, requiredCtx matcher, fun)
     let g = foldr (flip AppT) (ConT tcon) rest
         ctxG = [ pure (AppT (ConT ''Matchable) g) | hasTyVar g ]
     return $ Matcher
       { leftPat = varP l
       , rightPat = varP r
       , usesF = usesF'
       , bodyExp = [| zipMatchWith $fun $(varE l) $(varE r) |]
       , requiredCtx = ctxG ++ ctx }
  _ -> unexpectedType ty "Matchable"

spine :: Type -> (Type, [Type])
spine (ParensT t) = spine t
spine (AppT t1 t2) = let (h, r) = spine t1 in (h, t2:r)
spine (SigT t _)   = spine t
spine t            = (t, [])

occurs :: Type -> Type -> Bool
occurs t u | t == u = True
occurs t u = case u of
  AppT u1 u2 -> occurs t u1 || occurs t u2
  ParensT u' -> occurs t u'
  SigT u' _ -> occurs t u'
  _ -> False

hasTyVar :: Type -> Bool
hasTyVar (VarT _) = True
hasTyVar (ParensT t) = hasTyVar t
hasTyVar (AppT t1 t2) = hasTyVar t1 || hasTyVar t2
hasTyVar (SigT t _) = hasTyVar t
hasTyVar _ = False

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
                          | otherwise -> undefined
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

unexpectedType :: Type -> String -> Q a
unexpectedType ty cls = fail $
  "unexpected type " ++ show ty ++ " in derivation of " ++ cls ++
  " (it's only possible to implement " ++ cls ++
  " genericaly when all subterms are traversable)"
