{-# LANGUAGE CPP             #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Matchable.TH (
  deriveMatchable, makeZipMatchWith,
  deriveBimatchable, makeBizipMatchWith
) where

import           Data.Bimatchable             (Bimatchable (..))
import           Data.Matchable               (Matchable (..))

import           Data.Monoid                  (Monoid (..))
import           Data.Semigroup               (Semigroup (..))

import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype (ConstructorInfo (..),
                                               DatatypeInfo (..), reifyDatatype)

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

  dec <- instanceD ctx (appT (conT ''Matchable) (pure f))
           [ funD 'zipMatchWith [clause [] (normalB zipMatchWithE) []] ]

  pure [dec]

makeZipMatchWith :: Name -> ExpQ
makeZipMatchWith name = makeZipMatchWith' name >>= snd

boundName :: TyVarBndr -> Name
boundName (KindedTV a _) = a
boundName (PlainTV a)    = a

makeZipMatchWith' :: Name -> Q ((Q Cxt, Type), ExpQ)
makeZipMatchWith' name = do
  info <- reifyDatatype name
  let DatatypeInfo { datatypeVars = dtVars , datatypeCons = cons } = info
      tyA : rest' = reverse (VarT . boundName <$> dtVars)
      dtFunctor = foldr (flip AppT) (ConT name) rest'

  f <- newName "f"

  let mkMatchClause (ConstructorInfo ctrName _ _ fields _ _) =
        do matchers <- mapM (dMatchField tyA f) fields
           let lFieldsP = leftPat <$> matchers
               rFieldsP = rightPat <$> matchers
               bodyUsesF = any additionalInfo matchers
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

data Matcher u = Matcher
  { leftPat        :: PatQ
  , rightPat       :: PatQ
  , bodyExp        :: ExpQ
  , requiredCtx    :: [TypeQ]
  , additionalInfo :: u }

dMatchField :: Type -> Name -> Type -> Q (Matcher Bool)
dMatchField tyA fName ty = case spine ty of
  _ | ty == tyA -> do
        l <- newName "l"
        r <- newName "r"
        return $ Matcher
          { leftPat = varP l
          , rightPat = varP r
          , additionalInfo = True
          , bodyExp = [| $(varE fName) $(varE l) $(varE r) |]
          , requiredCtx = [] }
    | not (occurs tyA ty) -> do
        l <- newName "l"
        r <- newName "r"
        let ctx = [ pure (AppT (ConT ''Eq) ty) | hasTyVar ty ]
        return $ Matcher
          { leftPat = varP l
          , rightPat = varP r
          , additionalInfo = False
          , bodyExp = [| if $(varE l) == $(varE r)
                           then Just $(varE l)
                           else Nothing |]
          , requiredCtx = ctx }
  (ListT, ty':_) -> dWrapped ty'
  (TupleT n, subtys) -> do
     matchers <- mapM (dMatchField tyA fName) (reverse subtys)
     let lP = tupP (leftPat <$> matchers)
         rP = tupP (rightPat <$> matchers)
         tupcon = [| pure $(conE (tupleDataName n)) |]
         anyUsesF = any additionalInfo matchers
         body = foldl (\x y -> [| $x <*> $y |]) tupcon (bodyExp <$> matchers)
         ctx = concatMap requiredCtx matchers
     return $ Matcher
       { leftPat = lP
       , rightPat = rP
       , additionalInfo = anyUsesF
       , bodyExp = body
       , requiredCtx = ctx }
  (ConT tcon, ty' : rest) | all (not . occurs tyA) rest -> do
     let g = foldr (flip AppT) (ConT tcon) rest
         ctxG = [ pure (AppT (ConT ''Matchable) g) | hasTyVar g ]
     matcher <- dWrapped ty'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (ConT tcon, ty1' : ty2' : rest) | all (not . occurs tyA) rest -> do
     let g = foldr (flip AppT) (ConT tcon) rest
         ctxG = [ pure (AppT (ConT ''Bimatchable) g) | hasTyVar g ]
     -- Note that since @spine@ reverses argument order,
     -- it must be dWrappedBi ty2 ty1.
     matcher <- dWrappedBi ty2' ty1'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (VarT t, ty' : rest) | all (not . occurs tyA) rest -> do
     let g = foldr (flip AppT) (VarT t) rest
         ctxG = [ pure (AppT (ConT ''Matchable) g) ]
     matcher <- dWrapped ty'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (VarT t, ty1' : ty2' : rest) | all (not . occurs tyA) rest -> do
     let g = foldr (flip AppT) (VarT t) rest
         ctxG = [ pure (AppT (ConT ''Bimatchable) g) | hasTyVar g ]
     matcher <- dWrappedBi ty2' ty1'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (ForallT _ _ _, _) -> unexpectedType ty "Matchable"
  (ParensT _, _) -> error "Never reach here"
  (AppT _ _, _) -> error "Never reach here"
  (SigT _ _, _) -> error "Never reach here"
  _ -> unexpectedType ty "Matchable"

  where
    dWrapped :: Type -> Q (Matcher Bool)
    dWrapped ty' =do
      l <- newName "l"
      r <- newName "r"
      (usesF', ctx, fun) <- do
         matcher <- dMatchField tyA fName ty'
         let fun = lamE [leftPat matcher, rightPat matcher] (bodyExp matcher)
         return (additionalInfo matcher, requiredCtx matcher, fun)
      return $ Matcher
        { leftPat = varP l
        , rightPat = varP r
        , additionalInfo = usesF'
        , bodyExp = [| zipMatchWith $fun $(varE l) $(varE r) |]
        , requiredCtx = ctx }

    dWrappedBi :: Type -> Type -> Q (Matcher Bool)
    dWrappedBi ty1 ty2 = do
      l <- newName "l"
      r <- newName "r"
      (usesF', ctx, fun1, fun2) <- do
         matcher1 <- dMatchField tyA fName ty1
         matcher2 <- dMatchField tyA fName ty2
         let fun1 = lamE [leftPat matcher1, rightPat matcher1] (bodyExp matcher1)
             fun2 = lamE [leftPat matcher2, rightPat matcher2] (bodyExp matcher2)
             usesF' = additionalInfo matcher1 || additionalInfo matcher2
             ctx = requiredCtx matcher1 ++ requiredCtx matcher2
         return (usesF', ctx, fun1, fun2)
      return $ Matcher
        { leftPat = varP l
        , rightPat = varP r
        , additionalInfo = usesF'
        , bodyExp = [| bizipMatchWith $fun1 $fun2 $(varE l) $(varE r) |]
        , requiredCtx = ctx }

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
  ((ctx, f), zipMatchWithE) <- makeBizipMatchWith' name

  dec <- instanceD ctx (appT (conT ''Bimatchable) (pure f))
           [ funD 'bizipMatchWith [clause [] (normalB zipMatchWithE) []] ]

  pure [dec]

makeBizipMatchWith :: Name -> ExpQ
makeBizipMatchWith name = makeBizipMatchWith' name >>= snd

makeBizipMatchWith' :: Name -> Q ((Q Cxt, Type), ExpQ)
makeBizipMatchWith' name = do
  info <- reifyDatatype name
  let DatatypeInfo { datatypeVars = dtVars , datatypeCons = cons } = info
      tyB : tyA : rest' = reverse (VarT . boundName <$> dtVars)
      dtFunctor = foldr (flip AppT) (ConT name) rest'

  f <- newName "f"
  g <- newName "g"

  let mkMatchClause (ConstructorInfo ctrName _ _ fields _ _) =
        do matchers <- mapM (dBimatchField tyA f tyB g) fields
           let lFieldsP = leftPat <$> matchers
               rFieldsP = rightPat <$> matchers
               Usage2 usesF usesG = foldMap additionalInfo matchers
               body = foldl (\x y -> [| $x <*> $y |])
                            [| pure $(conE ctrName) |]
                            (bodyExp <$> matchers)
               ctx = concatMap requiredCtx matchers
               fPat = if usesF then varP f else wildP
               gPat = if usesG then varP g else wildP
               lPat = conP ctrName lFieldsP
               rPat = conP ctrName rFieldsP
           return (clause [fPat, gPat, lPat, rPat] (normalB body) [], ctx)

  matchClausesAndCtxs <- mapM mkMatchClause cons

  let matchClauses = map fst matchClausesAndCtxs
      ctx = concatMap snd matchClausesAndCtxs
      mismatchClause = clause [ wildP, wildP, wildP, wildP ] (normalB [| Nothing |]) []
      finalClauses = case cons of
        []  -> []
        [_] -> matchClauses
        _   -> matchClauses ++ [mismatchClause]

  bzmw <- newName "bzmw"
  return ((sequenceA ctx, dtFunctor), letE [ funD bzmw finalClauses ] (varE bzmw))

data FunUsage2 = Usage2 Bool Bool

instance Semigroup FunUsage2 where
  Usage2 f1 g1 <> Usage2 f2 g2 = Usage2 (f1 || f2) (g1 || g2)

instance Monoid FunUsage2 where
  mempty = Usage2 False False
  mappend = (<>)

dBimatchField :: Type -> Name -> Type -> Name -> Type -> Q (Matcher FunUsage2)
dBimatchField tyA fName tyB gName ty = case spine ty of
  _ | ty == tyA -> do
        l <- newName "l"
        r <- newName "r"
        return $ Matcher
          { leftPat = varP l
          , rightPat = varP r
          , additionalInfo = Usage2 True False
          , bodyExp = [| $(varE fName) $(varE l) $(varE r) |]
          , requiredCtx = [] }
    | ty == tyB -> do
        l <- newName "l"
        r <- newName "r"
        return $ Matcher
          { leftPat = varP l
          , rightPat = varP r
          , additionalInfo = Usage2 False True
          , bodyExp = [| $(varE gName) $(varE l) $(varE r) |]
          , requiredCtx = [] }
    | isConst ty -> do
        l <- newName "l"
        r <- newName "r"
        let ctx = [ pure (AppT (ConT ''Eq) ty) | hasTyVar ty ]
        return $ Matcher
          { leftPat = varP l
          , rightPat = varP r
          , additionalInfo = Usage2 False False
          , bodyExp = [| if $(varE l) == $(varE r)
                           then Just $(varE l)
                           else Nothing |]
          , requiredCtx = ctx }
  (ListT, ty':_) -> dWrapped ty'
  (TupleT n, subtys) -> do
     matchers <- mapM (dBimatchField tyA fName tyB gName) (reverse subtys)
     let lP = tupP (leftPat <$> matchers)
         rP = tupP (rightPat <$> matchers)
         tupcon = [| pure $(conE (tupleDataName n)) |]
         anyUsesF = foldMap additionalInfo matchers
         body = foldl (\x y -> [| $x <*> $y |]) tupcon (bodyExp <$> matchers)
         ctx = concatMap requiredCtx matchers
     return $ Matcher
       { leftPat = lP
       , rightPat = rP
       , additionalInfo = anyUsesF
       , bodyExp = body
       , requiredCtx = ctx }
  (ConT tcon, ty' : rest) | all isConst rest -> do
     let g = foldr (flip AppT) (ConT tcon) rest
         ctxG = [ pure (AppT (ConT ''Matchable) g) | hasTyVar g ]
     matcher <- dWrapped ty'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (ConT tcon, ty1' : ty2' : rest) | all isConst rest -> do
     let g = foldr (flip AppT) (ConT tcon) rest
         ctxG = [ pure (AppT (ConT ''Bimatchable) g) | hasTyVar g ]
     -- Note that since @spine@ reverses argument order,
     -- it must be dWrappedBi ty2 ty1.
     matcher <- dWrappedBi ty2' ty1'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (VarT t, ty' : rest) | all isConst rest -> do
     let g = foldr (flip AppT) (VarT t) rest
         ctxG = [ pure (AppT (ConT ''Matchable) g) ]
     matcher <- dWrapped ty'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (VarT t, ty1' : ty2' : rest) | all isConst rest -> do
     let g = foldr (flip AppT) (VarT t) rest
         ctxG = [ pure (AppT (ConT ''Bimatchable) g) | hasTyVar g ]
     matcher <- dWrappedBi ty2' ty1'
     return $ matcher{ requiredCtx = ctxG ++ requiredCtx matcher }
  (ForallT _ _ _, _) -> unexpectedType ty "Bimatchable"
  (ParensT _, _) -> error "Never reach here"
  (AppT _ _, _) -> error "Never reach here"
  (SigT _ _, _) -> error "Never reach here"
  _ -> unexpectedType ty "Bimatchable"

  where
    isConst :: Type -> Bool
    isConst t = not (occurs tyA t || occurs tyB t)

    dWrapped :: Type -> Q (Matcher FunUsage2)
    dWrapped ty' = do
      l <- newName "l"
      r <- newName "r"
      (usesF', ctx, fun) <- do
         matcher <- dBimatchField tyA fName tyB gName ty'
         let fun = lamE [leftPat matcher, rightPat matcher] (bodyExp matcher)
         return (additionalInfo matcher, requiredCtx matcher, fun)
      return $ Matcher
        { leftPat = varP l
        , rightPat = varP r
        , additionalInfo = usesF'
        , bodyExp = [| zipMatchWith $fun $(varE l) $(varE r) |]
        , requiredCtx = ctx }

    dWrappedBi :: Type -> Type -> Q (Matcher FunUsage2)
    dWrappedBi ty1 ty2 = do
      l <- newName "l"
      r <- newName "r"
      (usesF', ctx, fun1, fun2) <- do
         matcher1 <- dBimatchField tyA fName tyB gName ty1
         matcher2 <- dBimatchField tyA fName tyB gName ty2
         let fun1 = lamE [leftPat matcher1, rightPat matcher1] (bodyExp matcher1)
             fun2 = lamE [leftPat matcher2, rightPat matcher2] (bodyExp matcher2)
             usesF' = additionalInfo matcher1 <> additionalInfo matcher2
             ctx = requiredCtx matcher1 ++ requiredCtx matcher2
         return (usesF', ctx, fun1, fun2)
      return $ Matcher
        { leftPat = varP l
        , rightPat = varP r
        , additionalInfo = usesF'
        , bodyExp = [| bizipMatchWith $fun1 $fun2 $(varE l) $(varE r) |]
        , requiredCtx = ctx }

-----------------------------

unexpectedType :: Type -> String -> Q a
unexpectedType ty cls = fail $
  "unexpected type " ++ show ty ++ " in derivation of " ++ cls ++
  " (it's only possible to implement " ++ cls ++
  " genericaly when all subterms are traversable)"

spine :: Type -> (Type, [Type])
spine (ParensT t)  = spine t
spine (AppT t1 t2) = let (h, r) = spine t1 in (h, t2:r)
spine (SigT t _)   = spine t
spine t            = (t, [])

occurs :: Type -> Type -> Bool
occurs t u | t == u = True
occurs t u = case u of
  AppT u1 u2 -> occurs t u1 || occurs t u2
  ParensT u' -> occurs t u'
  SigT u' _  -> occurs t u'
  _          -> False

hasTyVar :: Type -> Bool
hasTyVar (VarT _)     = True
hasTyVar (ParensT t)  = hasTyVar t
hasTyVar (AppT t1 t2) = hasTyVar t1 || hasTyVar t2
hasTyVar (SigT t _)   = hasTyVar t
hasTyVar _            = False
