{-# LANGUAGE CPP             #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Matchable.TH (
  deriveInstances,

  deriveMatchable, makeZipMatchWith,
  deriveBimatchable, makeBizipMatchWith
) where

import           Data.Bifunctor (Bifunctor (..))
import           Data.Traversable (forM)
import           Data.Bimatchable             (Bimatchable (..))
import           Data.Matchable               (Matchable (..))
import Data.Functor.Classes ( Eq2(..), Eq1(..) )

import           Language.Haskell.TH hiding (TyVarBndr(..))
import           Language.Haskell.TH.Datatype (ConstructorInfo (..),
                                               DatatypeInfo (..), reifyDatatype)
import           Language.Haskell.TH.Datatype.TyVarBndr

import Data.Bifunctor.TH ( makeBimap )
import Data.Monoid (Any (..))

import Data.Matchable.TH.Matcher

warnStrat :: Maybe DerivStrategy -> Q ()
warnStrat Nothing = pure ()
warnStrat (Just strat) = reportWarning $ "Specifying deriving strategy have no effect: " ++ show strat

data Deriver = Deriver { _className :: Name, _methodDerivers :: [(Name, Name -> Q Exp)] }

deriveInstanceWith :: Deriver -> Cxt -> Type -> Q [Dec]
deriveInstanceWith deriver context ty =
  case spine ty of
    (ConT dataCon, _) -> do
      methods <- forM (_methodDerivers deriver) $ \(methodName, makeImpl) -> do
        impl <- makeImpl dataCon
        pure $ FunD methodName [ Clause [] (NormalB impl) [] ]
      pure [InstanceD Nothing context (ConT (_className deriver) `AppT` ty) methods]
    _ -> do reportError ("Instance declaration must be of shape Cls (TyCon ty1 ty2 ...), but it's" ++ show ty)
            pure []

deriveInstances :: Q [Dec] -> Q [Dec]
deriveInstances decsQ = do
  decs <- decsQ
  derivedDecss <- mapM deriveInstance decs
  pure $ concat derivedDecss

deriveInstance :: Dec -> Q [Dec]
deriveInstance dec = case dec of
  StandaloneDerivD strat context typ -> case typ of
    AppT (ConT cls) typ'
      | cls == ''Eq      -> reportWarning "Use stock deriving for Eq" >> pure [dec]
      | cls == ''Functor -> reportWarning "Use stock deriving for Functor" >> pure [dec]
      | cls == ''Bifunctor -> warnStrat strat >> deriveInstanceWith bifunctorDeriver context typ'
      | cls == ''Eq1     -> warnStrat strat >> deriveInstanceWith eq1Deriver context typ'
      | cls == ''Matchable -> warnStrat strat >> deriveInstanceWith matchableDeriver context typ'
      | cls == ''Eq2     -> warnStrat strat >> deriveInstanceWith eq2Deriver context typ'
      | cls == ''Bimatchable -> warnStrat strat >> deriveInstanceWith bimatchableDeriver context typ'
    _ -> reportError ("Unsupported Instance: " ++ show typ) >> pure []
  _ -> reportError "Use instance declarations only" >> pure []

bifunctorDeriver, eq1Deriver, matchableDeriver, eq2Deriver, bimatchableDeriver :: Deriver
bifunctorDeriver = Deriver ''Bifunctor [ ('bimap, makeBimap) ]
eq1Deriver = Deriver ''Eq1 [ ('liftEq, makeLiftEq) ]

eq2Deriver = Deriver ''Eq2 [ ('liftEq2, makeLiftEq2 ) ]
matchableDeriver = Deriver ''Matchable [ ('zipMatchWith, makeZipMatchWith) ]
bimatchableDeriver = Deriver ''Bimatchable [ ('bizipMatchWith, makeBizipMatchWith) ]

makeLiftEq :: Name -> Q Exp
makeLiftEq name = do
  DatatypeInfo { datatypeVars = dtVarsNames , datatypeCons = cons }
     <- reifyDatatype name
  tyA <- case viewLast dtVarsNames of
    Nothing -> fail $ "Not a type constructor:" ++ show name
    Just (_, a) -> return (VarT (tvName a))
  
  eq <- newName "eq"

  matchClauses <- forM cons $
    \(ConstructorInfo ctrName _ _ fields _ _) -> do
        matcher <- combineMatchers (conP ctrName) andBoolExprs <$> mapM (dEq1Field tyA eq) fields
        let Any bodyUsesF = additionalInfo matcher
            fPat = if bodyUsesF then varP eq else wildP
        return $ clause [fPat, leftPat matcher, rightPat matcher] (normalB (bodyExp matcher)) []
  let mismatchClause = clause [ wildP, wildP, wildP ] (normalB [| False |]) []
      finalClauses = case cons of
        []  -> []
        [_] -> matchClauses
        _   -> matchClauses ++ [mismatchClause]
  
  lifteq <- newName "lifteq"
  letE [ funD lifteq finalClauses ] (varE lifteq)

dEq1Field :: Type -> Name -> Type -> Q (Matcher Any)
dEq1Field tyA fName = go
  where
    isConst t = not (occurs tyA t)

    go ty = case ty of
      _ | ty == tyA -> funMatcher (varE fName) (Any True)
        | isConst ty -> funMatcher ([| (==) |]) (Any False)
      AppT g ty' | isConst g -> do
        matcher <- go ty'
        liftMatcher [| liftEq |] matcher
      AppT (AppT g ty1') ty2' | isConst g -> do
        matcher1 <- go ty1'
        matcher2 <- go ty2'
        liftMatcher2 [| liftEq2 |] matcher1 matcher2
      (spine -> (TupleT _, subtys)) -> do
        matchers <- mapM go (reverse subtys)
        pure $ combineMatchers tupP andBoolExprs matchers
      _ -> unexpectedType ty "Eq1"

makeLiftEq2 :: Name -> Q Exp
makeLiftEq2 name = do
  DatatypeInfo { datatypeVars = dtVarsNames , datatypeCons = cons }
     <- reifyDatatype name
  (tyA, tyB) <- case viewLastTwo dtVarsNames of
    Nothing -> fail $ "Not a type constructor:" ++ show name
    Just (_, a, b) -> return (VarT (tvName a), VarT (tvName b))
  
  eqA <- newName "eqA"
  eqB <- newName "eqB"

  matchClauses <- forM cons $
    \(ConstructorInfo ctrName _ _ fields _ _) -> do
        matcher <- combineMatchers (conP ctrName) andBoolExprs <$> mapM (dEq2Field tyA eqA tyB eqB) fields
        let (Any bodyUsesF, Any bodyUsesG) = additionalInfo matcher
            fPat = if bodyUsesF then varP eqA else wildP
            gPat = if bodyUsesG then varP eqB else wildP
        return $ clause [fPat, gPat, leftPat matcher, rightPat matcher] (normalB (bodyExp matcher)) []
  let mismatchClause = clause [ wildP, wildP, wildP, wildP ] (normalB [| False |]) []
      finalClauses = case cons of
        []  -> []
        [_] -> matchClauses
        _   -> matchClauses ++ [mismatchClause]
  
  lifteq <- newName "lifteq"
  letE [ funD lifteq finalClauses ] (varE lifteq)

dEq2Field :: Type -> Name -> Type -> Name -> Type -> Q (Matcher (Any, Any))
dEq2Field tyA fName tyB gName = go
  where
    isConst t = not (occurs tyA t || occurs tyB t)

    go ty = case ty of
      _ | ty == tyA -> funMatcher (varE fName) (Any True, Any False)
        | ty == tyB -> funMatcher (varE gName) (Any False, Any True)
        | isConst ty -> funMatcher ([| (==) |]) mempty
      AppT g ty' | isConst g -> do
        matcher <- go ty'
        liftMatcher [| liftEq |] matcher
      AppT (AppT g ty1') ty2' | isConst g -> do
        matcher1 <- go ty1'
        matcher2 <- go ty2'
        liftMatcher2 [| liftEq2 |] matcher1 matcher2
      (spine -> (TupleT _, subtys)) -> do
        matchers <- mapM go (reverse subtys)
        pure $ combineMatchers tupP andBoolExprs matchers
      _ -> unexpectedType ty "Eq1"

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
  ((ctx, f), zipMatchWithClauses) <- makeZipMatchWith' name

  dec <- instanceD (pure ctx) (appT (conT ''Matchable) (pure f))
           [ funD 'zipMatchWith zipMatchWithClauses ]

  pure [dec]

makeZipMatchWith :: Name -> ExpQ
makeZipMatchWith name = do
  (_, clauses) <- makeZipMatchWith' name
  z <- newName "z"
  letE [ funD z clauses ] (varE z)

makeZipMatchWith' :: Name -> Q ((Cxt, Type), [Q Clause])
makeZipMatchWith' name = do
  info <- reifyDatatype name
  let DatatypeInfo { datatypeVars = dtVarsNames , datatypeCons = cons } = info
  (dtFunctor, tyA) <- case viewLast (VarT . tvName <$> dtVarsNames) of
    Nothing -> fail $ "Not a type constructor:" ++ show name
    Just (rest, tyA) -> return (foldl AppT (ConT name) rest, tyA)
  
  f <- newName "f"

  matchClausesAndCtxs <- forM cons $
    \(ConstructorInfo ctrName _ _ fields _ _) -> do
        let body = foldl (\x y -> [| $x <*> $y |]) [| pure $(conE ctrName) |]
        matcher <- combineMatchers (conP ctrName) body <$> mapM (dMatchField tyA f) fields
        let (ctx, Any bodyUsesF) = additionalInfo matcher
            fPat = if bodyUsesF then varP f else wildP
        return $ (ctx, clause [fPat, leftPat matcher, rightPat matcher] (normalB (bodyExp matcher)) [])

  let matchClauses = map snd matchClausesAndCtxs
      ctx = concatMap fst matchClausesAndCtxs
      mismatchClause = clause [ wildP, wildP, wildP ] (normalB [| Nothing |]) []
      finalClauses = case cons of
        []  -> []
        [_] -> matchClauses
        _   -> matchClauses ++ [mismatchClause]

  return ((ctx, dtFunctor), finalClauses)

dMatchField :: Type -> Name -> Type -> Q (Matcher (Cxt, Any))
dMatchField tyA fName = go
  where
    isConst = not . occurs tyA

    go ty = case ty of
      _ | ty == tyA -> funMatcher (varE fName) ([], Any True)
        | isConst ty -> 
            let ctx = [ AppT (ConT ''Eq) ty | hasTyVar ty ]
            in matcherExpr
                  (\l r -> [| if $l == $r then Just $l else Nothing |])
                  (ctx, Any False)
      (AppT g ty') | isConst g -> do
        let ctxG = [ AppT (ConT ''Matchable) g | hasTyVar g ]
        matcher <- go ty'
        matcher' <- liftMatcher [| zipMatchWith |] matcher
        return $ (ctxG, mempty) `addInfo` matcher'
      (AppT (AppT g ty1') ty2') | isConst g -> do
        let ctxG = [ AppT (ConT ''Bimatchable) g | hasTyVar g ]
        matcher1 <- go ty1'
        matcher2 <- go ty2'
        matcher' <- liftMatcher2 [| bizipMatchWith |] matcher1 matcher2
        return $ (ctxG, mempty) `addInfo` matcher'
      (spine -> (TupleT n, subtys)) -> do
        let body = foldl (\x y -> [| $x <*> $y |]) [| pure $(conE (tupleDataName n)) |]
        matchers <- mapM go (reverse subtys)
        pure $ combineMatchers tupP body matchers
      _ -> unexpectedType ty "Matchable"

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
  ((ctx, f), clauses) <- makeBizipMatchWith' name

  dec <- instanceD (pure ctx) (appT (conT ''Bimatchable) (pure f))
           [ funD 'bizipMatchWith clauses ]

  pure [dec]

makeBizipMatchWith :: Name -> ExpQ
makeBizipMatchWith name = do
  (_, clauses) <- makeBizipMatchWith' name
  z <- newName "z"
  letE [ funD z clauses ] (varE z)

makeBizipMatchWith' :: Name -> Q ((Cxt, Type), [Q Clause])
makeBizipMatchWith' name = do
  info <- reifyDatatype name
  let DatatypeInfo { datatypeVars = dtVars , datatypeCons = cons } = info
  (dtFunctor, tyA, tyB) <- case viewLastTwo (VarT . tvName <$> dtVars) of
      Nothing -> fail $ "Not a datatype with at least 2 parameters: " ++ show name
      Just (rest, tyA, tyB) -> return (foldl AppT (ConT name) rest, tyA, tyB)

  f <- newName "f"
  g <- newName "g"

  matchClausesAndCtxs <- forM cons $
    \(ConstructorInfo ctrName _ _ fields _ _) -> do
        let body = foldl (\x y -> [| $x <*> $y |]) [| pure $(conE ctrName) |]
        matcher <- combineMatchers (conP ctrName) body <$> mapM (dBimatchField tyA f tyB g) fields
        let (ctx, Any bodyUsesF, Any bodyUsesG) = additionalInfo matcher
            fPat = if bodyUsesF then varP f else wildP
            gPat = if bodyUsesG then varP g else wildP
        return $ (ctx, clause [fPat, gPat, leftPat matcher, rightPat matcher] (normalB (bodyExp matcher)) [])

  let matchClauses = map snd matchClausesAndCtxs
      ctx = concatMap fst matchClausesAndCtxs
      mismatchClause = clause [ wildP, wildP, wildP, wildP ] (normalB [| Nothing |]) []
      finalClauses = case cons of
        []  -> []
        [_] -> matchClauses
        _   -> matchClauses ++ [mismatchClause]

  return ((ctx, dtFunctor), finalClauses)

dBimatchField :: Type -> Name -> Type -> Name -> Type -> Q (Matcher (Cxt, Any, Any))
dBimatchField tyA fName tyB gName = go
  where
    isConst t = not (occurs tyA t || occurs tyB t)
    
    go ty = case ty of
      _ | ty == tyA -> funMatcher (varE fName) ([], Any True, Any False)
        | ty == tyB -> funMatcher (varE gName) ([], Any False, Any True)
        | isConst ty -> 
            let ctx = [ AppT (ConT ''Eq) ty | hasTyVar ty ]
            in matcherExpr
                  (\l r -> [| if $l == $r then Just $l else Nothing |])
                  (ctx, Any False, Any False)
      (AppT g ty') | isConst g -> do
        let ctxG = [ AppT (ConT ''Matchable) g | hasTyVar g ]
        matcher <- go ty'
        matcher' <- liftMatcher [| zipMatchWith |] matcher
        return $ (ctxG, mempty, mempty) `addInfo` matcher'
      (AppT (AppT g ty1') ty2') | isConst g -> do
        let ctxG = [ AppT (ConT ''Bimatchable) g | hasTyVar g ]
        matcher1 <- go ty1'
        matcher2 <- go ty2'
        matcher' <- liftMatcher2 [| bizipMatchWith |] matcher1 matcher2
        return $ (ctxG, mempty, mempty) `addInfo` matcher'
      (spine -> (TupleT n, subtys)) -> do
        matchers <- mapM go (reverse subtys)
        let body = foldl (\x y -> [| $x <*> $y |]) [| pure $(conE (tupleDataName n)) |]
        pure $ combineMatchers tupP body matchers
      _ -> unexpectedType ty "Bimatchable"
    

-----------------------------

unexpectedType :: Type -> String -> Q a
unexpectedType ty cls = fail $
  "unexpected type " ++ show ty ++ " in derivation of " ++ cls ++
  " (it's only possible to implement " ++ cls ++
  " genericaly when all subterms are traversable)"

andBoolExprs :: [Q Exp] -> Q Exp
andBoolExprs [] = [| True |]
andBoolExprs xs = foldr1 (\x y -> [| $x && $y |]) xs

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

viewLast :: [a] -> Maybe ([a], a)
viewLast as = case reverse as of
  [] -> Nothing
  a:rest -> Just (reverse rest, a)

viewLastTwo :: [a] -> Maybe ([a],a,a)
viewLastTwo as = case reverse as of
  b:a:rest -> Just (reverse rest, a, b)
  _ -> Nothing
