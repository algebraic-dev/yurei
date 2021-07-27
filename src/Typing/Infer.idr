module Typing.Infer

import public Typing.Term 
import Syntax.Term as Untyped 

import Control.Monad.State.State
import Control.Monad.State.Interface
import Control.Monad.Identity
import Control.Monad.Error.Either
import Control.Monad.Error.Interface
import Control.Monad.Trans

import Data.SortedMap
import Data.List

-- Some helper functions 


||| It's like A / B to multiset 
complement : Eq a => List a -> List a -> List a 
complement = foldl (flip Data.List.delete)
 
mapMap : Ord k => (v -> n) -> SortedMap k v -> SortedMap k n
mapMap f sMap = 
    let vals = toList sMap in 
    foldr (\c => insert (Builtin.fst c) (f $ Builtin.snd c)) empty vals

||| Substitutes freeVars for concrete types 

Subst : Type 
Subst = SortedMap String Ty

emptySubst : Subst
emptySubst = empty

||| Type enviroment that stores all mapping from strings to schemas
data Context = MkCtx (SortedMap (List String) Schema)

remove : Context -> List String -> Context 
remove (MkCtx ctx) str = MkCtx (delete str ctx)

findVar : (List String) -> Context -> Maybe Schema
findVar key (MkCtx ctx) = lookup key ctx

insert : Context -> (List String) -> Schema -> Context 
insert (MkCtx ctx) key val = MkCtx (insert key val ctx)

-- Apply and freevars implementation
interface Application a where 
  freeVars : a -> (List String)
  apply : Subst -> a -> a 

Application Ty where 
  freeVars (TyVar r n) = [n]
  freeVars (TyPoly r path ty) = freeVars ty 
  freeVars (TySimple r s) = []
  freeVars (TyArrow r a b) = (freeVars a) `union` (freeVars b)

  apply subs t@(TyVar r n) = 
    case lookup n subs of 
      Just val => val 
      Nothing  => t 
  
  apply subs (TyArrow r arg ret) = TyArrow r (apply subs arg) (apply subs ret)
  apply subs (TyPoly r name type) = TyPoly r name (apply subs type)
  apply subs (TySimple r t) = TySimple r t

Application Schema where 
  freeVars (MkSchema binded ty)   = freeVars ty `complement` binded
  apply subs (MkSchema binded ty) = 
    let bindFreeSubs = foldr Data.SortedMap.delete subs binded in
    MkSchema binded (apply bindFreeSubs ty)

Application a => Application (List a) where 
  apply      = map . apply
  freeVars   = foldr union [] . map freeVars

Application Context where 
  freeVars (MkCtx env) = freeVars (values env)
  apply subs (MkCtx env) = MkCtx (mapMap (apply subs) env)

composeSubst : Subst -> Subst -> Subst 
composeSubst s1 s2 = (mapMap (apply s1) s2) `mergeLeft` (s1)

-- Some functions of Hindley milner

TI : Type -> Type 
TI = StateT Int (EitherT String Identity)

newTyVar : TI Ty
newTyVar = do 
  res <- get 
  put (res + 1)
  pure (TyVar emptyRange (cast $ res + 1))

generalize : Context -> Ty -> Schema
generalize ctx ty = 
  let vars = freeVars ty `complement` freeVars ctx in 
  MkSchema vars ty

instantiate : Schema -> TI Ty
instantiate (MkSchema vars ty) = do 
  nvars <- traverse (const newTyVar) vars
  let subst = fromList $ zip vars nvars 
  pure (apply subst ty)

-- Unification 

isJust : Maybe a -> Bool 
isJust a = case a of 
  Just a => True 
  _ => False

bindVar : String -> Ty -> TI Subst 
bindVar name (TyVar r n) = empty
bindVar name other = 
  if isJust $ find (== name) (freeVars other)
    then pure $ insert name other empty
    else pure $ insert name other empty

unify : Ty -> Ty -> TI Subst 

unify (TyArrow r arg fun) (TyArrow r' arg' fun') = do 
  fstSub <- unify arg arg' 
  sndSub <- unify fun fun' 
  pure (fstSub `composeSubst` sndSub)

unify (TyVar r u) t = bindVar u t
unify t (TyVar r u) = bindVar u t

unify (TySimple r a) (TySimple r' b) = 
  if a == b 
    then pure empty 
    else lift $ throwE ("Cannot unify '" ++ a ++ "' and '" ++ b ++ "'")

unify a b = lift $ throwE ("Cannot unify '" ++ (show a) ++ "' and '" ++ (show b) ++ "'")

-- Inference 

pathToList : Path -> List String
pathToList path =
  case path of 
    (PDot r (MkName _ str) path) => (pathToList path)
    (PName r str)                => [str] 


getVar : Context -> Path -> TI Ty 
getVar ctx path = 
  let listPath = pathToList path in
  case findVar listPath ctx of 
    Just schema => do 
      res <- instantiate schema 
      pure res
    Nothing     => lift $ throwE ("Cannot find variable: '" ++ (show path) ++ "'")

getTy : Types -> Ty 
getTy (TSimple r s)    = TySimple r s 
getTy (TArrow r a b)   = TyArrow r (getTy a) (getTy b)
getTy (TPoly r path n) = TyPoly r path (getTy n)
getTy (TVar r a)       = TyVar r a

insertSchema : Context -> (List String) -> Schema -> Context
insertSchema ctx name schema = insert (remove ctx name) name schema

interface Infer e where 
  infer : Context -> e -> TI (Subst, Ty)

Infer Literal where
  infer ctx (LStr r _) = pure (empty, TySimple r "Text")
  infer ctx (LInt r _) = pure (empty, TySimple r "Int" )

Infer Expr where 
  infer ctx (ELit r lit) = infer ctx lit 
  
  infer ctx (EId path) =
    let listPath = pathToList path in
    case findVar listPath ctx of 
      Just schema => do 
        res <- instantiate schema 
        pure (empty, res)
      Nothing     => lift $ throwE ("Cannot find variable: '" ++ (show path) ++ "'")
  
  infer ctx (ELambda r (MkName r2 name) expr) = do 
    ty <- newTyVar
    let ctxWithArg = insertSchema ctx [name] (MkSchema [] ty)
    (s1, t1) <- infer ctxWithArg expr
    pure (s1, TyArrow r (apply s1 ty) t1)

  infer ctx (ECall r fun expr) = do
    tyRes       <- newTyVar
    (s1, tyFun) <- infer ctx fun 
    (s2, tyArg) <- infer (apply s1 ctx) expr
    s3          <- unify (apply s2 tyFun) (TyArrow r tyArg tyRes) 
    pure ((s3 `composeSubst` s2) `composeSubst` s1, apply s3 tyRes)
  
  infer ctx (EDo r (exprs@(h :: tl))) = do
    let lastExpr    = last exprs 
    let returnsVoid = init exprs
    (subst, ty) <- infer ctx lastExpr 
    subTypeList <- traverse (infer ctx) returnsVoid 
    substList   <- traverse (unify (TySimple emptyRange "Unit")) (map snd subTypeList)
    let voidSub   = foldl composeSubst empty substList
    let lastSub   = subTypeList
    pure (subst `composeSubst` voidSub, ty)
    
  infer ctx (EDo r []) = pure (empty, TySimple r "Unit")

  infer ctx (ELet r (MkName r2 name) val expr) = do
    (s0, tyVal)  <- infer ctx val 
    let s0WithArg = insertSchema (apply s0 ctx) [name] (MkSchema [] tyVal)
    (s1, tyExpr) <- infer s0WithArg expr 
    pure (s1, tyExpr) 

  infer ctx e = lift $ throwE ("Not implemented")

Infer Def where 
  infer ctx (Define name Nothing expr) = do 
    (s1, ty) <- infer ctx expr
    pure (s1, ty)

  infer ctx (Define name (Just ret) expr) = do 
    (s1, ty) <- infer ctx expr
    s2 <- unify (apply s1 (getTy ret)) ty
    pure (s2, ty)

public export 
runInference : Def -> Either String (Subst, Ty)
runInference e = 
  let ctx = (insert ["unit"] (MkSchema [] (TySimple emptyRange "Unit")) empty) in
  let (Id res) = runEitherT $ evalStateT 0 (infer (MkCtx ctx) e) in 
  res