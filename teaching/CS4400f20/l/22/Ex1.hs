{-

  n is an integer
------------------
 |- n : Integer

 |- e1 : Integer    |- e2 : Integer  op ∈ {+, -, *, /}
 -----------------------------------------------------
    |- e1 op e2 : Integer

 |- e1 : Integer    |- e2 : Integer
 ----------------------------------
    |- e1 - e2 : Integer

-----------------
|- True : Boolean

------------------
|- False : Boolean


|- e1 : Boolean   |- e2 : Boolean
---------------------------------
 |- e1 and e2 : Boolean


|- e1 : Integer   |- e2 : Integer
---------------------------------
   |- e1 <= e2 : Boolean


tenv |- e1 : Boolean   tenv |- e2 : t     tenv |- e3 : t
---------------------------------------------------------
     tenv |- if e1 then e2 else e3 : t


 tenv |- e1 : t1     (set tenv x t1) |- e2 : t2
-----------------------------------------
 tenv |- let x = e1 in e2 : t2

 t = get tenv x
---------------------
 tenv |- x : t

set tenv x t |- e : t'
------------------------------------
        tenv |- (λx : t. e) : t -> t'

tenv |- e1 : t2 -> t1     tenv |- e2 : t2
------------------------------------------
tenv |- e1 e2 : t1


tenv |- e : t -> t
--------------------------
tenv |- fix e : t 


Typing with fix:

fix (λx : Integer. x)

fix 12 : Integer
fix (λx : Integer. 12) : Integer -> Integer

((λx : Integer. 12) (fix (λx : Integer. 12))) : Integer

                     (Integer -> Integer) -> (Integer -> Integer)
    +----------------------------------------------------------------------------------+
    |                                                                                  |
fix (λsum : Integer -> Integer. (λn : Integer. if n <= 0 then 0 else n + (sum (n - 1))))
                                |                                                     |
                                +-----------------------------------------------------+
                                                 Integer -> Integer

tenv |- e : TyForall X t'
---------------------------------
tenv |- TApp e t : tySubst X t t'

tenv |- e : t
----------------------------------
tenv |- TLam X e : TyForall X t

(Lam "x" (TyVar "A") (Var "x")) : TyArrow (TyVar "A") (TyVar "A")
(TLam "A" (Lam "x" (TyVar "A") (Var "x"))) : TyForall "A" (TyArrow (TyVar "A") (TyVar "A")) ~ forall a. a -> a 

-}

import Maps
import Result
import Misc

-- Abstract syntax
data Expr = Integer Integer
          | Boolean Bool
          | Op String Expr Expr
          | And Expr Expr
          | Leq Expr Expr
          | If Expr Expr Expr
          | Var Variable
          | Let Variable Expr Expr
          | Lam Variable Type Expr
          | App Expr Expr
          | Fix Expr
          | TApp Expr Type
          | TLam TyVariable Expr
          deriving (Show, Eq)

{-(TApp (TLam "A" (Lam "x" (TyVar "A") (Var "x"))) TyInteger) 
 -> Lam "x" (TyInteger) (Var "x")
 -}

type TyVariable = String

-- Types
data Type = TyInteger
          | TyBoolean
          | TyArrow Type Type
          | TyVar TyVariable
          | TyForall TyVariable Type
          deriving (Show, Eq)

type TyEnv = Map String Type

-- Typing rules
typeOf :: TyEnv -> Expr -> Result Type
typeOf _ (Integer i) = return TyInteger
typeOf _ (Boolean b) = return TyBoolean
typeOf tenv (Op _ e1 e2) = do
    TyInteger <- typeOf tenv e1
    TyInteger <- typeOf tenv e2
    return TyInteger
typeOf tenv (And e1 e2) = do
    TyBoolean <- typeOf tenv e1
    TyBoolean <- typeOf tenv e2
    return TyBoolean
typeOf tenv (Leq e1 e2) = do
    TyInteger <- typeOf tenv e1
    TyInteger <- typeOf tenv e2
    return TyBoolean
typeOf tenv (If e1 e2 e3) = do
    TyBoolean <- typeOf tenv e1
    t2 <- typeOf tenv e2
    t3 <- typeOf tenv e3
    if t2 == t3
        then return t2
        else fail $ "If: incompatible branch types: " ++ show t2 ++ " and " ++ show t3
typeOf tenv (Var x) = 
    fromMaybe' 
        ("Variable " ++ x ++ " is not bound") $ 
        get tenv x
typeOf tenv (Let x e1 e2) = do
    t1 <- typeOf tenv e1
    typeOf (set tenv x t1) e2
typeOf tenv (Lam x t e) = do
    t' <- typeOf (set tenv x t) e
    return $ TyArrow t t'
typeOf tenv (App e1 e2) = do
    TyArrow t2 t1 <- typeOf tenv e1
    t2' <- typeOf tenv e2
    if t2 == t2'
        then return t1
        else fail $ "App: Incompatible argument types: " ++ show t2 ++ " and " ++ show t2'
typeOf tenv (Fix e) = do
    typ@(TyArrow t t') <- typeOf tenv e
    if t == t' 
        then return t
        else fail $ "Expected t -> t, got " ++ show typ
typeOf tenv (TApp e t) = do
    TyForall tyX t' <- typeOf tenv e
    return $ tySubst tyX t t'
typeOf tenv (TLam tyX e) = do
    t <- typeOf tenv e
    return $ TyForall tyX t


tySubst :: TyVariable -> Type -> Type -> Type
tySubst tyX t (TyArrow t1 t2) = TyArrow (tySubst tyX t t1) (tySubst tyX t t2)
tySubst tyX t (TyVar tyY) | tyX == tyY = t
                          | otherwise = TyVar tyY
tySubst tyX t (TyForall tyY t') 
    | tyX == tyY = TyForall tyY t'
    | otherwise = TyForall tyY' (tySubst tyX t (tySubst tyY (TyVar tyY') t'))
  where
    tyY' = gensym tyY' -- fresh variable not free in any of the terms

tySubst _ _ t' = t'
-- abbreviations
tyArrow ts@(_ : _) = foldr1 TyArrow ts
app es = foldl1 App es
lam args e = foldr (\(x, t) e -> Lam x t e) e args


ex1 = Op "+" (Integer 1) (Integer 3)
ex2 = And (Integer 1) (Boolean True)
ex3 = And (Boolean True) (And (Boolean False) (Boolean True))
ex4 = If (Boolean False) (Integer 1) (Boolean True)
ex5 = If (Boolean False) (Integer 1) (Integer 3)
ex6 = Op "+" (Integer 1) (If (Leq (Integer 4) (Integer 5)) (Integer 3) (Boolean False))
ex7 = Let "x" (Op "+" (Integer 1) (Integer 3)) 
          (Op "+" (Var "x") (Var "x"))
ex8 = Lam "x" TyInteger (Var "x")
ex9 = App ex8 (Op "+" (Integer 1) (Integer 2))
--     TyArrow  TyInteger (TyArrow TyInteger TyBoolean)
ex10 = (Lam "x" TyInteger (Lam "y" TyInteger (Leq (Var "x") (Var "y"))))
ex11 = (App ex10 (Integer 10))
ex12 = (App ex11 (Integer 1))
ex13 = (App ex11 (Boolean True))
ex14 = (Lam "sum" (TyArrow TyInteger TyInteger) 
         (Lam "n" TyInteger
           (If (Leq (Var "n") (Integer 0))
               (Integer 0) 
               (Op "+" (Var "n")
                    (App (Var "sum")
                         (Op "-" (Var "n") (Integer 1)))))))
ex15 = Fix ex14
-- flip
ex16 = lam [ ("f", tyArrow [TyInteger, TyBoolean, TyBoolean])
           , ("x", TyBoolean)
           , ("y", TyInteger) ] $
         app [Var "f", Var "y", Var "x"]
-- id : forall a. a -> a
ex17 = TLam "A" (Lam "x" (TyVar "A") (Var "x"))
-- flip : forall a b c. (a -> b -> c) -> (b -> a -> c)
ex18 = TLam "A" $ TLam "B" $ TLam "C" $
         lam [ ("f", tyArrow [TyVar "A", TyVar "B", TyVar "C"])
           , ("x", TyVar "B")
           , ("y", TyVar "A") ] $
         app [Var "f", Var "y", Var "x"]



showType :: Type -> String 
showType TyInteger = "Integer"
showType TyBoolean = "Boolean"
showType (TyArrow t1 t2) = "(" ++ showType t1 ++ " -> " ++ showType t2 ++ ")"

showExpr :: Expr -> String
showExpr (Integer i) = show i
showExpr (Boolean b) = show b
showExpr (Op op e1 e2) = "(" ++ showExpr e1 ++ " " ++ op ++ " " ++ showExpr e2 ++ ")"
showExpr (And e1 e2) = "(" ++ showExpr e1 ++ " && " ++ showExpr e2 ++ ")"
showExpr (Leq e1 e2) = "(" ++ showExpr e1 ++ " <= " ++ showExpr e2 ++ ")"
showExpr (If e1 e2 e3) = "if " ++ showExpr e1 ++ " then " ++ showExpr e2 ++ " else " ++ showExpr e3
showExpr (Var x) = x 
showExpr (Let x e1 e2) = "let " ++ x ++ " = " ++ showExpr e1 ++ " in " ++ showExpr e2
showExpr (Lam x t e) = "(λ" ++ x ++ " : " ++ showType t ++ ". " ++ showExpr e ++ ")"
showExpr (App e1 e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"
showExpr (Fix e) = "fix " ++ showExpr e

typeCheck e = do
    t <- toIO $ typeOf empty e
    putStrLn $ showExpr e ++ " : " ++ showType t

