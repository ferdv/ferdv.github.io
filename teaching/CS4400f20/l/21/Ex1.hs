{-

  n is an integer
------------------
 |- n : Integer

 |- e1 : Integer    |- e2 : Integer
 ----------------------------------
    |- e1 + e2 : Integer


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

fix (λx : Integer. x)

fix 12 : Integer
fix (λx : Integer. 12) : Integer -> Integer

((λx : Integer. 12) (fix (λx : Integer. 12))) : Integer

                     (Integer -> Integer) -> (Integer -> Integer)
    ------------------------------------------------------------------------------------
fix (λsum : Integer -> Integer. (λn : Integer. if n <= 0 then 0 else n + (sum (n - 1))))

-}

import Maps
import Result

-- Abstract syntax
data Expr = Integer Integer
          | Boolean Bool
          | Add Expr Expr
          | And Expr Expr
          | Leq Expr Expr
          | If Expr Expr Expr
          | Var String
          | Let String Expr Expr
          | Lam String Type Expr
          | App Expr Expr
          | Fix Expr 
          deriving (Show, Eq)

-- Types
data Type = TyInteger
          | TyBoolean
          | TyArrow Type Type
          deriving (Show, Eq)

type TyEnv = Map String Type

-- Typing rules
typeOf :: TyEnv -> Expr -> Result Type
typeOf _ (Integer i) = return TyInteger
typeOf _ (Boolean b) = return TyBoolean
typeOf tenv (Add e1 e2) = do
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
        else fail "if: no type"
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
    if (t2 == t2')
        then return t1
        else fail "Incompatible argument types."
typeOf tenv (Fix e) = do
    TyArrow t t' <- typeOf tenv e
    if t == t' 
        then return t
        else fail "Expected a t -> t type."

ex1 = Add (Integer 1) (Integer 3)
ex2 = And (Integer 1) (Boolean True)
ex3 = And (Boolean True) (And (Boolean False) (Boolean True))
ex4 = If (Boolean False) (Integer 1) (Boolean True)
ex5 = If (Boolean False) (Integer 1) (Integer 3)
ex6 = Add (Integer 1) (If (Leq (Integer 4) (Integer 5)) (Integer 3) (Boolean False))
ex7 = Let "x" (Add (Integer 1) (Integer 3)) 
          (Add (Var "x") (Var "x"))
ex8 = Lam "x" TyInteger (Var "x")
ex9 = App ex8 (Add (Integer 1) (Integer 2))
--     TyArrow  TyInteger (TyArrow TyInteger TyBoolean)
ex10 = (Lam "x" TyInteger (Lam "y" TyInteger (Leq (Var "x") (Var "y"))))
ex11 = (App ex10 (Integer 10))
ex12 = (App ex11 (Integer 1))
ex13 = (App ex11 (Boolean True))
ex14 = (Lam "sum" (TyArrow TyInteger TyInteger) 
         (Lam "n" TyInteger
           (If (Leq (Var "n") (Integer 0))
               (Integer 0) 
               (Add (Var "n")
                    (App (Var "sum")
                         (Add (Var "n") (Integer 1))))))) -- should be Sub 
ex15 = Fix ex14


-- id_integer = (λx : Integer. x)
-- id_boolean = (λx : Boolean. x)
-- id_integer_integer = (λx : Integer -> Integer. x) id_integer
-- if_integer_integer_integer 
-- ...???

showType :: Type -> String 
showType TyInteger = "Integer"
showType TyBoolean = "Boolean"
showType (TyArrow t1 t2) = "(" ++ showType t1 ++ " -> " ++ showType t2 ++ ")"

showExpr :: Expr -> String
showExpr (Integer i) = show i
showExpr (Boolean b) = show b
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (And e1 e2) = "(" ++ showExpr e1 ++ " && " ++ showExpr e2 ++ ")"
showExpr (Leq e1 e2) = "(" ++ showExpr e1 ++ " <= " ++ showExpr e2 ++ ")"
showExpr (If e1 e2 e3) = "if " ++ showExpr e1 ++ " then " ++ showExpr e2 ++ " else " ++ showExpr e3
showExpr (Var x) = x 
showExpr (Let x e1 e2) = "let " ++ x ++ " = " ++ showExpr e1 ++ " in " ++ showExpr e2
showExpr (Lam x t e) = "(λ" ++ x ++ " : " ++ showType t ++ ". " ++ showExpr e ++ ")"
showExpr (App e1 e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"
