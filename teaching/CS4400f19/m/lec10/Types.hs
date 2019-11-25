module Types where

import Maps

type Variable = String

data Value = Num Integer
           | Bool Bool
           deriving (Show, Eq)

data Expr = Val Value
          | Add Expr Expr
          | Sub Expr Expr
          | If Expr Expr Expr
          | And Expr Expr
          | Not Expr
          | Le Expr Expr
          | Let Variable Expr Expr
          | Var Variable
          deriving (Show, Eq)

num n = Val (Num n)
bool b = Val (Bool b)

data Type = TyInt
          | TyBool
          deriving (Show, Eq)

type TEnv = Map Variable Type

-- definition incomplete
typeOf :: TEnv -> Expr -> Maybe Type
typeOf tenv (Val (Num _)) = return TyInt
typeOf tenv (Val (Bool _)) = return TyBool
typeOf tenv (Add e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyInt
typeOf tenv (And e1 e2) =
  do TyBool <- typeOf tenv e1
     TyBool <- typeOf tenv e2
     return TyBool
typeOf tenv (If e1 e2 e3) =
  do TyBool <- typeOf tenv e1
     t2 <- typeOf tenv e2
     t3 <- typeOf tenv e3
     if t2 == t3 
        then return t2
        else Nothing
typeOf tenv (Let x e1 e2) =
  do t1 <- typeOf tenv e1
     t2 <- typeOf (add x t1 tenv) e2
     return t2
typeOf tenv (Var x) = get x tenv
typeOf _ e = error $ "typeOf undefined for " ++ show e

type Env = Map Variable Value

eval :: Env -> Expr -> Maybe Value
eval env (Val v) = Just v
eval env (Add e1 e2) =
  do Num n1 <- eval env e1
     Num n2 <- eval env e2
     return (Num (n1 + n2))
eval env (Sub e1 e2) =
  do Num n1 <- eval env e1
     Num n2 <- eval env e2
     return (Num (n1 + n2))
eval env (If e1 e2 e3) =
  do Bool b1 <- eval env e1
     if b1 
        then eval env e2
        else eval env e3
eval env (And e1 e2) = 
  do Bool b1 <- eval env e1
     Bool b2 <- eval env e2
     return (Bool (b1 && b2))
eval env (Not e) = 
  do Bool b <- eval env e
     return (Bool (not b))
eval env (Le e1 e2) =
  do Num n1 <- eval env e1
     Num n2 <- eval env e2
     return (Bool (n1 <= n2))
eval env (Var x) = get x env
eval env (Let x e1 e2) =
  do v1 <- eval env e1
     eval (add x v1 env) e2

ex1 = Add (num 1) (num 2)
ex2 = Add (num 1) (bool True)
ex3 = Add ex1 ex1
ex4 = Add ex1 ex2
ex5 = Let "x" (And (bool True) (bool False))
        (If (Var "x") (num 20) (num 30))
ex6 = Let "x" (And (bool True) (bool False))
        (If (Var "x") (Var "x") (num 30))
ex7 = Let "x" (Le (num 3) (num 4))
        (If (Var "x") (Var "x") (num 30))






















































{-
data Type = TNum
          | TBool

typeOf :: Expr -> Maybe Type
typeOf (Val (Num _)) = return TNum
typeOf (Add e1 e2) = 
  case typeOf e1 of
       Just TNum -> case typeOf e2 of
                    Just TNum -> return TNum
                    _ -> Nothing
       _ -> Nothing
typeOf (Sub e1 e2) = 
  case typeOf e1 of
       Just TNum -> case typeOf e2 of
                    Just TNum -> return TNum
                    _ -> Nothing
       _ -> Nothing
typeOf (If e1 e2 e3) = 
  case typeOf e1 of
       Just TBool ->
         case typeOf e2 

-}

