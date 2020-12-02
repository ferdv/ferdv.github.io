{- Type inference -}

import Maps
import Result
import Misc

import Data.List

-- Abstract syntax
data Expr = Integer Integer
          | Boolean Bool
          | Op String Expr Expr
          | And Expr Expr
          | Leq Expr Expr
          | If Expr Expr Expr
          | Var Variable
          | Let Variable Expr Expr
          | Lam Variable Expr --- remove type annotation
          | App Expr Expr
          | Fix Expr 
          deriving (Show, Eq)

type TyVariable = String

-- Types
data Type = TyInteger
          | TyBoolean
          | TyArrow Type Type
          | TyVar TyVariable --- add a type variable
          deriving (Show, Eq)

type TyEnv = Map String Type

-- Constraints:
-- E.g.,
--   X = TyInteger
--   X = Integer -> Integer
--   X -> X = Integer -> Integer
--   X -> Integer =? Integer -> Boolean
type Constraint = (Type, Type)

-- The type checker now computes a preliminary type together with a set of constraints
typeOf :: TyEnv -> Expr -> Result (Type, [Constraint])
typeOf _ (Integer i) = return (TyInteger, [])
typeOf _ (Boolean b) = return (TyBoolean, [])
typeOf tenv (Op _ e1 e2) = do
    (t1, c1) <- typeOf tenv e1
    (t2, c2) <- typeOf tenv e2
    return (TyInteger, [(t1, TyInteger), (t2, TyInteger)] ++ c1 ++ c2)
typeOf tenv (And e1 e2) = do
    (t1, c1)  <- typeOf tenv e1
    (t2, c2) <- typeOf tenv e2
    return (TyBoolean, [(t1, TyBoolean), (t2, TyBoolean)] ++ c1 ++ c2)
typeOf tenv (Leq e1 e2) = do
    (t1, c1) <- typeOf tenv e1
    (t2, c2) <- typeOf tenv e2
    return (TyBoolean, [(t1, TyInteger), (t2, TyInteger)] ++ c1 ++ c2)
typeOf tenv (If e1 e2 e3) = do
    (t1, c1) <- typeOf tenv e1
    (t2, c2) <- typeOf tenv e2
    (t3, c3) <- typeOf tenv e3
    return (t2, [(t1, TyBoolean), (t2, t3)] ++ c1 ++ c2 ++ c3)
typeOf tenv (Var x) = do
    t <- fromMaybe' ("Variable " ++ x ++ " is not bound") $ get tenv x
    return (t, [])
typeOf tenv (Let x e1 e2) = do
    (t1, c1) <- typeOf tenv e1
    (t2, c2) <- typeOf (set tenv x t1) e2
    return (t2, c1 ++ c2)
typeOf tenv (Lam x e) = do
    let tX = TyVar $ gensym "X"
    (t', c') <- typeOf (set tenv x tX) e -- if x then x + 1 else 3 -> X = TyBoolean, X = TyInteger
    return (TyArrow tX t', c')
typeOf tenv (App e1 e2) = do
    (t1, c1) <- typeOf tenv e1
    (t2, c2) <- typeOf tenv e2
    let tX = TyVar $ gensym "Y"
    return (tX, (t1, TyArrow t2 tX) : c1 ++ c2) 
typeOf tenv (Fix e) = do
    (t, c) <- typeOf tenv e
    let tX = TyVar $ gensym "Z"
    return (tX, (t, TyArrow tX tX) : c)

-- type substitution
tySubst :: TyVariable -> Type -> Type -> Type
tySubst x t (TyArrow t1 t2) = TyArrow (tySubst x t t1) (tySubst x t t2)
tySubst x t (TyVar y) | x == y = t
                      | otherwise = TyVar y
tySubst _ _ t' = t'

-- free type variables in a type
freeTyVars :: Type -> [TyVariable]
freeTyVars (TyArrow t1 t2) = freeTyVars t1 `union` freeTyVars t2
freeTyVars (TyVar x) = [x]
freeTyVars _ = []


-- a substitution is an associative list of type variables and types
type Substitution = [(TyVariable, Type)]

-- applying a Substitution
applySubst :: Substitution -> Type -> Type
applySubst ((x, t) : s) t' = applySubst s (tySubst x t t')
applySubst [] t' = t'

-- solve a set of constraints by unification
unify :: [Constraint] -> Result Substitution
unify ((t1, t2) : rest) | t1 == t2 = unify rest
unify ((TyVar x, t) : rest) | x `notElem` freeTyVars t = do
    s <- unify $ map (\(t1, t2) -> (tySubst x t t1, tySubst x t t2)) rest
    return $ (x, t) : s
unify ((t, TyVar x) : rest) | x `notElem` freeTyVars t = do
    s <- unify $ map (\(t1, t2) -> (tySubst x t t1, tySubst x t t2)) rest
    return $ (x, t) : s
unify ((TyArrow t1 t2, TyArrow t1' t2') : rest) = do 
    unify $ [(t1, t1'), (t2, t2')] ++ rest
unify [] = return []
unify ((t1, t2) : _) = fail $ "Could not unify " ++ showType t1 ++ " and " ++ showType t2

-- step-wise unification, returning an intermediate substitution and constraint
unifyStep :: [Constraint] -> Result (Substitution, [Constraint])
unifyStep ((t1, t2) : rest) | t1 == t2 = 
    return ([], rest)
unifyStep ((TyVar x, t) : rest) | x `notElem` freeTyVars t = do
    let rest' = map (\(t1, t2) -> (tySubst x t t1, tySubst x t t2)) rest
    return ([(x, t)], rest')
unifyStep ((t, TyVar x) : rest) | x `notElem` freeTyVars t = do
    let rest' = map (\(t1, t2) -> (tySubst x t t1, tySubst x t t2)) rest
    return ([(x, t)], rest')
unifyStep ((TyArrow t1 t2, TyArrow t1' t2') : rest) = do 
    return ([], [(t1, t1'), (t2, t2')] ++ rest)
unifyStep [] = return ([], [])
unifyStep ((t1, t2) : _) = fail $ "Could not unify " ++ showType t1 ++ " and " ++ showType t2


-------------- Examples ---------------------

-- abbreviations
tyArrow ts@(_ : _) = foldr1 TyArrow ts
app es = foldl1 App es
lam args e = foldr (\x e -> Lam x e) e args


ex1 = Op "+" (Integer 1) (Integer 3)
ex2 = And (Integer 1) (Boolean True)
ex3 = And (Boolean True) (And (Boolean False) (Boolean True))
ex4 = If (Boolean False) (Integer 1) (Boolean True)
ex5 = If (Boolean False) (Integer 1) (Integer 3)
ex6 = Op "+" (Integer 1) (If (Leq (Integer 4) (Integer 5)) (Integer 3) (Boolean False))
ex7 = Let "x" (Op "+" (Integer 1) (Integer 3)) 
          (Op "+" (Var "x") (Var "x"))
ex8 = Lam "x" (Var "x")
ex9 = App ex8 (Op "+" (Integer 1) (Integer 2))
ex10 = (Lam "x" (Lam "y" (Leq (Var "x") (Var "y"))))
ex11 = (App ex10 (Integer 10))
ex12 = (App ex11 (Integer 1))
ex13 = (App ex11 (Boolean True))
ex14 = (Lam "sum"
         (Lam "n" 
           (If (Leq (Var "n") (Integer 0))
               (Integer 0) 
               (Op "+" (Var "n")
                    (App (Var "sum")
                         (Op "-" (Var "n") (Integer 1)))))))
ex15 = Fix ex14
-- flip
ex16 = lam [ "f", "x", "y"] $
         app [Var "f", Var "y", Var "x"]
-- while the identity function (ex8) is initially assigned a type only 
-- involving variables, there is only one way of instantiating them, so the 
-- following fails to type-check
ex17 = Let "id" ex8 $
           If (App (Var "id") $ Boolean True)
              (App (Var "id") $ Integer 3)
              (Integer 2)

showType :: Type -> String 
showType TyInteger = "Integer"
showType TyBoolean = "Boolean"
showType (TyArrow t1 t2) = "(" ++ showType t1 ++ " -> " ++ showType t2 ++ ")"
showType (TyVar x) = x

showExpr :: Expr -> String
showExpr (Integer i) = show i
showExpr (Boolean b) = show b
showExpr (Op op e1 e2) = "(" ++ showExpr e1 ++ " " ++ op ++ " " ++ showExpr e2 ++ ")"
showExpr (And e1 e2) = "(" ++ showExpr e1 ++ " && " ++ showExpr e2 ++ ")"
showExpr (Leq e1 e2) = "(" ++ showExpr e1 ++ " <= " ++ showExpr e2 ++ ")"
showExpr (If e1 e2 e3) = "if " ++ showExpr e1 ++ " then " ++ showExpr e2 ++ " else " ++ showExpr e3
showExpr (Var x) = x 
showExpr (Let x e1 e2) = "let " ++ x ++ " = " ++ showExpr e1 ++ " in " ++ showExpr e2
showExpr (Lam x e) = "(λ" ++ x ++ ". " ++ showExpr e ++ ")"
showExpr (App e1 e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"
showExpr (Fix e) = "(fix " ++ showExpr e ++ ")"

typeCheck e = do
    putStrLn $ "Checking: " ++ showExpr e
    (t, c) <- toIO $ typeOf empty e
    putStrLn $ "Initial type: " ++ showType t
    putStrLn $ "Initial set of constraints:"
    mapM_ (\(t1, t2) -> putStrLn $ "   " ++ showType t1 ++ " = " ++ showType t2) c
    s <- toIO $ unify c
    putStrLn $ "Final type: " ++ (showType $ applySubst s t)

-- Interactive step-wise unification for inferring the final type
-- E.g., in GHCi:
--
--   > typeCheckStep ex14
--
typeCheckStep e = do
    putStrLn $ "Checking: " ++ showExpr e
    (t, c) <- toIO $ typeOf empty e
    t' <- loop t [] c
    putStrLn $ "Done. Final type: " ++ showType t'
  where 
    loop t s c = do
        let t' = applySubst s t
        printInfo t' s c
        putStrLn $ "Press enter to continue..."
        getLine
        (s', c') <- toIO $ unifyStep c
        if c' == c
            then return t'
            else loop t' (s' ++ s) c'

    printInfo t s c = do
        putStrLn $ "Current type: " ++ showType t
        putStrLn $ "Current constraints:"
        mapM_ (\(t1, t2) -> putStrLn $ "   " ++ showType t1 ++ " = " ++ showType t2) c
        putStrLn $ "Current substitution: "
        putStrLn $ "  " ++ concatMap (\(x, t) -> x ++ " := " ++ showType t ++ ", ") s
      

