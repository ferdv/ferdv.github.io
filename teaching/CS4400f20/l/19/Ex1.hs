


import Result

-----------------------------------------
-- De Bruijn indices
-----------------------------------------


{-
data Lambda = Var String
            | App Lambda Lambda
            | Lam String Lambda
            deriving (Show, Eq)


ex1 = Lam "x" (Var "x")
ex2 = Lam "y" (Var "y")
ex3 = Lam "z" (Var "z")

-}
{-
Lam  _  (Var  _) (Var   _)
     |        |         |
     +--------+---------+

(Lam "x" (Lam "y" (App (Var "x") (Lam "z" (Var "x")))))

(Lam  _  (Lam _   (App (Var  1)  (Lam  _  (Var  2 )))))
      |                      |         |        |
      +----------------------+         +--------+
-}

-- deBruijn indices

ident = (Lam (Var 0)) -- λx. x => λ. 0

chTrue = (Lam (Lam (Var 1)))  -- λx. λy. x => λ.λ. 1
chFals = (Lam (Lam (Var 0)))  -- λx. λy. y => λ.λ. 0

chZero = (Lam (Lam (Var 0)))  -- λs.λz z => (λ.λ. 0)
chOne = (Lam (Lam (App (Var 1) (Var 0)))) -- λs. λz. s z => (λ.λ. (1 0))
chTwo = (Lam (Lam (App (Var 1) (App (Var 1) (Var 0))))) -- λ.λ. 1 (1 0)

y = (Lam (App (Lam (App (Var 1) (App (Var 0) (Var 0))))
              (Lam (App (Var 1) (App (Var 0) (Var 0)))))) 
--λf. (λx. f (x x)) (λx. f (x x)) => λ. (λ. 1 (0 0)) (λ. 1 (0 0))


data Lambda = Var Int
            | Val Value
            | Lam Lambda
            | App Lambda Lambda
            | Add Lambda Lambda
            deriving Show

data Value = Closure Lambda [Value]
           | Integer Integer
           deriving Show

-- type Env = Map Variable Value

eval :: [Value] -> Lambda -> Result Value
eval env (Val v) = return v
eval env (Var i) = get env i
  where
    get env i | i < length env = return $ env!!i
              | otherwise = fail $ "Variable " ++ show i ++ " not found."
eval env (Lam e) = return $ Closure e env
eval env (App e1 e2) = do
    Closure e env' <- eval env e1
    v2 <- eval env e2
    eval (v2 : env') e
eval env (Add e1 e2) = do
    Integer i1 <- eval env e1
    Integer i2 <- eval env e2
    return $ Integer $ (i1 + i2)
    


ex = (App (App (Lam (Lam (Add (Var 0) (Var 1)))) (Val $ Integer 2)) (Val $ Integer 3))


{-eval [Integer 3, Integer 2] (Add (Var 0) (Var 1)) =
  eval ... (Add (Integer 3) (Integer 2))-}
