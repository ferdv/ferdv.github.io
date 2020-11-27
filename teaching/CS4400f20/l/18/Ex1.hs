
import Maps
import Result

type Variable = String

data Value = Closure Variable Expr Env
           | Integer Integer
           | Boolean Bool
           | PrimOp Op Integer [Value]
           deriving (Show, Eq)

int i = Val (Integer i)
true = Val (Boolean True)
false = Val (Boolean False)

data Expr = Var Variable
          | Lam Variable Expr
          | App Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
          | Let Variable Expr Expr
          | LetRec Variable Expr Expr
          | Val Value
           deriving (Show, Eq)

data Op = Op String ([Value] -> Result Value)

instance Show Op where
    show (Op s _) = "Op " ++ s

instance Eq Op where
    _ == _ = False

type Env = Map Variable Value

-- Call-by-value evaluator

eval :: Env -> Expr -> Result Value
eval env (Var x) = case get env x of
    Just v -> Success v
    Nothing -> fail $ "Variable " ++ x  ++ " not defined."
eval env (Lam x e) = return $ Closure x e env
eval env (App e1 e2) = do
    v1 <- eval env e1
    case v1 of 
         Closure x e env' -> do
            v2 <- eval env e2
            eval (set env' x v2) e
         PrimOp op 1 args -> do 
            v2 <- eval env e2
            apply op (args ++ [v2])
         PrimOp op n args | n > 1 -> do 
            v2 <- eval env e2
            return $ PrimOp op (n - 1) (args ++ [v2])
eval env (If e1 e2 e3) = do
    Boolean b <- eval env e1
    if b then eval env e2
         else eval env e3
eval env (Let x e1 e2) = do
  v1 <- eval env e1
  eval (set env x v1) e2


eval env (LetRec f (Lam x e) e2) = -- letrec x = e1 in e2 ==> let x = fix (λx. e1) in e2
   let v1 = Closure x e env'
       env' = set env f v1
   in eval env' e2

eval env (LetRec f e1 e2) = eval env (Let f e1 e2)



eval env (Val v) = return v


apply :: Op -> [Value] -> Result Value
apply (Op _ f) vs = f vs

addOp = PrimOp (Op "+" op) 2 []
  where
    op [Integer i1, Integer i2] = return $ Integer $ i1 + i2
    op _ = fail "Wrong number or type of arguments."
mulOp = PrimOp (Op "*" op) 2 []
  where
    op [Integer i1, Integer i2] = return $ Integer $ i1 * i2
    op _ = fail "Wrong number or type of arguments."
subOp = PrimOp (Op "-" op) 2 []
  where
    op [Integer i1, Integer i2] = return $ Integer $ i1 - i2
    op _ = fail "Wrong number or type of arguments."
leqOp = PrimOp (Op "<=" op) 2 []
  where
    op [Integer i1, Integer i2] = return $ Boolean $ i1 <= i2
    op _ = fail "Wrong number or type of arguments."


builtins :: Env
builtins = 
  [ ("+", wrap addOp)
  , ("-", wrap subOp)
  , ("*", wrap mulOp)
  , ("<=", wrap leqOp)
  ]
  where wrap = id

ex1 = LetRec "sum" (Lam "n" $
          (If (App (App (Var "<=") (Var "n")) (int 0))
          (int 0)
          (App (App (Var "+") (Var "n"))
               (App (Var "sum") (App (App (Var "-") (Var "n")) (int 1))))))
      (App (Var "sum") (int 5))

zComb = Lam "f" $ 
        (Lam "x" $ Var "f" `App` Lam "v" (Var "x" `App` Var "x" `App` Var "v"))
  `App` (Lam "x" $ Var "f" `App` Lam "v" (Var "x" `App` Var "x" `App` Var "v"))


{-
eval' :: Env -> Expr -> Result Value
eval' env (Val v) = return v
eval' env (Var x) = case get env x of
                         Just v -> Success v
                         Nothing -> Failure "Variable not found"
eval' env (If e1 e2 e3) = do
    Boolean b <- eval' env e1
    if b then eval' env e2
         else eval' env e3
-}
example =
  let evenOdd = ((\n -> (n == 0) || snd evenOdd (n - 1)), (\n -> if n == 0 then False else fst evenOdd (n - 1)))
  in fst evenOdd 42

