import Maps

data Stmt = Assign String Expr
          | Seq Stmt Stmt
          | Print Expr
          | While Expr Stmt
          deriving Show

data Expr = Val Value
          | Var String
          | Add Expr Expr
          | Le Expr Expr
          deriving Show

data Value = Num Integer
           | Bool Bool
           deriving Show

type Store = Map String Value

type Out = [Value]

evalExpr :: Store -> Expr -> Maybe Value
evalExpr sto (Val v) = Just v
evalExpr sto (Var x) = get x sto
evalExpr sto (Add e1 e2) =
  case evalExpr sto e1 of
       Just (Num n1) -> case evalExpr sto e2 of
                             Just (Num n2) -> Just (Num (n1 + n2))
                             _ -> Nothing
       _ -> Nothing
evalExpr sto (Le e1 e2) =
  case evalExpr sto e1 of
       Just (Num n1) -> case evalExpr sto e2 of
                             Just (Num n2) -> Just (Bool (n1 <= n2))
                             _ -> Nothing
       _ -> Nothing

execStmt :: (Store, Stmt) -> Maybe (Store, Out)
execStmt (sto, Assign x e) = 
  case evalExpr sto e of
       Just v -> Just (add x v sto, [])
       Nothing -> Nothing
execStmt (sto, Seq c1 c2) = 
  case execStmt (sto, c1) of
       Just (sto', out1) -> 
         case execStmt (sto', c2) of
              Just (sto'', out2) -> Just (sto'', out1 ++ out2)
              Nothing -> Nothing
       Nothing -> Nothing
execStmt (sto, Print e) =
  case evalExpr sto e of
       Just v -> Just (sto, [v])
       Nothing -> Nothing
execStmt (sto, While e c) =
  case evalExpr sto e of
       Just (Bool False) -> Just (sto, [])
       Just (Bool True) -> 
         case execStmt (sto, c) of
              Just (sto', out1) ->
                case execStmt (sto', While e c) of
                     Just (sto'', out2) -> Just (sto'', out1 ++ out2)
                     _ -> Nothing
              _ -> Nothing
       _ -> Nothing


-- print numbers 0 to 10
example1 = 
  Assign "x" (Val (Num 0)) `Seq`
  While (Le (Var "x") (Val (Num 10))) (
    Print (Var "x") `Seq`
    Assign "x" (Add (Var "x") (Val (Num 1)))
  )

-- sum up numbers from 1 to 10 and print the result
example2 = 
  Assign "x" (Val (Num 1)) `Seq`
  Assign "sum" (Val (Num 0)) `Seq`
  While (Le (Var "x") (Val (Num 10))) (
    Assign "sum" (Add (Var "sum") (Var "x")) `Seq`
    Assign "x" (Add (Var "x") (Val (Num 1)))
  ) `Seq`
  Print (Var "sum")

exec :: Stmt -> IO ()
exec s = do
  let (Just (sto, out)) = execStmt (empty, s)
  putStrLn (unlines (map show out))
