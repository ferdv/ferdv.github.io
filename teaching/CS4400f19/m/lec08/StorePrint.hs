
import Maps

data Stmt = Assign String Expr
          | Seq Stmt Stmt
          | Print Expr
          deriving Show

data Expr = Val Value
          | Var String
          | Add Expr Expr
          deriving Show

data Value = Num Integer
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

example1 = 
  Print (Val (Num 10)) `Seq`
  Assign "x" (Val (Num 20)) `Seq`
  Print (Var "x") `Seq`
  Print (Add (Var "x") (Var "x"))

