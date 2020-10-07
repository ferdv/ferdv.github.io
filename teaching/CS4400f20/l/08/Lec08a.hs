module Lec08a where

import SimpleTests (test, testError, testErrorMsg)

-- funcions as values

-- <SAE> ::= <Number>
--         | (+ <SAE> <SAE>)
--         | (- <SAE> <SAE>)
--         | (* <SAE> <SAE>)
--         | (/ <SAE> <SAE>)
--         | (<SAE> <SAE>)
--         | (fun (<Variable) <SAE>) -- example : (fun (x) x)
--         | (let (<Variable> <SAE>) <SAE>)
--         | <Variable>

type Variable = String

-- |Abstract syntax tree for SAE
data SAE = Value Value           -- NEW
         | Add SAE SAE
         | Sub SAE SAE
         | Mul SAE SAE
         | Div SAE SAE
         | App SAE SAE
         | Fun Variable SAE      -- NEW
         | Let Variable SAE SAE  -- NEW
         | Var Variable
         deriving (Eq, Show)

-- |Datatype for values aka evaluation results.
data Value = Number Integer
           | FunVal Variable SAE
           deriving (Show, Eq)

-- |To make our lives easier, we introduce abbreviations:
number :: Integer -> SAE
number i = Value (Number i)

-- Examples
ex_sae1, ex_sae2 :: SAE
-- (- (+ 1 2) 3)
ex_sae1 = Sub (Add (number 1) (number 2)) (number 3)
-- (/ 44 (+ (* 2 5) 1))
ex_sae2 = Div (number 44) (Add (Mul (number 2) (number 5)) (number 1))
-- (let (y (- 20 8)) (let (x (+ y 4)) (+ x 1)))
ex_sae3 = Let "y" (Sub (number 20) (number 8)) 
            (Let "x" (Add (Var "y") (number 4)) 
              (Add (Var "x") (number 1)))
ex_sae4 = Fun "x" (Var "x")
ex_sae5 = Let "add1" (Fun "x" (Add (Var "x") (number 1))) (App (Var "add1") (number 3)) -- (let (add1 (fun (x) (+ x 1))) (add1 3))




--(let (x 42) ((fun (x) x) x))

{- Abstracting "case of case of" for operations
 - 
 - Here, we basically separate control flow from applying the actual operation.
 - This is the pattern we are capturing here:
 -
  case ... e1 ... of
       Just v1 -> 
           case ... e2 ... of
                Just v2 ->
                    ... v1 ... v2 ...
                Nothing -> Nothing
       Nothing -> Nothing
-}

-- |"Lift" a (partial) binary operation.
apply2 :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
apply2 f Nothing _ = Nothing
apply2 f _ Nothing = Nothing
apply2 f (Just v1) (Just v2) = f v1 v2

-- |Apply a given arithmetic operation to a value. Fail if the value
-- types don't match.
applyArith :: (Integer -> Integer -> Integer) -> Value -> Value -> Maybe Value
applyArith f (Number n1) (Number n2) = Just (Number (f n1 n2))
applyArith f _ _ = Nothing


-- |Evaluates the given SAE, computing the corresponding integer.
eval :: SAE -> Maybe Value
eval (Value v) = Just v
eval (Add e1 e2) = apply2 (applyArith (+)) (eval e1) (eval e2)
eval (Sub e1 e2) = apply2 (applyArith (-)) (eval e1) (eval e2)
eval (Mul e1 e2) = apply2 (applyArith (*)) (eval e1) (eval e2)
eval (Div e1 e2) = apply2 (applyArith div) (eval e1) (eval e2)
eval (App e1 e2) = 
    case eval e1 of
         Just (FunVal x e) -> 
            case eval e2 of 
                 Just v2 -> eval (subst x v2 e) -- eval (Let x (Value v2) e)
                 Nothing -> Nothing
         _ -> Nothing
eval (Fun x e) = Just (FunVal x e)
eval (Let x e1 e2) = 
    case eval e1 of
         Just v1 -> eval (subst x v1 e2)
         Nothing -> Nothing
eval (Var x) = Nothing

-- |substitution: replace any matching variable with the given value
subst :: Variable -> Value -> SAE -> SAE                   -- in "formal notation":
subst x v (Value w) = Value w                              -- n[x := v] = n
subst x v (Var y) | x == y = Value v                       -- x[x := v] = v
                  | otherwise = Var y                      -- y[x := v] = y if x /= y
subst x v (Add e1 e2) = Add (subst x v e1) (subst x v e2)  -- (? e1 e2)[x := v]
subst x v (Sub e1 e2) = Sub (subst x v e1) (subst x v e2)  --   = (? e1[x := v] e2[x:= v))
subst x v (Mul e1 e2) = Mul (subst x v e1) (subst x v e2)  --   ... for ? in {+, -, * /}
subst x v (Div e1 e2) = Div (subst x v e1) (subst x v e2)
subst x v (App e1 e2) = App (subst x v e1) (subst x v e2)
subst x v (Fun y e) | x == y = Fun y e
                    | otherwise = Fun y (subst x v e)
subst x v (Let y e1 e2)                                    -- (let (x e1) e2)[x := v] 
    | y == x = Let y (subst x v e1) e2                     --   = (let (x e1) e1[x := v])
    | otherwise = Let y (subst x v e1) (subst x v e2)      -- (let (y e1) e2)[x := v]
                                                           --   = (let (y e1[x := v]) e2[x := v])
                                                           --   ... if x /= y

test_eval = do
    test "ex_sae1" (eval ex_sae1) (Just $ Number 0)
    test "ex_sae2" (eval ex_sae2) (Just $ Number 4)
    test "ex_sae3" (eval ex_sae3) (Just $ Number 17)
    test "ex_sae4" (eval ex_sae4) (Just $ FunVal "x" (Var "x"))
    test "ex_sae3" (eval ex_sae5) (Just $ Number 4)

main = test_eval


