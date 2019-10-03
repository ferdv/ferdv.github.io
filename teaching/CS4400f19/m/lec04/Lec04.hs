-- LExpr reduction & reduction strategies


-- Pure lambda calculus
data LExpr = Var String          -- <LExpr> ::= <Var>
           | App LExpr LExpr     --           | <LExpr> <LExpr>
           | Lam String LExpr    --           | λ <Var> . <LExpr>
            deriving Show  

-- Substitution - only defined for terms that do not have clashing variables
-- I.e., not alpha-renaming is implemented
subst :: String -> LExpr -> LExpr -> LExpr
subst var s (Var var') | var == var' = s
                       | var /= var' = (Var var')
subst var s (App t1 t2) = App (subst var s t1) (subst var s t2)
subst var s (Lam var' t) | var == var' = (Lam var' t)
                         | not (free var' s) = Lam var' (subst var s t)

-- free variable predicate: check if the given variable is free in the given
-- term
free :: String -> LExpr -> Bool
free var (Var var') | var == var' = True
                    | otherwise = False -- free "x" (Var "y") 
free var (App t1 t2) = free var t1 || free var t2
free var (Lam var' t) | var == var' = False
                      | otherwise = free var t

-- Beta-reduction - only defined on an actual redex, otherwise fails
-- Suggestion: reimplement this with a Maybe LExpr return type
reduce :: LExpr -> LExpr
reduce (App (Lam var t1) t2) = subst var t2 t1
reduce _ = undefined

-- Reduction strategies

-- Each strategy either finds a redex, in which case it returns Just t, where
-- t is the updated term, or it returns Nothing if a redex could not be found.

-- Normal order reduction --
-- 
-- Reduce the leftmost outermost redex.
stepNormal :: LExpr -> Maybe LExpr
stepNormal (App (Lam var t1) t2) = Just (reduce (App (Lam var t1) t2))
stepNormal (App t1 t2) = 
  case stepNormal t1 of  -- first look in the LHS of the application
       Just t1' -> Just (App t1' t2)    -- success!
       Nothing -> case stepNormal t2 of -- then look in the RHS of the app.
                       Just t2' -> Just (App t1 t2') -- success!
                       Nothing -> Nothing            -- no redex here
stepNormal (Lam var t) = 
  case stepNormal t of -- normal reduction reduces under lambdas
       Just t' -> Just (Lam var t')
       Nothing -> Nothing
stepNormal (Var _) = Nothing -- nothing to reduce in a variable


-- Call-by-name reduction
--
-- Reduce the leftmost outermost redex, but
-- (a) don't reduce the RHS of an application
-- (b) don't reduce under lambdas
stepCBN :: LExpr -> Maybe LExpr
stepCBN (App (Lam var t1) t2) = Just (reduce (App (Lam var t1) t2))
stepCBN (App t1 t2) = 
  case stepCBN t1 of -- only attempt to reduce the LHS of an application
       Just t1' -> Just (App t1' t2)
       Nothing -> Nothing
stepCBN (Lam var t) = Nothing -- no reduction under lambdas
stepCBN (Var var) = Nothing

-- Call-by-value reduction (left to right)
--
-- Reduce the leftmost outermost redex, do not reduce under lambdas, reduce
-- RHS of applications.
stepCBV :: LExpr -> Maybe LExpr
stepCBV (App (Lam var t1) t2) = 
  case stepCBV t2 of -- try to reduce the RHS - if LHS already reduced
       Just t2' -> Just (App (Lam var t1) t2')
       Nothing -> Just (reduce (App (Lam var t1) t2)) -- if not reducible, beta-reduce the application
stepCBV (App t1 t2) =
  case stepCBV t1 of -- try reducing the LHS
       Just t1' -> Just (App t1' t2)
       Nothing -> Nothing 
stepCBV (Lam var t) = Nothing
stepCBV (Var var) = Nothing

-- "Evaluation" proceeds by iterating a reduction strategy until a normal form 
-- is reached.
eval :: (LExpr -> Maybe LExpr) -> LExpr -> LExpr
eval step t = 
  case step t of
       Just t' -> eval step t'   -- a redex was found and reduced
       Nothing -> t              -- no redex found - return the original term


evalNormal :: LExpr -> LExpr
evalNormal = eval stepNormal

evalCBN :: LExpr -> LExpr
evalCBN = eval stepCBN

evalCBV :: LExpr -> LExpr
evalCBV = eval stepCBV

-- example term: (λa. a) ((λb. b) (λd. (λc. c) d))
term = App (Lam "a" (Var "a"))
           (App (Lam "b" (Var "b"))
                (Lam "d" (App (Lam "c" (Var "c"))
                              (Var "d"))))


