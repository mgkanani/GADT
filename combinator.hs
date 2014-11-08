{-# Language GADTs #-}

--Example of combinator we have used in class CS-613(@IITB).
data Lam t where
  Lit :: a                     -> Lam a
  Tup  :: Lam a -> Lam b        -> Lam (a, b)
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b)
  App  :: Lam (a -> b) -> Lam a -> Lam b
  FixComb  :: Lam (a -> a)          -> Lam a

eval :: Lam t -> t
eval (Lit v)    = v
eval (Tup e1 e2) = (eval e1, eval e2)
eval (Lam f)     = \x -> eval (f (Lit x))
eval (App e1 e2) = (eval e1) (eval e2)
eval (FixComb f)     = (eval f) (eval (FixComb f))

fact = FixComb (Lam (\f -> Lam (\y -> Lit (if eval y == 0 then 1 else eval y * (eval f) (eval y - 1)))))
fib = FixComb (Lam (\f -> Lam (\y -> Lit (if eval y <= 1 then eval y else ((eval f) (eval y -2)) + (eval f) (eval y - 1)))))

exp1 = eval ( App fact (Lit 5))
exp2 = eval ( App fib (Lit 5))
