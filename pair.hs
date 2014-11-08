{-# Language GADTs #-}

-- defining Pair like we have use in untyped lambda calculus. 
data Term a where
    Lit :: a -> Term a
    Pair  :: Term a -> Term b -> Term (a,b)
    Fst :: Term (a,b) -> Term a
    Snd :: Term (a,b) -> Term b

eval :: Term a -> a
eval (Lit x) = x
eval (Pair t1 t2) = (eval t1, eval t2)
eval (Fst t1) = fst (eval t1)
eval (Snd t1) = snd (eval t1)

pair1= eval (Pair (Lit 1) (Lit 'c')) -- will return (1,'c')
--uncomment to check below will fail to compile.
--pair2= eval (Fst (Lit 1)) -- will give compile time error. Since Fst requires the data of type Term (a,b) while 'Lit 1' is of type Term a.
