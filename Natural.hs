{-# Language GADTs #-}

--implementation of Natural number using GADT.
data Nat a where
        B:: Bool -> Nat Bool
        Zero :: Nat Int
        Succ :: Nat Int -> Nat Int
        Pred :: Nat Int -> Nat Int
        IsZero :: Nat Int -> Nat Bool
        Eq :: Eq a => Nat a -> Nat a -> Nat Bool
        If :: Nat Bool -> Nat a -> Nat a -> Nat a

eval :: Nat a -> a

eval (B b) = b
eval (Eq e1 e2) = (eval e1)==(eval e2)
eval (Zero) = 0
eval (Succ x) = (eval x) + 1
eval (Pred x) = (eval x) - 1
eval (IsZero x) = (eval x)==0
eval (If cnd e1 e2 ) = if (eval cnd) then (eval e1) else (eval e2)

nat1 = eval (If (IsZero (Pred (Succ Zero))) Zero (Succ Zero))
