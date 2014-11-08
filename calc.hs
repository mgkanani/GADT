--Example of function overloading using GADT

{-# Language GADTs #-}
data Expr a where
        L :: [a] -> Expr [a]
        N :: Num a=> a -> Expr a
        S :: String -> Expr String
        B :: Bool -> Expr Bool
	G :: a -> Expr a

eval :: Expr a -> a
eval (N n) = n
eval (S s) = s
eval (B b) = b

add :: Expr a -> Expr a -> Expr a
eq :: Eq a => Expr a -> Expr a -> Expr Bool
len :: Expr [a] -> Expr Int

add (N a) (N b) = N (a + b)
add (S s1) (S s2) = S (s1 ++ s2)
and (B a) (B b) = B (a && b)
eq (G a) (G b) = B (a==b)
eq (B a) (B b) = B (a==b)
eq (L a) (L b) = B (a==b)
eq (N a) (N b) = B (a==b)
eq (S a) (S b) = B (a==b)
len (L l) = N (length l)

exp1=let
        e1=(S "hello")
        e2=(S "hi")
        e3=(N 1)
        x= (add e1 e2)
        in eval x
exp2=eval (add (N 10) (S "1"))

--exp3::(Expr_Class a)=>[a]
--exp3 = [(S "hi"),(N 10)]

{-
This example illustrates the use of GADT. If we have ADT instead of GADT then we have to write the code for handling ill-expressions in every definition of above functions.
Use of ADT instead of GADT will have reduced the readability of our code and also have introduced the runtime overhead.
Without GADT, we can not take the advantage of Haskell's static-type checking in Generic Programming.
-}
