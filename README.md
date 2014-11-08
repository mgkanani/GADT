GADT(Generalized Algebraic DataTypes) In Haskell
====

There is not very much regarding GADT. GADT actually seems very simple once you understand how it is working. To understand it, It took much effort. I have tried to make it very simple to understand for naive in Haskell. Actully this article concluded based on many articles available on internet.

I have tried to explain GADT based on one example. Let's start it.

----
[![Algebraic Data Types](https://www.haskell.org/haskellwiki/Algebraic_data_type)](https://www.haskell.org/haskellwiki/Algebraic_data_type):
    ADTs are data types created using algebraic operations. In ADTs, there are 2 algebraic operations
possible. These operations are Sum and Product.

      Sum:It is represented as alternation. This means either of the data constructor might apply. i.e. A|B.
      Product:It is represented as combination. This means a combination of two terms. i.e. A B.

Examples:

      data Either a b = Left a | Right b
      data Btree a = Leaf a | Node (Btree a) (Btree a)

To understand the purpose of using GADT, let us consider the following example.
We define a expression data type for arithmetic operation as follows:

      Expr -> Int | Add Expr Expr | Mul Expr Expr
This can be done using following code in Haskell.

      data Expr = I Int -- integer constants
                | Add Expr Expr -- add two expressions
                | Mul Expr Expr -- multiply two expressions

Let us extend this to include an equality test operation.
For this, the data constructor will look like:

      data Expr = I Int
                | B Bool -- boolean constants
                | Add Expr Expr
                | Mul Expr Expr
                | Eq Expr Expr -- equality test

For evaluating the above expression, the return type of eval can be either Int or Bool. So, the type
signature of the evaluation function will be look like:

      eval :: Expr -> Maybe (Either Int Bool)
We can write the function for evaluation of the above data expression in Haskell as follows:

      data Expr = I Int
                | B Bool
                | Add Expr Expr
      eval :: Expr -> Maybe (Either Int Bool)
      eval (I a) = Just (Left a)
      eval (B a) = Just (Right a)
      eval (Add e1 e2) = case eval(e1) of
                            Nothing -> Nothing
                            Just a1 -> case a1 of
                                          Right b1 -> Nothing
                                          Left i1 -> case eval e2 of
                                                      Nothing -> Nothing
                                                      Just a2 -> case a2 of
                                                                  Right b2 -> Nothing
                                                                  Left i2 -> Just (Left (i1+i2))

In above code, we are manually handling ill-expressions like `(Add (B True) (I 5)) :: Expr`. What we
would desire is that Haskell's type checker itself should refuse to process this further like it does for
expression `True + 5`.
We do not want to check types ourselves while deconstructing AST(Abstract Syntax Tree). In other
words, we want to impose type safety of our code without having additional runtime overhead.

To achieve that goal, we can modify the data definition for Expr as below:

      data Expr a = I Int
                  | B Bool
                  | Add (Expr Int) (Expr Int)
                  | Mul (Expr Int) (Expr Int)
                  | Eq (Expr a) (Expr a)
This type of data definition uses a concept known as phantom type.
[![Phantom Types](http://en.wikibooks.org/wiki/Haskell/Phantom_types
)](http://en.wikibooks.org/wiki/Haskell/Phantom_types
) are the data types in which parameters on left hand side of `=` do not all appear in right hand side of the new data type definition. Expr is phantom type because variable 'a' is not appearing after '=' sign with all data
constructors. The dummy variable `a` in `Expr a` is used for tracking the type of expression.

Now, let reconsider the expression `x = Add (I 5) (B True)`. This still does not give any compile-time error.
Because, the type signature of `I:: Int -> Expr a` but not `I:: Int -> Expr Int`. Similarly for B data constructor, its type signature is `B::Bool -> Expr a` but not `B:: Bool -> Expr Bool`.
In expression `Add (I 5) (B True)`, `(B True)` will be treated as `Expr Int`.

For new data types, the main hurdle for making use of haskell's static type checking is ,we do not have controll to restrict the type of data constructor. In other words, we can not restrict the data constructor `I` to return `Expr Int` instead of `Expr a` using Phantom types.

[![Generalized Algebraic DataType(GADT)](http://en.wikibooks.org/wiki/Haskell/GADT)](http://en.wikibooks.org/wiki/Haskell/GADT) helps to resolve the issue of specifying return types of data constructor. GADT allows us to control the value of `a` for data constructors. Means we can restrict the return type of data constructors, `I` to return `Expr Int` and `B` to `Expr Bool`.
The modified GADT code will look like:

      {-# Language GADTs #-}
      data Expr a where
               I		:: Int -> Expr Int
               B		:: Bool -> Expr Bool
               Add	:: Expr Int -> Expr Int -> Expr Int
               Mul	:: Expr Int -> Expr Int -> Expr Int
               Eq	  :: Expr a -> Expr a -> Expr Bool
      eval:: Expr a -> a
      eval (I i) = i
      eval (B b) = b
      eval (Add e1 e2) = (eval e1) + (eval e2)
      eval (Mul e1 e2) = (eval e1) * (eval e2)
      
      exp1= eval (Mul ( Add (I 1) (I 4) ) ( I 2 ) )     -- will print 10.
      exp2= eval (Mul ( Add (I 1) (I 4) ) ( B True ) )  -- This will give error.

Benefits of GADT:-
    Static type checking for ADT :-  
      Ill-expression(invalid syntax tree)s will be handled using Haskell's type checker.
    Cleaner and readable code

Applications:-
    Implementing Domain Specific Languages with haskell's static type checking feature.
    Generic Programming.

Here is the example of Generic Programming:-

      {-# Language GADTs #-}
      data Expr a where
        N :: Num a=> a -> Expr a
        S :: String -> Expr String
        
        eval :: Expr a -> a
        eval (N n) = n
        eval (S s) = s
        
        add :: Expr a -> Expr a -> Expr a
        add (N a) (N b) = N (a + b)
        add (S s1) (S s2) = S (s1 ++ s2)
        
        exp1=eval (add (S "hi") (S "hello"))
        exp2=eval (add (N 10) (N 5.1))
