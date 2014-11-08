{-# Language GADTs #-}
import Data.Char
import System.IO

data Dt = D
data Ch = C
data Str = S

data PrintData a where
	PD :: (Dt,Int) -> PrintData String
	PC :: (Ch,Char) -> PrintData String
	PS :: (Str,String) -> PrintData String

eval:: PrintData a->String
eval (PD a) = show (snd a)
eval (PC a) = [(snd a)]
eval (PS a) = (snd a)

printstr::String->[PrintData a]->String

printstr [] _ = []
printstr ('#':'@':'#':xs) (e1:cs) = eval e1 ++ printstr xs cs
printstr ('#':'@':'#':xs) [] = "Insufficient arguments."
printstr (x:xs) e1 = x:(printstr xs e1)

printf s dt = do
		   putStrLn (printstr s dt)

x = printf "There are #@# students in #@# division of #@# department." [(PD (D,2)),(PC (C,'A')),(PS (S,"Computer Science"))]

{-
Usage:

	printf <String containing placeholder as "#@#" for variables> <List of PrintData variables>


Explanation:

	The main purpose of using tuple for specifying variables is to have a mechanism, in which we can correlate the argument type( %d,%c ,etc.) with type of value actually passed at compile time. 
	To utilize static type checking of GADT, we have used a pair (type,value), which will allow us to decide on compile time, whether the arguments are of matching types.
	The reason why we have not used delimiters like %d within the string is because we need to know, at compile time, what the expected type of the variables must be. String evaluation for determining expected type cannot be done on compile time.
	For Eg.: printf "... %d ..." (PD 2). 
	-> Here, to match %d and (PD 2) at compile time is not possible. This is because %d is within a String and String has to be evaluated.

	In Haskell, for List data structure, there is no mechanism so that at compile time, we are able to find the length of list. So, matching the number of arguments is not possible at compile time.
-}
