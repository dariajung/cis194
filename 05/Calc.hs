module Calc where

import ExprT
import Parser

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe (Just (Lit x)) = Just x
evalMaybe (Just (Add x y)) = (Just (eval x + eval y))
evalMaybe (Just (Mul x y)) = (Just (eval x * eval y))
evalMaybe Nothing = Nothing

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr str = evalMaybe (parseExp Lit Add Mul str)

reify :: ExprT -> ExprT
reify = id

class Expr a where 
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where 
    lit x = Lit x
    add x y = Add x y
    mul x y = Mul x y

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = if x >= 0 then True else False
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    add a@(MinMax x) b@(MinMax y) = if max x y == x then a else b
    mul a@(MinMax x) b@(MinMax y) = if min x y == x then a else b

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

