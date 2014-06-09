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