module Calc where

import ExprT
import Parser

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe (Just expression@(Lit x)) = Just x
evalMaybe (Just expression@(Add x y)) = (Just (eval x + eval y))
evalMaybe (Just expression@(Mul x y)) = (Just (eval x * eval y))
evalMaybe Nothing = Nothing

eval :: ExprT -> Integer
eval expression@(Lit x) = x
eval expression@(Add x y) = eval x + eval y
eval expression@(Mul x y) = eval x * eval y


evalStr :: String -> Maybe Integer
evalStr str = evalMaybe (parseExp Lit Add Mul str)