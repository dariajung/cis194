module Calc where

import ExprT

eval :: ExprT -> Integer
eval expression@(Lit x) = x
eval expression@(Add x y) = eval x + eval y
eval expression@(Mul x y) = eval x * eval y