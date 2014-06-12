{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid

import Buffer
import JoinList
import Scrabble
import Sized

instance Buffer (JoinList (Score, Size) String) where
    toString Empty   = ""
    toString (Single _ x) = x
    toString (Append _ j j') = toString j ++ toString j'

    fromString ""     = Empty
    fromString str    = Single ((scoreString str), (Size $ length str)) str

    line n Empty = Nothing
    line n buff  = indexJ n buff

    -- replaceLine 

    numLines j = length $ toString j
    value j = scoreLine $ toString j

--  replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
--      where replaceLine' pre [] = pre
--            replaceLine' pre (_:ls) = pre ++ l:ls

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs