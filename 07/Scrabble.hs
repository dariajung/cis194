{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score n 
    | n `elem` "aeiounlrstAEIOUNLRST"   = Score 1
    | n `elem` "dD"                     = Score 2
    | n `elem` "bcmpBCMP"               = Score 3
    | n `elem` "fvwyFVWY"               = Score 4
    | n `elem` "kK"                     = Score 5
    | n `elem` "jxJX"                   = Score 8
    | n `elem` "qzQZ"                   = Score 10
    | otherwise                         = Score 0

--scoreString :: String -> Score
--scoreString
