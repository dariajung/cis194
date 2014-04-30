{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg
        | length first == 1     = getMsg $ words msg 
        | otherwise             = Unknown msg
    where first = head $ words msg

getMsg :: [String] -> LogMessage
getMsg (x:y:zs)
    | x == "I"  = LogMessage Info (read $ y :: Int) (unwords zs)
    | x == "W"  = LogMessage Warning (read $ y :: Int) (unwords zs)
    | x == "E"  = LogMessage (Error (read $ y :: Int)) (read $ head zs :: Int) (unwords $ drop 1 zs)

