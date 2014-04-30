{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--parseMessage :: String -> LogMessage
parseMessage msg =
    let msgType = getMsgType $ words msg
    in msgType

getMsgType :: [String] -> MessageType
getMsgType (x:ys)
    | x == "I"  = Info
    | x == "W"  = Warning
    | x == "E"  = Error (read $ head ys :: Int)
