{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--parseMessage :: String -> LogMessage
parseMessage msg =
    let msgType = getMsgType $ words msg
    in words msg 

getMsgType :: [String] -> MessageType
getMsgType (x:y:zs)
    | x == "I"  = Info
    | x == "W"  = Warning
    | x == "E"  = Error (read y :: Int)
