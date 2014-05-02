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

parse :: String -> [LogMessage]
parse text  =
    let errors = lines text
    in map (parseMessage) errors

-- recursive insertion of BST
insert :: LogMessage -> MessageTree -> MessageTree
-- In case of Unknown error, return original tree
insert (Unknown _) tree = tree
-- If the LogMessage being inserted is the first,
-- make it the root and its left and right trees Leaves
insert log@(LogMessage _ _ _) (Leaf) = Node Leaf log Leaf  
