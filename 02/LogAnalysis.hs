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
insert msg@(LogMessage _ _ _) (Leaf) = Node Leaf msg Leaf  
insert msg@(LogMessage _ n _) (Node l current@(LogMessage _ x _) r) = case compare n x of
    GT  -> Node l current (insert msg r)
    _   -> Node (insert msg l) current r 
    
build :: [LogMessage] -> MessageTree    
build []        = Leaf
build (x:xs)    = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf)  = []
inOrder (Node l current r) = inOrder l ++ current : inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong messages  = filterErrors messages

filterErrors :: [LogMessage] -> [String]
filterErrors [] = []
filterErrors ((LogMessage (Error n) _ s):xs)
    | n > 49    = s : filterErrors xs 
    | otherwise = filterErrors xs
filterErrors ((LogMessage _ _ _): xs) = filterErrors xs
