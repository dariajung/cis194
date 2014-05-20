data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- break this up into three parts:
-- foldTree, insertNode, and height (of tree)
foldTree :: [a] -> Tree a

