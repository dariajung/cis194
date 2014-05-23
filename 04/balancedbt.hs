data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- break this up into three parts:
-- foldTree, insertNode, and height (of tree)
-- foldTree :: [a] -> Tree a

insert a (Node n left current right)
    | h1 > h2   = Node n left current (insert a right)
    | h1 < h2   = Node n (insert a left) current right
    | otherwise = Node (h + 1) (insert a left) current right
    where h1 = height left
          h2 = height right
          h = height (insert a left)

height :: Tree a -> Integer
height Leaf = 0
height (Node n left current right) = n

