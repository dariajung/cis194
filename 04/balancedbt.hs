data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\x tree -> insert x tree) Leaf

insert a Leaf = Node 0 Leaf a Leaf

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

