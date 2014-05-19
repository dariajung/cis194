data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
