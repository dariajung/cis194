module Party where

import Employee
import Data.Tree
import Data.List (foldl)
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons person@(Emp { empFun = y } ) gl@(GL list fun) = GL (person : list) (y + fun)

instance Monoid GuestList where 
  mempty = GL [] 0
  mappend (GL emp fun) (GL emp2 fun2) = GL (emp ++ emp2) (fun + fun2) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL e f) l'@(GL e' f') 
    | f > f'    = l
    | otherwise = l'


-- already defined in Data.Tree but inserting
-- so I can refer to the data type definition:

-- data Tree a = Node {
--      rootLabel :: a, -- label value
--      subForest :: [Tree a] -- zero or more child trees
-- }

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f accum (Node { rootLabel = x, subForest = y }) = foldl (treeFold f) (accum `f` x) y

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss list =
    let withBoss = boss `glCons` with list
        withoutBoss = without list
    in (withBoss, withoutBoss)

with ((l, l'):xs) = l' `mappend` with xs
without ((l, l'):xs) = (l `moreFun` l') `mappend` without xs