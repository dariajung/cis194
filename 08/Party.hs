module Party where

import Employee
import Data.Tree
import Data.List (foldl, sort)
import Data.Monoid

import System.IO  
import Control.Monad

import Data.Char(toUpper)

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

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tree = f (rootLabel tree) (map (treeFold f) (subForest tree))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss list =
    let withBoss = boss `glCons` with list
        withoutBoss = without list
    in (withBoss, withoutBoss)

with :: [(GuestList, GuestList)] -> GuestList
with ((l, l'):xs) = l' `mappend` with xs
with [] = GL [] 0

without :: [(GuestList, GuestList)] -> GuestList
without ((l, l'):xs) = (moreFun l l') `mappend` without xs
without [] = GL [] 0

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

showGL :: GuestList -> IO ()
showGL (GL employees fun) = putStrLn ("Total fun: " ++ show fun) >>
                            mapM_ putStrLn (sort $ map empName employees)

main :: IO ()
main = readFile "company.txt" >>= (showGL . maxFun . read)

