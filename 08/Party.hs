module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons person@(Emp { empFun = y } ) gl@(GL list fun) = GL (person : list) (y + fun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL e f) l'@(GL e' f') 
    | f > f'    = l
    | otherwise = l'