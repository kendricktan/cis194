module Party where

import Employee

-- Exercise 1 --
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp{empFun = ef}) (GL el f) = GL (el ++ [emp]) (ef + f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL e1 ef1) (GL e2 ef2) = GL (e1 ++ e2) (ef1 + ef2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b

-- Exercise 2 --
