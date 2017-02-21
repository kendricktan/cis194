{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
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
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f init (Node {rootLabel = rl, subForest = sf})
  = f rl (map (treeFold f init) sf)

-- Exercise 3 --
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b (x@((GL el1 f1), (GL el2 f2)):xs) = undefined
