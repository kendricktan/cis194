{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import Parser
import StackVM

-- Exercise 1 --
eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add i j) = eval i + eval j
eval (ExprT.Mul i j) = eval i * eval j

-- Exercise 2 --
evalStr :: String -> Maybe Integer
evalStr s = eval <$> n
    where n = Parser.parseExp ExprT.Lit ExprT.Add ExprT.Mul s

-- Exercise 3 --
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add (Lit a) (Lit b) = Lit ((+) a b)
    mul (Lit a) (Lit b) = Lit ((*) a b)

reify :: ExprT -> ExprT
reify = id

-- Exercise 4 --
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (0 <)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit a
      | a < 0 = Mod7 0
      | a > 6 = Mod7 6
      | otherwise = Mod7 a
    add (Mod7 a) (Mod7 b) = Mod7 $ (7 `mod`) $ (+) a b
    mul = add

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5 --
instance Expr StackVM.Program where
    lit i = [StackVM.PushI i]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

testStack   = testExp :: Maybe StackVM.Program

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul
