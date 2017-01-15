{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT as E
import Parser as P
import StackVM as S
import qualified Data.Map as M
import Data.Maybe

-- Exercise 1 --
eval :: ExprT -> Integer
eval (E.Lit a)   = a
eval (E.Add a b) = eval a + eval b
eval (E.Mul a b) = eval a * eval b

-- Exercise 2 --
evalStr :: String -> Maybe Integer
evalStr s = eval <$> es
    where es = P.parseExp E.Lit E.Add E.Mul s

-- Exercise 3 --
reify :: ExprT -> ExprT
reify = id

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = E.Lit
    add (E.Lit a) (E.Lit b) = E.Lit (a+b)
    mul (E.Lit a) (E.Lit b) = E.Lit (a*b)

-- Exercise 4 --
newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (0>)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit n
      | n > 6 = Mod7 6
      | n < 0 = Mod7 0
      | otherwise = Mod7 n

    add (Mod7 a) (Mod7 b) = Mod7 ((`mod` 7) $ (+) a b)
    mul = add

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5 --
instance Expr Program where
    lit n = [S.PushI n]
    add a b = a ++ b ++ [S.Add]
    mul a b = a ++ b ++ [S.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6 --
data VarExprT = VarExprT String Integer
type MapExpr = (M.Map String Integer -> Maybe Integer)

class HasVars a where
    var :: String -> a

instance HasVars VarExprT where
    var s = VarExprT s 0

instance Expr VarExprT where
    lit = VarExprT ""
    add (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a + b)
    mul (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a * b)

instance HasVars MapExpr where
    var = M.lookup

instance Expr MapExpr where
    lit a = (\_ -> Just a)
    add f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                      True -> Nothing
                      _    -> Just (fromJust (f m) + fromJust (g m))
    mul f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                      True -> Nothing
                      _    -> Just (fromJust (f m) * fromJust (g m))
        
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

