{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid

import Editor
import Buffer
import Scrabble
import Sized

-- Data Structures --
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

-- Exercise 1 --
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m

jlbToList :: JoinList m a -> [a]
jlbToList Empty            = []
jlbToList (Single m a)     = [a]
jlbToList (Append m l1 l2) = jlbToList l1 ++ jlbToList l2

-- Exercise 2 --
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i l@(Append m l1 l2)
  | i < 0 || i > size0 = Nothing
  | i < size1          = indexJ i l1
  | otherwise          = indexJ (i - size1) l2
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _        = Nothing
(!!?) _ i | i < 0 = Nothing
(!!?) (x:xs) 0    = Just x
(!!?) (x:xs) i    = xs !!? (i-1)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i l@(Single _ _)
  | i <= 0 = l
dropJ i l@(Append m l1 l2)
  | i >= centerSize = Empty
  | i < leftSize = dropJ i l1 +++ l2
  | i > 0 = dropJ (i-leftSize) l2
  | otherwise = l
      where centerSize = getSize . size $ m
            leftSize = getSize . size . tag $ l1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 1 l1@(Single _ _) = l1
takeJ i _ | i < 1 = Empty
takeJ i l@(Append m l1 l2)
  | i >= centerSize     = l
  | i <= leftSize       = takeJ i l1
  | i > leftSize        = l1 +++ takeJ (i-leftSize) l2
    where leftSize = getSize . size . tag $ l1
          centerSize = getSize . size $ m
takeJ _ _ = Empty
