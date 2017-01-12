{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.List
import Data.Char
import Log

-- Exercise 1 --
-- Convert raw inputs to LogMessage type
filterNonNumbers :: [String] -> [String]
filterNonNumbers = filter (all isDigit)

parseMessageType :: [String] -> Maybe MessageType
parseMessageType s
  | nc == 'e' = Just (Error x)
  | nc == 'i' = Just Info
  | nc == 'w' = Just Warning
  | otherwise = Nothing
  where nc = (toLower . head . head) s
        x  = read (s !! 1) :: Int

parseLogMessage :: Maybe MessageType -> [String] -> LogMessage
parseLogMessage Nothing  s = Unknown (unwords s)
parseLogMessage (Just a) s = LogMessage a (read ts :: Int) (unwords rs)
  where nn = filterNonNumbers s -- Non Numbers
        ts = (head . reverse) nn   -- Time Stamp
        rs = if (toLower . head . head) s == 'e'
             then drop 3 s
             else drop 2 s

parseMessage :: String -> LogMessage
parseMessage s = parseLogMessage pmt wl
  where wl = words s
        pmt = parseMessageType wl

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Exercise 2 --
logMsgTimeStamp :: LogMessage -> Maybe Int
logMsgTimeStamp (Unknown _)        = Nothing
logMsgTimeStamp (LogMessage _ t _) = Just t

insert :: LogMessage -> MessageTree -> MessageTree
insert l1 Leaf            = Node Leaf l1 Leaf
insert l1 (Node m1 l2 m2)
  | b == a = Node m1 l2 m2
  | b < a  = Node (LogAnalysis.insert l1 m1) l2 m2
  | b > a  = Node m1 l2 (LogAnalysis.insert l1 m2)
  where b = logMsgTimeStamp l1
        a = logMsgTimeStamp l2

-- Exercise 3 --
build :: [LogMessage] -> MessageTree
build = foldr LogAnalysis.insert Leaf

-- Exercise 4 --
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf          = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

-- Exercise 5 --
isLogErrorSevere :: LogMessage -> Bool
isLogErrorSevere (LogMessage (Error n) _ _) = n >= 50
isLogErrorSevere _                  = False

logMsgString :: LogMessage -> String
logMsgString (LogMessage _ _ m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = map logMsgString $ filter isLogErrorSevere l