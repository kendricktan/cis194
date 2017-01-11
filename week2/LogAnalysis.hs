{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.List
import Data.Char
import Log

filterNonNumbers :: [String] -> [String]
filterNonNumbers = filter (all isDigit)

filterNumbers :: [String] -> [String]
filterNumbers = filter (all isLetter)

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
        rs = (drop 1 $ filterNumbers s)
        

parseMessage :: String -> LogMessage
parseMessage s = parseLogMessage pmt wl
  where wl = words s
        pmt = parseMessageType wl

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)