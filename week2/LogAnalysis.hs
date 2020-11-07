{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Control.Applicative (liftA2)
import Data.Maybe          (fromMaybe)
import Text.Read           (readMaybe)
import Log

parseMessage :: String -> LogMessage
parseMessage message = fromMaybe (Unknown message) (parseMessage' $ words message)
  where
    parseMessage' ("I" : x : xs) = fmap (\t -> LogMessage Info t (unwords xs)) (readMaybe x)
    parseMessage' ("W" : x : xs) = fmap (\t -> LogMessage Warning t (unwords xs)) (readMaybe x)
    parseMessage' ("E" : x : x' : xs) =
        liftA2 (\e t -> LogMessage (Error e) t (unwords xs)) (readMaybe x) (readMaybe x')
    parseMessage' _ = Nothing

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined
