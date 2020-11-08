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
insert (Unknown _) messageTree = messageTree
insert message@(LogMessage _ timestamp _) messageTree =
  case messageTree of
    Leaf -> Node Leaf message Leaf
    Node _ (Unknown _) _ -> undefined
    Node leftTree cMessage@(LogMessage _ cTimestamp _) rightTree ->
        if cTimestamp > timestamp
           then (Node (insert message leftTree) cMessage rightTree)
           else (Node leftTree cMessage (insert message rightTree))

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree message rightTree) =
  (inOrder leftTree) ++ [message] ++ (inOrder rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getLogContent . filterByErrorSeverity (> 50) . inOrder . build
  where
    filterByErrorSeverity f =
      filter (\message ->
        case message of
          LogMessage (Error severity) _ _ -> f severity
          _ -> False
      )
    getLogContent (LogMessage _ _ content) = content
    getLogContent (Unknown content) = content

