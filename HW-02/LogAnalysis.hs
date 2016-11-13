{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Most of the answers is improved by using solutions from
-- https://gist.github.com/damncabbage/1a9817a348e8a4e522c0
-- Ex 1
-- | @parseMessage x@ parseMessage the string @x@ to return
-- the LogMessage, eg:
--
-- parseMessage "I 29 la la la"
--   == LogMessage Info 29 "la la la"
--
-- parseMessage "This is not in the right format"
--   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage msg = case (words msg) of
  ("I":time:string)      -> LogMessage Info    (read time) (unwords string)
  ("W":time:string)      -> LogMessage Warning (read time) (unwords string)
  ("E":code:time:string) -> LogMessage (Error (read code)) (read time) (unwords string)
  _                      -> Unknown msg

-- | @parse x@ parse the whole string @x@ in a file to return
-- a list of LogMessage, eg:
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Ex 2
--
-- | @insert x t@ insert a LogMessage @x@ into a binary tree @t@
-- and return a new sorted binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf = Node Leaf lm Leaf
insert msg (Node left node right)
  | (ts msg <= ts node) = Node (insert msg left) node right
  | otherwise           = Node left node (insert msg right)
  where
    ts (LogMessage _ t _) = t
    ts _                  = 0

-- Ex 3
--
-- | @build x@ build a MessageTree based on @x@ list of LogMessage
-- and return a new sorted binary tree
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Ex 4
--
-- | @inOrder t@ build an ordered list of LogMessage
-- based on @t@ MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node s lm l) = inOrder s ++ [lm] ++ inOrder l

-- Ex 5
--
-- | @whatWentWrong x@ build an ordered (by timestamp) list of String error with greater 50
-- severity from an unordered list of LogMessage @x@
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = filter (not . null) (map whatWentWrongEach ((inOrder . build) x))

-- | @whatWentWrongEach x@ build a String error with greater 50
-- severity from a LogMessage @x@
whatWentWrongEach :: LogMessage -> String
whatWentWrongEach (LogMessage (Error s) _ x)
  | s >= 50 = x
  | otherwise = ""
whatWentWrongEach _ = ""

{- Other Solutions:
 - From https://gist.github.com/damncabbage/1a9817a348e8a4e522c0
 - whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = (map message (filter important (inOrder (build xs))))
  where
    important (LogMessage (Error sev) _ _) = (sev >= 50)
    important _ = False
    message (LogMessage _ _ m) = m
    message (Unknown _) = ""
-}
