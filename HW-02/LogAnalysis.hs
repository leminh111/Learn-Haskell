{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Ex 1
--
-- | @extractWord n s@ extractWord the @n@th word in the string @s@
extractWord :: Int -> String -> String
extractWord x y
  | x > 0 = unwords (take 1 (drop (x-1) (words y)))
  | otherwise = ""

-- | @extractWords n s@ extractWords the @n@th words to the end
-- in the string @s@
extractWords :: Int -> String -> String
extractWords x y
  | x > 0 = unwords (drop (x-1) (words y))
  | otherwise = ""

-- | @parseMessage x@ parseMessage the string @x@ to return
-- the LogMessage, eg:
--
-- parseMessage "I 29 la la la"
--   == LogMessage Info 29 "la la la"
--
-- parseMessage "This is not in the right format"
--   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage x@('E':_) = LogMessage
                           (Error (read (extractWord 2 x)))
                           (read (extractWord 3 x))
                           (extractWords 4 x)
parseMessage x@('I':_) = LogMessage
                           (Info)
                           (read (extractWord 2 x))
                           (extractWords 3 x)
parseMessage x@('W':_) = LogMessage
                           (Warning)
                           (read (extractWord 2 x))
                           (extractWords 3 x)
parseMessage x = Unknown x

-- | @parse x@ parse the whole string @x@ in a file to return
-- a list of LogMessage, eg:
parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

-- Ex 2
--
-- | @insert x t@ insert a LogMessage @x@ into a binary tree @t@
-- and return a new sorted binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ x _) (Node s rlm@(LogMessage _ m _) l)
  | (x >= m) && (l == Leaf) = Node s rlm (Node Leaf lm Leaf)
  | (x < m) && (s == Leaf) = Node (Node Leaf lm Leaf) rlm l
  | (x >= m) && (l /= Leaf) = Node s rlm (insert lm l)
  | (x < m) && (s /= Leaf) = Node (insert lm s) rlm l

-- Ex 3
--
-- | @build x@ build a MessageTree based on @x@ list of LogMessage
-- and return a new sorted binary tree
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Ex 4
--
-- | @inOrder t@ build an ordered list of LogMessage
-- based on @t@ MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder t@(Node s lm l) = inOrder s ++ [lm] ++ inOrder l

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
whatWentWrongEach (LogMessage _ _ _) = ""
whatWentWrongEach (Unknown _) = ""

