module Golf where

import Data.List

{- Old version
-- Ex1
-- | @skips xs@ form a list of list with each element is a list that only take
-- the nth element based on the original list elements' index
-- Eg: skips "ABCD" == ["ABCD", "BD", "C", "D"]
--     skips [1,2,3,4] == [[1,2,3,4], [2,4], [3], [4]]
skips :: [a] -> [[a]]
skips = zipWith everynth [1..] . createList
-}

skips :: [a] -> [[a]]
skips xs = zipWith everynth [1..length xs] $ repeat xs

{- Old version
-- | @everynth n xs@ create a new list that only take the
-- @n@th element of the list @xs@
-- More info: http://stackoverflow.com/a/2028218
-- Eg: everynth 2 [1,2,3,4] == [2,4]
everynth :: Int -> [a] -> [a]
everynth n xs = case drop (n-1) xs of
              (y:ys) -> y : everynth n ys
              [] -> []
-}

{- New version, taken from
 - https://www.reddit.com/r/haskell/comments/1cj8lc/take_every_nth_element_from_list_without_recursion/
 -}
everynth :: Int -> [a] -> [a]
everynth n = map snd . filter ((==n) . fst) . zip (cycle [1..n])

{- Other interesting functions, not tested though
 - http://codereview.stackexchange.com/questions/105100/upenn-cis-194-homework-3-code-golf
 - skips' n root@(_:xs) = [ x | (x,k) <- zip root [0..], mod k n == 0]
 -}

{- Old version
-- | @createList xs@ create a list of list with the same
-- length as the orignal list @xs@ and each element is the list @xs@
-- Eg: createList "ABCD" == ["ABCD", "ABCD", "ABCD", "ABCD"]
--     createList [1,2] == [[1,2], [1,2]]
createList :: [a] -> [[a]]
createList xs = replicate (length xs) xs
-}

{- A very clever solution from bschwb, utilizing list comprehension
 - https://github.com/bschwb/cis194-solutions/tree/master/03-rec-poly
 - skips :: [a] -> [[a]]
 - skips lst = [each i lst | i <- [1..length lst]]
 - each :: Int -> [a] -> [a]
 - each n lst = [lst !! i | i <- [n-1, n-1+n..length lst - 1]]
 -}

-- Ex2
-- | @localMaxima xs@ create a list of local maximum
-- Eg: localMaxima [2,9,5,6,1] == [9,6]
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x <= y && y >= z  = y : localMaxima (y:z:zs)
  | otherwise         = localMaxima (y:z:zs)
localMaxima _ = []

-- Ex3
-- This whole answer is taken from
-- https://github.com/bschwb/cis194-solutions/tree/master/03-rec-poly
-- use putStr (histogram [3,5]) to show this output in ghci
histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

-- returns one * line from the above function
line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

-- counts occurence of numbers in [0..9] in the input list.
count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]
