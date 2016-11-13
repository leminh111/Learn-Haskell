-- Validating Credit Card Numbers
-- Ex1
toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = toDigits (div x 10) ++ [mod x 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = (mod x 10) : toDigitsRev (div x 10)
  | otherwise = []

-- Ex2
{- Old version
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (a:b:c)
  | (length (a:b:c)) `mod` 2 /= 0 = [a, 2*b] ++ doubleEveryOther (c)
  | (length (a:b:c)) `mod` 2 == 0 = [2*a, b] ++ doubleEveryOther (c)
-}

{-
 - Some Explanation:
 - This function is based on the answer on StackOverFlow:
 - http://stackoverflow.com/questions/17383169/haskell-double-every-2nd-element-in-list
 -
 - The function is written in point-free style
 - First, it reverses the array of Integer
 - Then it runs the zipWith thingy
 - Lastly it reverses the array again
 -
 - The cycle thingy basically creates an infinite array of functions like:
 - [id, (*2), id, (*2), ...]
 - (*2) is the function double what it receives
 - id is the function that returns the param itself (identify)
 -
 - zipWith combines 2 arrays into 1
 - ($) is the function, the action to take care of this merging business, for example:
 - [id, (*2), ...] and [1, 2, 3, 4]
 - will be combine like:
 - [($) id 1, ($) (*2) 2, ($) id 3, ($) (*2) 4]
 - or
 - [id $ 1, (*2) $ 2, id $ 3, (*2) $ 4]
 - or
 - [id 1, 2 * 2, id 3, 2 * 4]
 -}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id,(*2)]) . reverse

-- Ex3
{- Old version
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 9 = sumDigits (toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs
-}

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- Ex4
{- Old version
validate :: Integer -> Bool
validate x
  | x > 0 = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0
  | otherwise = False
-}

validate :: Integer -> Bool
validate x
  | x > 0 = mod total 10 == 0
  | otherwise = False
  where total = sumDigits $ doubleEveryOther $ toDigits x

-- Ex5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi x a b c
  | x > 0 = firstHalfSteps ++ middleStep ++ lastHalfSteps
  | otherwise = []
  where firstHalfSteps  = hanoi (x-1) a c b
        middleStep      = hanoi 1 a b c
        lastHalfSteps   = hanoi (x-1) c b a

-- Ex6
hanoix :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoix 1 a b c d = [(a, b)]
hanoix x a b c d
  | x <= 0 = []
  | x < optimalN x = hanoix x a b c d
  | otherwise = firstHalfSteps ++ middleStep ++ lastHalfSteps
  where firstHalfSteps  = hanoix  (optimalN x)      a c b d
        middleStep      = hanoi   (x - optimalN x)  a b d
        lastHalfSteps   = hanoix  (optimalN x)      c b a d

optimalN :: Integer -> Integer
optimalN n
  | n >= 0 = (n + 1 -) $ round $ sqrt $ fromIntegral $ 2*n + 1
  | otherwise = 0

