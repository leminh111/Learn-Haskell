-- Validating Credit Card Numbers
-- Ex1
toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = [x `mod` 10] ++ toDigitsRev (x `div` 10)
  | otherwise = []

-- Ex2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (a:b:c)
  | (length (a:b:c)) `mod` 2 /= 0 = [a, 2*b] ++ doubleEveryOther (c)
  | (length (a:b:c)) `mod` 2 == 0 = [2*a, b] ++ doubleEveryOther (c)

-- Ex3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 9 = sumDigits (toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs

-- Ex4
validate :: Integer -> Bool
validate x
  | x > 0 = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0
  | otherwise = False

-- Ex5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi x a b c
  | x > 0 = hanoi (x-1) a c b ++ hanoi 1 a b c ++ hanoi (x-1) c b a
  | otherwise = []

-- Ex6
hanoix :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoix 1 a b c d = [(a, b)]
hanoix x a b c d
  | x <= 0 = []
  | x < optimalN x = hanoix x a b c d
  | otherwise = hanoix (optimalN x) a c b d ++ hanoi (x - (optimalN x)) a b d ++ hanoix (optimalN x) c b a d

optimalN :: Integer -> Integer
optimalN n
  | n >= 0 = n - round (sqrt (fromIntegral(2*n + 1))) + 1
  | otherwise = 0

