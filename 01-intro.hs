sumtorial :: Integer -> Integer
sumtorial 1 = 1
sumtorial 2 = 1
sumtorial n = sumtorial(n-2) + sumtorial(n-1)


changeSingle :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
changeSingle n a b c d
  | n <= 0 = [0]
  | n < c = d : changeSingle (n - d) a b c d
  | n < b = c : changeSingle (n - c) a b c d
  | n < a = b : changeSingle (n - b) a b c d
  | n >= a = a : changeSingle (n - a) a b c d


change :: Integer -> Integer -> Integer -> Integer -> Integer -> [[Integer]]
change n a b c d
  | n <= 0 = [[0]]
  | n < c = [d : changeSingle (n - d) a b c d]
  | n < b = [c : changeSingle (n - c) a b c d] ++ [(changeSingle c a b c d) ++ changeSingle (n - c) a b c d]
  | n < a = [b : changeSingle (n - b) a b c d] ++ [(changeSingle b a b c d) ++ changeSingle (n - b) a b c d]
  | n >= a = [a : changeSingle (n - a) a b c d] ++ [(changeSingle a a b c d) ++ changeSingle (n - a) a b c d]
