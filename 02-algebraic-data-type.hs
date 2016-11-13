data FailableDouble = Failure
                    | OK Double
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- If not use algebraic type
notSafeDiv :: Double -> Double -> Double
notSafeDiv x y
  | y /= 0 = x / y
  | otherwise = 0

