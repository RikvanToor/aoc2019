module Days.Day1 where

-- | Divides the input by 3, rounds down and subtracts 2.
calculateFuel :: Int -> Int
calculateFuel = (+ (- 2)) . (`div` 3)

-- | Calls a function which calculates the required amount
-- of fuel, such as 'calculatefuel', on each item in a list
-- and sums the result.
calculateFuelSum :: (Int -> Int) -> [Int] -> Int
calculateFuelSum f = foldr ((+) . f) 0

-- | Calculates the amount of fuel, and keeps doing this
-- for the amount of fuel added each iteration.
calculateFuelRecursive :: Int -> Int
calculateFuelRecursive = sum
                       . takeWhile (>0)
                       . tail
                       . iterate calculateFuel

day1 :: String -> IO ()
day1 input = do
    let masses = read <$> lines input
    putStrLn "Answer 1:"
    print (calculateFuelSum calculateFuel masses)
    putStrLn "Asnwer 2:"
    print (calculateFuelSum calculateFuelRecursive masses)
    