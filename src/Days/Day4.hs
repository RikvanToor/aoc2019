module Days.Day4 where

import Data.List (nub, (\\))

-- | Converts an integer to a list of its digits.
--
-- >>> toDigits 12345
-- [1,2,3,4,5]
toDigits :: Int -> [Int]
toDigits = map (read . pure) . show

-- | Checks if any list of elements contain any duplicate elements
--
-- >>> hasRepeat [1,2,3]
-- False
--
-- >>> hasRepeat [1,2,2]
-- True
hasRepeat :: Eq a => [a] -> Bool
hasRepeat xs = nub xs /= xs

-- | Checks if a list of elements contains any decreasing elements.
--
-- >>> decreases [1,2,3,4,5]
-- False
--
-- >>> decreases [1,3,2,4,5]
-- True
decreases :: Ord a => [a] -> Bool
decreases (x:y:ys) = x > y || decreases (y:ys)
decreases _        = False

-- | Checks if a password is correct. It is correct when
--
--    * The digits in a password never decrease
--    * At least one digit repeats at least once
isCorrect :: Int -> Bool
isCorrect i = let digits = toDigits i
                  dec = snd $ foldr (\x (y, b) -> (x, y >= x && b)) (10, True) digits
              in hasRepeat digits && not (decreases digits)

-- | Checks if a password has at least one pair of repeating digits
-- that only repeats once.
--
-- >>> isCorrecter 111111
-- False
--
-- >>> isCorrecter 111122
-- True
isCorrecter :: Int -> Bool
isCorrecter i = let digits = toDigits i
                in hasSingleRepeat digits

-- | Checks if there is an element in the list that repeats, but only once.
hasSingleRepeat :: [Int] -> Bool
hasSingleRepeat (x:y:ys) = x == y && notElem x ys
                          || hasSingleRepeat (dropWhile (== x) (y:ys))
hasSingleRepeat _          = False


day4 :: String -> IO ()
day4 input = do
    let minV    = read (take 6 input)
        maxV    = read (drop 7 input)
        range   = [minV..maxV]
        correct = filter isCorrect range
    putStrLn "Answer 1:"
    print (length correct)

    let correcter = filter isCorrecter correct
    putStrLn "Answer 2:"
    print (length correcter)