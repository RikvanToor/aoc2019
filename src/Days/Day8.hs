module Days.Day8 where

import Data.List
import Utils

data Color = Black
           | White
           | Transparent
    deriving Eq

instance Show Color where
    show Black       = " "
    show White       = "■"
    show Transparent = "X"

-- | Parses a string to a list of colors using 'charToColor'.
parseInput :: String -> [Color]
parseInput = map charToColor

charToColor :: Char -> Color
charToColor '0' = Black
charToColor '1' = White
charToColor '2' = Transparent
charToColor _   = error "Unknown color"

-- | Splits a list of elements into sublists of n elements.
getChunks :: Int -> [a] -> [[a]]
getChunks _ [] = []
getChunks i xs = let (first, second) = splitAt i xs
                 in first : getChunks i second

-- | Counts the number of times a given element appears in a list
count :: Eq a => a -> [a] -> Int
count e = length . filter (== e)

-- | Traverses layers of colors, and returns the layer with the lowest
-- number of 'Black' vlues.
getFewestBlackLayer :: [[Color]] -> [Color]
getFewestBlackLayer = let cb = count Black
                      in minimumBy (\x y -> cb x `compare` cb y)

-- | Combines layers of images into one single image by combining
-- overlapping colors on different layers. 'Transparent' pixels
-- get overwritten, other colors do not.
decodeImage :: [[Color]] -> [Color]
decodeImage = foldr1 (zipWith combine)
    where combine Transparent c = c
          combine c _           = c

-- | Turns a layer into a string so it can be printed.
-- Needs the width of the image.
showImage :: Int -> [Color] -> String
showImage = unlines ∘∘ map (concatMap show) ∘∘ getChunks

width, height :: Int
width  = 25
height = 6

day8 :: String -> IO ()
day8 input = do
    let layers      = getChunks (width * height) $ parseInput input
    putStrLn "Answer 1:"
    let fewestBlack = getFewestBlackLayer layers
    print (count White fewestBlack * count Transparent fewestBlack)
    putStrLn "Answer 2:"
    let image = decodeImage layers
    putStrLn (showImage width image)