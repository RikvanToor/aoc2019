module Days.Day10 where

import Data.List
import Utils.V2


-- | Takes the field of asteroids and returns the coordinates of
-- all asteroids.
--
-- Important! This function assumes the input is a rectangle,
-- i.e. every line in the input has the same lenght.
parseInput :: String -> [V2 Int]
parseInput input = let ls     = lines input
                       height = length ls
                       width  = length (head ls)
                       ls'    = zip [0..] ls
                       indices = concatMap (uncurry (\y -> fmap (`V2` y)) . fmap (elemIndices '#')) ls'
                   in indices

-- | Checks wheter asteroid Y is in the line of sight of asteroid X, given
-- a list of asteroids that potentially block the line of sight.
isInLineOfSight :: V2 Int  -- Asteroid X
                -> V2 Int  -- Asteroid Y
                -> [V2 Int] -- The asteroid field
                -> Bool
isInLineOfSight c1 c2 cs | c1 == c2  = False
                         | otherwise = 
    let V2 dx dy   = c2 - c1
        fac        = gcd dx dy
        delta      = (`div` fac) <$> V2 dx dy
        -- All points that are in between asteroids X and Y
        points = map (\p -> c1 + fromIntegral p * delta) [1..(fac-1)]
    -- Y is visible from X if none of the points in between
    -- are obstructed.
    in all (not . (`elem` cs)) points

-- | Finds all asteroids in a field that are in sight from a given
-- location.
allAsteroidsInSight :: V2 Int -> [V2 Int] -> [V2 Int]
allAsteroidsInSight c cs = filter (\y -> isInLineOfSight c y cs) cs

-- | Finds the asteroid from which the most other asteroids are visible.
findBestLocation :: [V2 Int] -> V2 Int
findBestLocation cs = let f = length . (`allAsteroidsInSight` cs)
                      in maximumBy (\x y -> f x `compare` f y) cs


laserOrder :: V2 Int -> [V2 Int] -> [V2 Int] -> Double -> [V2 Int]
laserOrder p [] order _     = order
laserOrder p cs order angle = let visible   = map (\x -> (x, calcAngle p x)) $ allAsteroidsInSight p cs
                                  ordered'  = sortBy (\x y -> snd x `compare` snd y) visible
                                  ordered   = map fst ordered'
                                  newcs     = cs \\ ordered
                                  newangle  = 0
                              in laserOrder p newcs (order ++ ordered) newangle

-- | Calculates the line between two points as compared to a horizontal line.
calcAngle :: V2 Int -> V2 Int -> Double
calcAngle p1 p2 = let ang = unangle' . perp $ p2 - p1
                  in if ang < 0 then 2 * pi + ang else ang

test = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##"
startPos = V2 8 3

day10 :: String -> IO ()
day10 input = do
    let field = parseInput input
        point = findBestLocation field
    putStrLn "Answer 1:"
    print $ length $ (`allAsteroidsInSight` field) point
    putStrLn "Answer 2:"
    let order = laserOrder point (filter (/= point) field) [] 0
        V2 x y = order !! 199
    print (x * 100 + y)
