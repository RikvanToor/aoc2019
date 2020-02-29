module Days.Day3 where

import Utils
import Utils.V2
import qualified Data.Set as S
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Dir = V2 Int
type Pos = V2 Int
type Path = [Dir]

-- | Parses a string to a 'Dir'
readDir :: String -> Dir
readDir ('R' : n) = V2 (read n) 0
readDir ('L' : n) = V2 (-read n) 0
readDir ('U' : n) = V2 0 (read n)
readDir ('D' : n) = V2 0 (-read n)


-- | Calculates the manhattan distance between two points,
-- which equals the sum of the differences in X and Y coordinates.
manhattanDistance :: Pos -> Pos -> Int
manhattanDistance p1 p2 = foldr ((+) . abs) 0 (p2 - p1)

-- | For a given position and a direction,
-- get all points from the position to the position where the direction leads to.
pointsBetweenMove :: Pos
                  -> Dir
                  -> (Pos, [Pos])
                  -- ^ The first 'Pos' is the result of applying the 'Dir' to the input 'Pos'.
                  -- The list of 'Pos's are all points in between the starting 'Pos' and the
                  -- end 'Pos', including the start and end points themselves.
pointsBetweenMove p@(V2 x y) d
  = let p2@(V2 x2 y2)  = p + d
        getRange a b = if b < a then reverse [b..a] else [a..b]
        xs           = getRange x x2
        ys           = getRange y y2
    in (p2, tail $ V2 <$> xs <*> ys)

-- | Get a list of all points that are on a given 'Path'.
pointsInPath :: Path -> [Pos]
pointsInPath ds = go (V2 0 0) ds []
    where go _ [] ps     = ps
          go p (d:ds) ps = let (p2, ps2) = pointsBetweenMove p d
                           in go p2 ds (ps ++ ps2)

-- | Finds all points in two different paths, and returns all
-- points where these paths intersect.
getIntersections :: Path -> Path -> [Pos]
getIntersections p1 p2 =
    let ps1 = S.fromList (pointsInPath p1)
        ps2 = S.fromList (pointsInPath p2)
    in S.toList $ S.intersection ps1 ps2 S.\\ S.singleton (V2 0 0)

-- | Splits the input file into lines, then splits those lines
-- by comma and parses all values using 'readDir'.
parseInput :: String -> [Path]
parseInput = map (map readDir . splitByComma) . lines

day3 :: String -> IO ()
day3 input = do
    let [p1, p2] = parseInput input
        is       = getIntersections p1 p2
    putStrLn "Answer 1:"
    print $ minimum $ map (manhattanDistance (V2 0 0)) is
    let points1 = pointsInPath p1
        points2 = pointsInPath p2
        index   = fromMaybe (error "Point does not exist in path") ∘∘ elemIndex
    let steps = minimum (map (\x -> index x points1 + index x points2) is) + 2
    putStrLn "Answer 2:"
    print steps