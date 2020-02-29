module Days.Day6 where

import Data.Tree
import Data.List
import Data.Maybe

type Key = String
type Orbit = (Key, Key)

-- | Parses the input file to a list of keys where the right
-- key orbits the left one.
parseString :: String -> [Orbit]
parseString = concatMap get . lines
    where get s = maybeToList $ fmap tail . (`splitAt` s) <$> elemIndex ')' s

type Map = Tree Key

-- | Creates a tree structure of all 'Key's in a list, using COM as the root.
createMap :: [Orbit] -> Map
createMap o = unfoldTree (\x -> (x, map snd $ filter ((== x) . fst) o)) "COM"

-- | Gets all 'Key's that do not orbit around any other 'Key's
getRoots :: [Orbit] -> [Key]
getRoots s = foldr ((\a b -> filter (/= a) b) . snd) (map fst s) s

-- | Turns all elements in a tree into the depth level they are in the tree.
-- The root has level 0, its children level 1, etc.
-- Also retains the original element by returning a tree of tuples.
treeDepths :: Tree a -> Tree (a, Int)
treeDepths m = go m 0
    where go (Node x xs) i = Node (x, i) (map (`go` (i + 1)) xs)


-- | Counts the number of direct and indirect orbits in a 'Map'
countOrbits :: Map -> Int
countOrbits = sum . fmap snd . treeDepths

-- | Finds the closest common parent between two 'Key's in a 'Map'.
-- Returns the sum of the distances between both 'Key's and the common parent.
findCommonParentDepth :: Map -> Key -> Key -> Int
findCommonParentDepth m s1 s2 =
    let depths     = treeDepths m
        depth1         = snd $ head $ filterTree ((== s1) . fst) depths
        depth2         = snd $ head $ filterTree ((== s2) . fst) depths
        candidates = filterTree snd (applyTree (checkCandidate s1 s2) depths)
        height     = maximum (map (snd . fst) candidates)
    in depth1 - height + depth2 - height - 2

filterTree :: (a -> Bool) -> Tree a -> [a]
filterTree f (Node x xs) = let next = concatMap (filterTree f) xs
                           in if f x then x : next else next

-- | Checks whether a node contains two input 'Key's somewhere in the tree.
checkCandidate :: Key -> Key -> Tree (Key, Int) -> Bool
checkCandidate s1 s2 = (== 2) . length . filterTree (\(y,_) -> y == s1 || y == s2)

-- | Recursively pplies a function to a node. Returns the tree structure
-- with both the original value and the result of the function.
applyTree :: (Tree a -> b) -> Tree a -> Tree (a, b)
applyTree f n@(Node x xs) = Node (x, f n) (map (applyTree f) xs)

day6 :: String -> IO ()
day6 input = do
    let map = createMap (parseString input)
    putStrLn "Answer 1:"
    print (countOrbits map)
    putStrLn "Answer 2:"
    print (findCommonParentDepth map "YOU" "SAN")