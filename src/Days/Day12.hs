module Days.Day12 where

import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.Combinator
import Linear.V3

data Moon = Moon { pos :: V3 Int, vel :: V3 Int }
  deriving Show

-- * Parsing

parseInt :: Parsec String u Int
parseInt = (maybe id (const negate) <$> optionMaybe (char '-'))
       <*> (read <$> many1 digit)

parseVector :: Parsec String u (V3 Int)
parseVector = between (char '<') (char '>')
            $ V3
          <$> getNum
          <*  sep
          <*> getNum
          <*  sep
          <*> getNum
    where getNum = alphaNum *> char '=' *> parseInt
          sep    = char ',' *> char ' '

parseInput :: Parsec String u [Moon]
parseInput = map (`Moon` V3 0 0 0) <$> endBy parseVector (char '\n')

-- * Part 1

-- | Update the positions and velocities of a list of moons.
applyGravity :: [Moon] -> [Moon]
applyGravity moons = map (\x -> let newVel = vel x + f x
                                in  x { vel = newVel, pos = pos x + newVel}) moons
    where f m     = sum $ map (g m) moons
          g m1 m2 = signum (pos m2 - pos m1)

calcEnergy :: Moon -> Int
calcEnergy m = sumScalars (pos m) * sumScalars (vel m)
    where sumScalars (V3 x y z) = abs x + abs y + abs z

-- | Calculate the sum of the energy of all moons in a given list.
totalEnergy :: [Moon] -> Int
totalEnergy = sum . map calcEnergy

-- * Part 2

-- | Find the amount of steps it takes for a set of moons to reset to
-- the initial position
findRepetition :: [Moon] -> Int
findRepetition init =
    let getRep f = let ap = fmap f
                       i  = ap init
                       ss = map ap (tail (iterate applyGravity init))
                   in fromJust (elemIndex i ss) + 1
        rx       = getRep pxvx
        ry       = getRep pyvy
        rz       = getRep pzvz
    in lcm' (V3 rx ry rz)

-- | Get the X-dimensions of the position and velocity of a moon
pxvx :: Moon -> (Int, Int)
pxvx (Moon (V3 px py pz) (V3 vx vy vz)) = (px, vx)

-- | Get the Y-dimensions of the position and velocity of a moon
pyvy :: Moon -> (Int, Int)
pyvy (Moon (V3 px py pz) (V3 vx vy vz)) = (py, vy)

-- | Get the Z-dimensions of the position and velocity of a moon
pzvz :: Moon -> (Int, Int)
pzvz (Moon (V3 px py pz) (V3 vx vy vz)) = (pz, vz)

-- | Find the least common multiple of the values in a vector
lcm' :: V3 Int -> Int
lcm' (V3 x y z) = let l1 = lcm x y
                      l2 = lcm x z
                      l3 = lcm y z
                  in lcm l1 (lcm l2 l3)

day12 :: String -> IO ()
day12 input = do
    let Right moons = parse parseInput "" input
        steps = iterate applyGravity moons
    putStrLn "Answer 1:"
    let step1000 = steps !! 1000
    print (totalEnergy step1000)
    putStrLn "Answer 2:"
    print $ findRepetition moons