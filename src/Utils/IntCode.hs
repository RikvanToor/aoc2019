module Utils.IntCode where

import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Applicative
import Control.Monad.State
import Utils

data S = S { memory :: M.Map Int Int
           , current :: Int
           , pointer :: Int }
  deriving Show

-- | Parse a program and create an initial state.
initialize :: String -> S
initialize p = S (M.fromList (zip [0..] (parseString p))) 0 0

-- | Splits a string by comma, and converts all the values to 'Int's.
parseString :: String -> [Int]
parseString = map read . splitByComma

data Operator = Quit
              | Add
              | Multiply
              | Input
              | Output
              | IsTrue
              | IsFalse
              | LowerThan
              | EqualTo
              | SetPointer
  deriving Show

intToOp :: Int -> Operator
intToOp 99 = Quit
intToOp 1  = Add
intToOp 2  = Multiply
intToOp 3  = Input
intToOp 4  = Output
intToOp 5  = IsTrue
intToOp 6  = IsFalse
intToOp 7  = LowerThan
intToOp 8  = EqualTo
intToOp 9  = SetPointer
intToOp x  = error ("Invalid operator " ++ show x)

-- | Runs a step of the program and returns the current 'Operator'.
run :: Monad m => StateT S m Operator
run = do
  S d p r                <- get
  (op, val1, val2, val3) <- getOpAndVals
  case op of
    Add        -> put (S (M.insert val3 (val1 + val2) d) (p + 4) r)
    Multiply   -> put (S (M.insert val3 (val1 * val2) d) (p + 4) r)
    IsTrue     -> do let newPos          = if val1 /= 0
                                           then val2
                                           else p + 3
                     put (S d newPos r)
    IsFalse    -> do let newPos          = if val1 == 0
                                           then val2
                                           else p + 3
                     put (S d newPos r)
    LowerThan  -> do let result            = fromEnum $ val1 < val2
                     put (S (M.insert val3 result d) (p + 4) r)
    EqualTo    -> do let result            = fromEnum $ val1 == val2
                     put (S (M.insert val3 result d) (p + 4) r)
    SetPointer -> put (S d (p + 2) (r + val1))
    _  -> return ()
  return op

-- | Runs a program until a the program quits, needs an input or will output something.
runUntilAction :: Monad m => StateT S m Operator
runUntilAction = do op <- run
                    case op of
                      Quit   -> return Quit
                      Input  -> return Input
                      Output -> return Output
                      _      -> runUntilAction

-- | Tries to insert a given integer into the program.
-- Failes if the program does not ask for an input.
runInput :: Monad m => Int -> StateT S m ()
runInput i = do
  S d p r          <- get
  (op, val1, _, _) <- getOpAndVals
  (_, mod1, _, _)  <- getOpAndModes
  case op of
      Input -> do (_, val1, _, _) <- getOpAndVals
                  (_, mod1, _, _) <- getOpAndModes
                  S d p r         <- get
                  let pos = (if mod1 == 2 then r else 0) + lu (p + 1) d
                  put (S (M.insert pos i d) (p + 2) r)
      _     -> error "No input expected."

-- | Tries to get an output out of the program.
-- Fails if there is no output available.
runOutput :: Monad m => StateT S m Int
runOutput = do
  S d p r          <- get
  (op, val1, _, _) <- getOpAndVals
  case op of
    Output -> do put (S d (p + 2) r)
                 return val1
    _      -> error "No output available."

-- | Runs a program in 'IO' mode, where inputs and outputs are handled by the terminal.
runIO :: StateT S IO ()
runIO = do op <- runUntilAction
           case op of
             Quit   -> return ()
             Input  -> do lift (putStrLn "Input: ")
                          i <- lift readLn
                          runInput i
                          runIO
             Output -> do x <- runOutput
                          lift (print x)
                          runIO
             _      -> error "No Actions available"

-- | Get the current operator and its parameters
getOpAndVals :: Monad m => StateT S m (Operator, Int, Int, Int)
getOpAndVals = do
  S d p r                <- get
  (op, mod1, mod2, mod3) <- getOpAndModes
  let par1, par2, par3 :: Int
      par1 = fromJust $ M.lookup (p + 1) d <|> Just 99999
      par2 = fromJust $ M.lookup (p + 2) d <|> Just 99999
      par3 = fromJust $ M.lookup (p + 3) d <|> Just 99999
      getV 0 par = lu par d
      getV 1 par = par
      getV 2 par = lu (r + par) d
      getP 2 par = par + r
      getP _ par = par
      val1 = getV mod1 par1
      val2 = getV mod2 par2
      val3 = getP mod3 par3
  return (op, val1, val2, val3)

-- | Get the current opeartor and the modes of each parameter.
-- Modes can be 0: position, 1: immediate, or 2: relative.
-- In position mode, parameters reference an exact memory location.
-- In immediate mode, parameters are literal values.
-- In relative mode, parameters reference a memory location based on the current pointer.
getOpAndModes :: Monad m => StateT S m (Operator, Int, Int, Int)
getOpAndModes = do
  S d p r <- get
  let p' = show $ lu p d
      num = replicate (5 - length p') '0' ++ p'
      mod1, mod2, mod3 :: Int
      mod1 = read $ pure $ num !! 2
      mod2 = read $ pure $ num !! 1
      mod3 = read $ pure $ head num
      op   = intToOp $ read (drop 3 num)
  return (op, mod1, mod2, mod3)

-- | Lookup the value at a given memory index.
-- Throws an error if the index is negative, but returns 0 for a positive, but unknown address.
lu :: (Ord k, Show k, Num k, Num c) => k -> M.Map k c -> c
lu k | k < 0     = error ("Negative index " ++ show k)
     | otherwise = fromMaybe 0 . M.lookup k