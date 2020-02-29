module Days.Day7 where

import Utils.IntCode
import Control.Monad.State
import Data.List (permutations)
import Data.Maybe (catMaybes)
import Utils

-- | Runs a program until it needs to input a value.
-- Inputs this value.
runUntilInput :: Monad m => Int -> StateT S m ()
runUntilInput input = do op <- runUntilAction
                         case op of
                             Input -> runInput input
                             Quit  -> return ()
                             _     -> error "Expected input"

-- | Runs a program until it outputs a value. Returns this value
-- if possible, or Nothing if the program quit.
runUntilOutput :: Monad m => StateT S m (Maybe Int)
runUntilOutput = do op <- runUntilAction
                    case op of
                        Output -> Just <$> runOutput
                        Quit   -> return Nothing
                        _      -> error "Expected output"

-- | Inputs a phase value, and then an input value.
-- Returns the first output
runAmplifier :: Monad m => Int -> Int -> StateT S m (Maybe Int)
runAmplifier phase input = do
    runUntilInput phase
    runOnce input

-- | Run  one input and then one output
runOnce :: Monad m => Int -> StateT S m (Maybe Int)
runOnce input = runUntilInput input >> runUntilOutput

-- | Runs a system of amplifiers based on a given program and
-- a list of phase settings
runSystem :: [Int] -> S -> Maybe Int
runSystem phases program = foldl go (Just 0) phases
    where go (Just input) phase = evalState (runAmplifier phase input) program
          go Nothing _          = Nothing

-- | Runs a system of amplifiers recursively until at least one of the
-- amplifiers has halted. Returns the last output value.
runSystemFeedback :: [Int] -> S -> Int
runSystemFeedback phases program =
    let init = map (\x -> execState (runUntilInput x) program) phases
        step (input, running, states) = (\(x,y,z) -> (x,y,reverse z)) $
                                        foldl go (input, running, []) states
        go (input, running, states) amp
            = let (num, state) = runState (runOnce input) amp
                  ss           = state : states
              in maybe (input, False, ss) (\x -> (x, True, ss)) num
    in fst3 $ until (not . snd3) step (0, True, init)


day7 :: String -> IO ()
day7 input = do
    let program = initialize input
        phases  = permutations [0..4]
        total   = map (`runSystem` program) phases
    putStrLn "Answer 1:"
    print (maximum (catMaybes total))
    putStrLn "Answer 2:"
    let phases2 = permutations [5..9]
        total2  = map (`runSystemFeedback` program) phases2
    print (maximum total2)

    