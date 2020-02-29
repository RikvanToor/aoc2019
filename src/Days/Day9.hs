module Days.Day9 where

import Utils.IntCode
import Control.Monad.State

-- | Runs a program until it can input a given value.
-- Then continues running the program until it outputs a value,
-- and returns this value.
runInputOutput :: Monad m => Int -> StateT S m Int
runInputOutput input = do
    op <- runUntilAction
    runInput input
    runUntilAction
    runOutput

day9 :: String -> IO ()
day9 input = do
    let program = initialize input
    putStrLn "Answer 1:"
    answer1 <- evalStateT (runInputOutput 1) program
    print answer1
    putStrLn "Answer 2:"
    answer2 <- evalStateT (runInputOutput 2) program
    print answer2