module Days.Day2 where

import           Utils.IntCode
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as M

-- | Takes a program state 'S'.
-- Also takes two 'Int' parameters that decide the input values,
-- which get inserted at addresses 1 and 2.
-- Runs the program and returns the value at address 0.
runProgram :: S -> Int -> Int -> IO Int
runProgram s a b = let s' = s { memory = M.insert 1 a (M.insert 2 b (memory s)) }
                   in lu 0 . memory <$> execStateT runIO s

day2 :: String -> IO ()
day2 input = do
    let program = initialize input
    x <- runProgram program 12 2
    putStrLn "Answer 1:"
    print x
    -- Carthesian product [0..99]×[0..99]
    let options = (,) <$> [0..99] <*> [0..99]
        goal    = 19690720
    answers <- traverse (uncurry (runProgram program)) options
    let inputs = (\(x,y) -> x*100 + y) . (options !!)
             <$> elemIndex goal answers
    putStrLn "Answer 2:"
    print inputs