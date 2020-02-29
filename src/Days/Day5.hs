module Days.Day5 where

import Utils.IntCode
import Control.Monad.State

-- | Runs a program, inserting the input 'Int' whenever the
-- program asks for an input. Returns a list of all outputs.
runTests :: Monad m => Int -> StateT S m [Int]
runTests input = do
    op <- runUntilAction
    case op of
        Input  -> runInput input >> runTests input
        Output -> do
            output <- runOutput
            xs <- runTests input
            return (output : xs)
        Quit -> return []

-- | Runs a program and returns a list of all test results and
-- a diagnostic code.
runTestsFull :: Int
             -> S
             -> IO () -- ^ The list of all test results
                             -- and the diagnostic code.
runTestsFull input s = do (outputs, _) <- runStateT (runTests input) s
                          let (code:res)  = reverse outputs
                              testResults = reverse res 
                          putStrLn "Running tests..."
                          putStrLn ( if all (==0) testResults
                                     then "All tests passed!"
                                     else "There were errors..." )
                          putStrLn $ "Diagnostic code: " ++ show code

day5 :: String -> IO ()
day5 input = do
    let program = initialize input
    putStrLn "Answer 1:"
    runTestsFull 1 program
    putStrLn "Answer 2:"
    runTestsFull 5 program