{-# LANGUAGE TypeApplications #-}

module Days.Day13.GameLogic where

import Control.Monad.State hiding (State)
import Data.Grid
import Data.List
import Days.Day13.Data
import Utils.IntCode
import System.Exit

-- | Handles 'updateState', but outside of the 'State' monad.
updateGame :: Float -> (World, S) -> IO (World, S)
updateGame t (w, s) = do
    ((o, w'), s') <- runStateT (runStateT updateState w) s
    case o of
        Quit -> print (score w') >> exitSuccess
        _    -> return (w', s')

-- | Run a program until it outputs something.
-- If the program halts or asks for an input instead, this will cause an error.
outputOrError :: Monad m => StateT S m Int
outputOrError = runUntilAction >> runOutput

-- | Runs the program until an action is required.
-- If the program quits, do nothing. If the program outputs a block or score update,
-- use 'updateBlock' to process this, and then run 'updateState' again.
-- If the program asks for an input, automatically insert the best possible input
-- that makes the paddle be right below the ball.
updateState :: Monad m => State m Operator
updateState = do
    op <- lift runUntilAction
    case op of
        Quit   -> return Quit
        Output -> do x <- lift outputOrError
                     y <- lift outputOrError
                     b <- lift outputOrError
                     w <- get
                     if x == -1 && y == 0
                     then put (w { score = b })
                     else put (w { wmap = updateBlock x y (intToBlock b) (wmap w)})
                     updateState
        Input  -> do w <- get
                     let x1 = findX Ball (wmap w)
                         x2 = findX Paddle (wmap w)
                     lift (runInput (signum (x1 - x2)))
                     return Input

-- | Finds the first X-coordinate of a given block type in a map.
findX :: Block -> Map -> Int
findX b m = let xs = map (elem b) $ toNestedLists m
            in foldr min width (elemIndices True xs)

-- | Place a block at given X and Y coordinates in a map.
updateBlock :: Int -> Int -> Block -> Map -> Map
updateBlock x y b m = let maybeC = coord @GridSize [x, y]
                      in maybe m (\c -> m // [(c, b)]) maybeC