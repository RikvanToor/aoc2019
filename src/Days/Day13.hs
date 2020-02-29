module Days.Day13 where

import           Control.Monad.State hiding (State)
import           Graphics.Gloss.Interface.IO.Game
import           Days.Day13.Data
import           Days.Day13.Render
import           Days.Day13.GameLogic
import           Data.Grid
import           Utils
import           Utils.IntCode
import qualified Data.Map.Strict as M

play :: World -> S -> IO ()
play w s = playIO (InWindow "Day 13" (width * blockSize, height * blockSize) (0, 0))
                  white
                  30
                  (w, s)
                  (return . drawWorld . fst)
                  (return ∘∘ seq)
                  updateGame

day13 :: String -> IO ()
day13 input = do
    let program = initialize input
    w <- evalStateT (execStateT updateState emptyWorld) program
    let count = sum $ length . filter (== Block) <$> toNestedLists (wmap w)
    putStrLn "Answer 1:"
    print count

    let program2 = program { memory = M.insert 0 2 (memory program) }
    (w, s) <- runStateT (execStateT updateState emptyWorld) program2
    putStrLn "Answer 2:"
    play w s