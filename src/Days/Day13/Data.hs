{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Days.Day13.Data where

import Control.Monad.State
import Data.Grid
import Utils.IntCode

width, height, blockSize :: Num a => a
width    = 43
height   = 23
blockSize = 20

data Block = Empty
           | Wall
           | Block
           | Paddle
           | Ball
    deriving (Eq, Show)

intToBlock :: Int -> Block
intToBlock = \case 0 -> Empty
                   1 -> Wall
                   2 -> Block
                   3 -> Paddle
                   4 -> Ball
                   _ -> error "Unknown block"

type GridSize = '[43, 23]

type Map = Grid GridSize Block

data World = World { wmap  :: Map
                   , score :: Int
                   }

type State m a = StateT World (StateT S m) a

emptyGrid :: Grid GridSize Block
emptyGrid = generate (const Empty)

emptyWorld :: World
emptyWorld = World emptyGrid 0