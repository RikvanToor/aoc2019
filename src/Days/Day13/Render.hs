{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Days.Day13.Render where

import Days.Day13.Data
import Data.Grid
import Graphics.Gloss.Interface.IO.Game

render :: World -> IO Picture
render = return . drawWorld

-- | Draw all the 'Block's in a map
drawMap :: Map -> Picture
drawMap = pictures
        . map f
        . concat
        . zipWith (\x col -> zipWith (x,,) [0..] col) [0..]
        . toNestedLists
    where f (x,y,b) = translate (blockSize * fromIntegral x) (blockSize * fromIntegral (height - y)) (drawBlock b)

drawScore :: Int -> Picture
drawScore = let s = blockSize * 0.01
            in translate blockSize blockSize
               . scale s s
               . color red
               . text
               . show

-- | Draw the map using 'drawMap', and place the score on the bottom left using 'drawScore'
drawWorld :: World -> Picture
drawWorld w = translate (blockSize / 2) (-blockSize / 2)
            $ translate (-width * 0.5 * blockSize) (-height * 0.5 * blockSize)
            $ pictures [ drawScore (score w), drawMap (wmap w)]

drawBlock :: Block -> Picture
drawBlock = let rect = (`color` polygon (rectanglePath blockSize blockSize))
            in \case Empty  -> blank
                     Wall   -> rect black
                     Block  -> rect orange
                     Ball   -> circle (blockSize * 0.4)
                     Paddle -> color (greyN 0.5) (translate (-blockSize / 2) (blockSize / 4) (polygon (rectanglePath (blockSize * 2) (blockSize / 2))))