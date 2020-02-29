module Days.Day11 where

import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import           Utils.IntCode hiding (run)
import           Control.Monad.State
import           Utils.V2


data Color = Unpainted
           | Black
           | White
    deriving Eq

instance Show Color where
    show Black     = " "
    show White     = "■"
    show Unpainted = " "

colorToInt :: Color -> Int
colorToInt Black     = 0
colorToInt White     = 1
colorToInt Unpainted = 0

intToColor :: Int -> Color
intToColor 0 = Black
intToColor 1 = White
intToColor _ = error "Unknown color"

data S2 = S2 { position :: V2 Int
             , direction :: V2 Int
             , field :: M.Map (V2 Int) Color
             , s :: S
             }
    deriving Show

type Nest m a = StateT S2 (StateT S m) a

-- | Start a program with the position (0,0), an upwards direction
-- and an empty field.
initS2 :: S -> S2
initS2 = S2 (V2 0 0) (V2 0 1) mempty

-- | Enter the color of the current position,
-- get the new color and the new direction.
runStep :: Monad m => Color -> V2 Int -> StateT S m (Color, V2 Int)
runStep color dir = do
    op <- runUntilAction
    case op of
        Quit -> error "Did not expect to quit here"
        Input -> do
            runInput (colorToInt color)
            runUntilAction
            newColor <- intToColor <$> runOutput
            runUntilAction
            turn <- runOutput
            let newDir = (if turn == 0 then perp else perpcw) dir
            return (newColor, newDir)
        Output -> error "Did not expect an output here"

-- | Look up the color of the current position.
-- If it is unknown, add an entry to the field for the position
-- and make the color 'Unpainted'.
lookupPosition :: Monad m => Nest m Color
lookupPosition = do
    s2 <- get
    let pos   = position s2
        fld   = field s2
        c     = M.lookup pos fld
    case c of
        Nothing -> put s2 { field = M.insert pos Unpainted fld }
                >> return Unpainted
        Just c  -> return c

-- | Overwrite the color at the current position,
-- overwrite the direction,
-- add the direction to the position.
applyStep :: Monad m => Color -> V2 Int -> Nest m ()
applyStep color dir = do
    s2 <- get
    let pos = position s2
    put $ s2 { position = pos + dir
             , direction = dir
             , field = M.insert pos color (field s2)
             }

-- | Runs the entire program
run :: Monad m => Nest m ()
run = do
    op <- lift runUntilAction
    case op of
        Quit -> return ()
        Input -> do
            s2 <- get
            let pos = position s2
                dir = direction s2
            c <- lookupPosition
            (color, dir) <- lift (runStep c dir)
            applyStep color dir
            run
        Output -> error "Did not expect an output here"


-- | Creates a grid of pixels, colors them white or transparent.
showField :: M.Map (V2 Int) Color -> String
showField field =
    let ks            = M.keys field
        getX (V2 x _) = x
        getY (V2 _ y) = y
        minx          = minimum $ map getX ks
        maxx          = maximum $ map getX ks
        miny          = minimum $ map getY ks
        maxy          = maximum $ map getY ks
        getC x y      = fromMaybe Unpainted $ M.lookup (V2 x y) field
        grid = (\y -> concatMap (\x -> show $ getC x y) [minx..maxx])
           <$> reverse [miny..maxy]
    in unlines grid

day11 :: String -> IO ()
day11 input = do
    let program = initialize input
        s2      = initS2 program
    s2 <- evalStateT (execStateT run s2) program
    putStrLn "Answer 1:"
    let count = length $ M.filter (/= Unpainted) (field s2)
    print count

    let s2  = initS2 program
        s22 = s2 { field = M.insert (V2 0 0) White (field s2)}
    s2 <- evalStateT (execStateT run s22) program
    putStrLn "Answer 2:"
    putStrLn (showField (field s2))

