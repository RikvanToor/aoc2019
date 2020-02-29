module Main where

import Days
import Data.Maybe

main :: IO ()
main = do
    putStrLn $ "Enter the day you want to run [1-" ++ show lastDay ++ "]:"
    i <- getLine
    let n = fmap fst . listToMaybe . reads $ i
    case n of
        Nothing  -> putStrLn (i ++ " is not a valid number.") >> main
        (Just d) -> readFile ("data/" ++ show d ++ ".in") >>= getDay d