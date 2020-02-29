module Days 
    ( module Days
    , day1
    , day2
    , day3
    , day4
    , day5
    , day6
    , day7
    , day8
    , day9
    , day10
    , day11
    , day12
    , day13
    ) where

import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6
import Days.Day7
import Days.Day8
import Days.Day9
import Days.Day10
import Days.Day11
import Days.Day12
import Days.Day13

lastDay :: Int
lastDay = 13

getDay :: Int -> String -> IO ()
getDay  1 = day1
getDay  2 = day2
getDay  3 = day3
getDay  4 = day4
getDay  5 = day5
getDay  6 = day6
getDay  7 = day7
getDay  8 = day8
getDay  9 = day9
getDay 10 = day10
getDay 11 = day11
getDay 12 = day12
getDay 13 = day13
getDay n  = error (show n ++ " is not a valid day.")