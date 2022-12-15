module Lib
    (
    multipleSolve
    ) where


multipleSolve :: [Int] -> Int -> Int -> Int -> [Int]
multipleSolve range base mod_ res = [x | x <- range, singleSolve x base mod_ res]

singleSolve :: Int -> Int -> Int -> Int -> Bool
singleSolve candidate base mod_ res = base ^ candidate `mod` mod_ == res
