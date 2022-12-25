module Lib
    (
    findSolutions
    ) where


findSolutions :: [Int] -> Int -> Int -> Int -> [Int]
findSolutions range base mod_ res = [x | x <- range, solve x base mod_ res]

solve :: Int -> Int -> Int -> Int -> Bool
solve candidate base mod_ res = base ^ candidate `mod` mod_ == res
