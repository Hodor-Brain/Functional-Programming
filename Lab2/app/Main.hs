module Main (main) where

import Lib
import System.IO
import Control.Concurrent

printSolution :: [Int] -> Int -> Int -> Int -> IO String
printSolution interval base modulus remainder = do
  tid <- myThreadId
  let solved = findSolutions interval base modulus remainder
  let result = show tid ++ " with result " ++ show solved
  return $! result

solve :: MVar () -> Chan () -> [Int] -> Int -> Int -> Int -> IO()
solve mutex endFlags interval base modulus remainder = do
  solution <- printSolution interval base modulus remainder
  takeMVar mutex
  putStrLn solution
  putMVar mutex ()
  writeChan endFlags ()

main :: IO()
main = do
  putStrLn "(base)^x = reminder (mod modulus)"
  putStrLn "base:"
  input1 <- getLine
  putStrLn "remainder:"
  input2 <- getLine
  putStrLn "modulus:"
  input3 <- getLine
  let base = (read input1 :: Int)
  let remainder = (read input2 :: Int)
  let modulus = (read input3 :: Int)

  hSetBuffering stdout NoBuffering
  mutex <- newEmptyMVar
  endFlags <- newChan

  forkIO $ solve mutex endFlags [1..20] base modulus remainder
  forkIO $ solve mutex endFlags [21..40] base modulus remainder
  forkIO $ solve mutex endFlags [41..60] base modulus remainder

  putMVar mutex ()
  mapM_ (const $ readChan endFlags) [1..3]
