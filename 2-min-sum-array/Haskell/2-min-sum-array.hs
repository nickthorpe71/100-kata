module Main where

import Data.List (sort)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Random

-- Define functions
minSum :: [Int] -> Int
minSum arr = sum $ zipWith (*) firstHalf (reverse secondHalf)
  where
    sorted = sort arr
    (firstHalf, secondHalf) = splitAt (length sorted `div` 2) sorted

main :: IO ()
main = do
  gen <- newStdGen
  let randomNumbers = take 1000000 (randoms gen :: [Int])
  start <- getCurrentTime
  let sum = minSum randomNumbers
  print sum
  end <- getCurrentTime
  print (diffUTCTime end start)

