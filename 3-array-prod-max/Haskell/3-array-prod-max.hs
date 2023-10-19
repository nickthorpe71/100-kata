module Main where

import Test.HUnit
import Data.List (sort)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Random


-- Define functions
maxProduct :: (Ord a, Num a) => [a] -> Int -> a 
maxProduct xs n = product $ take n $ reverse $ sort xs 

main :: IO ()
main = do
  -- Run the test suite
  -- counts <- runTestTT testSuite
  gen <- newStdGen
  let randomNumbers = take 1000000 (randoms gen :: [Int])
  start <- getCurrentTime
  let sum = maxProduct randomNumbers 5
  print sum
  end <- getCurrentTime
  print (diffUTCTime end start)
  -- putStrLn $ show counts
