module Main where

import Test.HUnit
import Data.List (sort)

-- Define functions
minSum :: [Int] -> Int
minSum arr = sum $ zipWith (*) firstHalf (reverse secondHalf)
  where
    sorted = sort arr
    (firstHalf, secondHalf) = splitAt (length sorted `div` 2) sorted

-- Define test cases
test1 :: Test
test1 = TestCase $ assertEqual "retuns 22" 22 (minSum [5,4,2,3])

test2 :: Test
test2 = TestCase $ assertEqual "returns 342" 342 (minSum [12,6,10,26,3,24])

-- Create a test suite
testSuite :: Test
testSuite = TestList [TestLabel "returns 22" test1, TestLabel "returns 342" test2]
main :: IO ()
main = do
  -- Run the test suite
  counts <- runTestTT testSuite
  -- Print the test results
  putStrLn $ show counts

