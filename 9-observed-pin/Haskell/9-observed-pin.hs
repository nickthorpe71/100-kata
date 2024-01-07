module Main where

import Test.HUnit
import Data.Time (getCurrentTime, diffUTCTime)

-- Define functions
oddOrEven :: Int -> String
oddOrEven n = if odd n then "Odd" else "Even"

-- Define test cases
exapmpleTest :: Test
exapmpleTest = TestCase $ assertEqual "Odd test" "Odd" (oddOrEven 1)

-- Create a test suite
testSuite :: Test
testSuite = TestList [TestLabel "Test Even" testEven, TestLabel "TestOdd" testOdd]
main :: IO ()
main = do
  start <- getCurrentTime
  -- Run the test suite
  counts <- runTestTT testSuite
  end <- getCurrentTime
  -- Print the test results
  print (diffUTCTime end start)
  putStrLn $ show counts

