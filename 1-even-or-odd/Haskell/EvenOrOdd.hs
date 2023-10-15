module Main where

import Test.HUnit

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n
  | even n    = "Even"
  | otherwise = "Odd"

-- Define test cases
testEven :: Test
testEven = TestCase $ assertEqual "Even test" "Even" (evenOrOdd 4)

testOdd :: Test
testOdd = TestCase $ assertEqual "Odd test" "Odd" (evenOrOdd 7)

-- Create a test suite
testSuite :: Test
testSuite = TestList [TestLabel "Test Even" testEven, TestLabel "TestOdd" testOdd]

-- Run the tests
main :: IO ()
main = do
  counts <- runTestTT testSuite
  putStrLn $ show counts
