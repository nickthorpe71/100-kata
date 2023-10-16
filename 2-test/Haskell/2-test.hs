module Main where

import Test.HUnit

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
	-- Run the test suite
	counts <- runTestTT testSuite
	-- Print the test results
	putStrLn shot counts

