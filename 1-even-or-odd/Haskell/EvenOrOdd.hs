module Main where

import System.IO 
import Data.Time.Clock

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n
  | even n    = "Even"
  | otherwise = "Odd"

main :: IO ()
main = do
  start <- getCurrentTime
  let result = evenOrOdd 1
  putStrLn $ show "res: " ++ show result
  end <- getCurrentTime
  print (diffUTCTime end start)
