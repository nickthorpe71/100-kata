module Main where

import Data.Time (getCurrentTime, diffUTCTime)

-- Define functions
oddOrEven :: Int -> String
oddOrEven n = if odd n then "Odd" else "Even"

main :: IO ()
main = do
  start <- getCurrentTime
  -- run function
  let result = oddOrEven 1
  putStrLn $ "res: " ++ show result
  end <- getCurrentTime
  print (diffUTCTime end start)

