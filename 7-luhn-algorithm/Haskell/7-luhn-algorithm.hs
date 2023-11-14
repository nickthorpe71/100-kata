module Main where

import Data.Time (getCurrentTime, diffUTCTime)

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sum . zipWith ($) (cycle [id, sum . digits . (*2)]) . reverse . digits
  where digits = map (read . return) . show

main :: IO ()
main = do
  start <- getCurrentTime
  
  print $ validate 2121 

  end <- getCurrentTime
  print (diffUTCTime end start)

