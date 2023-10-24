module Main where

import Data.Time (getCurrentTime, diffUTCTime)

-- Define functions
stringConstructing :: String -> String -> Int
stringConstructing a s = 

levelshtein :: String -> String -> Int
levelshtein s1 s2 = last $ foldl transform [0..length s1] s2
  where
    transform sx@(x:xs) c = scanl compute (x + 1) (zip3 s1 xs xs')
      where
        compute z (c', x, y) = minimum [y + 1, z + 1, x + if c == c' then 0 else 1]

main :: IO ()
main = do
  start <- getCurrentTime
  -- run function
  let result = oddOrEven 1
  putStrLn $ "res: " ++ show result
  end <- getCurrentTime
  print (diffUTCTime end start)

