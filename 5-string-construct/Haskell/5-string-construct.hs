module Main where

import Data.Time (getCurrentTime, diffUTCTime)

-- Define functions
stringConstructing :: String -> String -> Int
stringConstructing a s = construct 0 0 "" where
  construct i ops curr
    | curr == s = ops
    | otherwise =
      let (c', i') = findMismatch i curr s
      in if i' >= length curr
        then construct i' (ops + 1) (curr ++ a)
        else construct i' (ops + 1) (take i' c' ++ drop (i' + 1) c')

findMismatch :: Int -> String -> String -> (String, Int)
findMismatch i c s
  | i < length c && i < length s && c !! i == s !! i = findMismatch (i + 1) c s
  | otherwise = (c, i)


main :: IO ()
main = do
  start <- getCurrentTime
  -- run function
  putStrLn . show $ stringConstructing "a" "a"
  putStrLn . show $ stringConstructing "aba" "abbabba"
  putStrLn . show $ stringConstructing "a" "aaa"
  putStrLn . show $ stringConstructing "bbaabcbcbc" "bbcccbabcc" 
  end <- getCurrentTime
  print (diffUTCTime end start)

