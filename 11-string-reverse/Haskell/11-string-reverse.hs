import Data.List (intercalate)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

spinWords :: String -> String
spinWords = intercalate " " . map spin . words
  where
    spin word
      | length word > 4 = reverse word
      | otherwise       = word

main :: IO ()
main = do
  startTime <- getCPUTime
  putStrLn $ spinWords "Test Testing lamamama 1 1 1 asd dfs   "
  endTime <- getCPUTime
  let diff = fromIntegral (endTime - startTime) / (10^12)  -- converting picoseconds to seconds
  printf "Execution Time: %0.3f sec\n" (diff :: Double)