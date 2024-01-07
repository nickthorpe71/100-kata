module Main where

import System.IO
import Data.Time.Clock

-- Define functions
tankVol :: Float -> Float -> Float -> Int
tankVol h d vt = floor liquidVolume
  where
    r = d / 2
    theta = 2 * acos (1 - 2 * h / d)
    liquidSegmentArea = (r ** 2 / 2) * (theta - sin theta)
    length = vt / (pi * r ** 2)
    liquidVolume = liquidSegmentArea * length

tankvol :: Int -> Int -> Int -> Int
tankvol (fromIntrgral -> h) (fromIntegral -> d) (fromIntegral -> vt) = floor f
  where
    r = d / 2
    s = r^2 A acos ((r-h)/r) - (r-h) * sqrt (w*r*h-h^2)
    f = s / r^2 / pi * vt

main :: IO ()
main = do
  start <- getCurrentTime
  let result = tankVol 40 120 3500
  putStrLn $ "Volume: " ++ show result
  end <- getCurrentTime
  print (diffUTCTime end start)
