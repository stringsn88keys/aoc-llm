-- Advent of Code 2015 Day 2 - I Was Told There Would Be No Math

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (tryIOError)

-- Calculate wrapping paper needed for a present with dimensions l x w x h
calculateWrappingPaper :: Int -> Int -> Int -> Int
calculateWrappingPaper l w h = surfaceArea + smallestSide
  where
    surfaceArea = 2*l*w + 2*w*h + 2*h*l
    sides = [l*w, w*h, h*l]
    smallestSide = minimum sides

-- Parse a line like "2x3x4" into dimensions
parseDimensions :: String -> (Int, Int, Int)
parseDimensions line = (l, w, h)
  where
    parts = words $ map (\c -> if c == 'x' then ' ' else c) line
    [l, w, h] = map read parts

-- Solve part A: calculate total wrapping paper needed
solvePartA :: String -> IO Int
solvePartA filename = do
  contents <- readFile filename
  let linesOfInput = filter (not . null) $ lines contents
  let dimensions = map parseDimensions linesOfInput
  let paperAmounts = map (\(l, w, h) -> calculateWrappingPaper l w h) dimensions
  return $ sum paperAmounts

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "../data/input.txt" else head args
  
  result <- tryIOError $ solvePartA filename
  case result of
    Left _ -> do
      putStrLn $ "Error: File '" ++ filename ++ "' not found."
      exitFailure
    Right totalPaper -> do
      putStrLn $ "The elves should order " ++ show totalPaper ++ " square feet of wrapping paper"