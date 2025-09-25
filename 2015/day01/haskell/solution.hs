-- Advent of Code 2015 Day 1 - Haskell Solution
-- Not Complete: This is just an example structure to demonstrate the new language support

module Main where
import System.Environment (getArgs)
import System.IO

-- Calculate the final floor based on instructions
calculateFloor :: String -> Int
calculateFloor = foldl updateFloor 0
  where
    updateFloor acc '(' = acc + 1
    updateFloor acc ')' = acc - 1
    updateFloor acc _   = acc

-- Find the position of the first character that causes Santa to enter the basement (floor -1)
findBasementPosition :: String -> Int
findBasementPosition instructions = findBasement instructions 0 1
  where
    findBasement [] _ _ = -1  -- Never enters basement
    findBasement (c:cs) floor pos
      | newFloor == -1 = pos
      | otherwise = findBasement cs newFloor (pos + 1)
      where
        newFloor = case c of
          '(' -> floor + 1
          ')' -> floor - 1
          _   -> floor

main :: IO ()
main = do
    args <- getArgs
    let inputFile = case args of
                      [] -> "../data/input.txt"
                      (file:_) -> file
    
    contents <- readFile inputFile
    let instructions = filter (`elem` "()") contents
    
    -- Part A: Calculate final floor
    let finalFloor = calculateFloor instructions
    putStrLn $ "Santa ends up on floor " ++ show finalFloor
    
    -- Part B: Find position where Santa first enters basement
    let basementPos = findBasementPosition instructions
    if basementPos /= -1
        then putStrLn $ "The first character that causes Santa to enter the basement is at position " ++ show basementPos
        else putStrLn "Santa never enters the basement"