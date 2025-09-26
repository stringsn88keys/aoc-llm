-- Tests for Advent of Code 2015 Day 2 Part A

-- Simple testing framework without external dependencies

calculateWrappingPaper :: Int -> Int -> Int -> Int
calculateWrappingPaper l w h = surfaceArea + smallestSide
  where
    surfaceArea = 2*l*w + 2*w*h + 2*h*l
    sides = [l*w, w*h, h*l]
    smallestSide = minimum sides

parseDimensions :: String -> (Int, Int, Int)
parseDimensions line = (l, w, h)
  where
    parts = words $ map (\c -> if c == 'x' then ' ' else c) line
    [l, w, h] = map read parts

-- Simple test runner
runTest :: String -> Bool -> IO ()
runTest name result = do
  let status = if result then "✓" else "✗"
  putStrLn $ status ++ " " ++ name

main :: IO ()
main = do
  putStrLn "Running Haskell Day 2 tests..."
  
  -- Test calculateWrappingPaper with examples
  runTest "Example 1 (2x3x4 = 58)" (calculateWrappingPaper 2 3 4 == 58)
  runTest "Example 2 (1x1x10 = 43)" (calculateWrappingPaper 1 1 10 == 43)
  
  -- Test edge cases
  runTest "Cube (2x2x2 = 28)" (calculateWrappingPaper 2 2 2 == 28)
  runTest "Unit cube (1x1x1 = 7)" (calculateWrappingPaper 1 1 1 == 7)
  
  -- Test parseDimensions
  runTest "Parse 2x3x4" (parseDimensions "2x3x4" == (2,3,4))
  runTest "Parse 1x1x10" (parseDimensions "1x1x10" == (1,1,10))
  runTest "Parse 100x200x300" (parseDimensions "100x200x300" == (100,200,300))
  
  putStrLn "All tests completed!"