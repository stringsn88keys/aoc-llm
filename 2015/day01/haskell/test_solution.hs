-- Advent of Code 2015 Day 1 - Haskell Tests
-- Not Complete: This is just an example structure to demonstrate the new language support

import System.Exit (exitFailure, exitSuccess)

-- Import the solution functions (would need proper module setup for real implementation)
-- For this example, we'll redefine them locally

calculateFloor :: String -> Int
calculateFloor = foldl updateFloor 0
  where
    updateFloor acc '(' = acc + 1
    updateFloor acc ')' = acc - 1
    updateFloor acc _   = acc

findBasementPosition :: String -> Int
findBasementPosition instructions = findBasement instructions 0 1
  where
    findBasement [] _ _ = -1
    findBasement (c:cs) floor pos
      | newFloor == -1 = pos
      | otherwise = findBasement cs newFloor (pos + 1)
      where
        newFloor = case c of
          '(' -> floor + 1
          ')' -> floor - 1
          _   -> floor

-- Test framework
data TestResult = Pass | Fail String deriving (Eq)

runTest :: String -> Bool -> TestResult
runTest name True = Pass
runTest name False = Fail name

-- Test cases
tests :: [TestResult]
tests = [
    runTest "empty string" (calculateFloor "" == 0),
    runTest "(()) equals 0" (calculateFloor "(())" == 0),
    runTest "()() equals 0" (calculateFloor "()()" == 0),
    runTest "((( equals 3" (calculateFloor "(((" == 3),
    runTest "))) equals -3" (calculateFloor ")))" == -3),
    runTest "basement at 1" (findBasementPosition ")" == 1),
    runTest "basement at 5" (findBasementPosition "()())" == 5),
    runTest "never basement" (findBasementPosition "(((" == -1)
  ]

main :: IO ()
main = do
    let failures = [msg | Fail msg <- tests]
    let passCount = length tests - length failures
    
    putStrLn $ "Tests run: " ++ show (length tests)
    putStrLn $ "Passed: " ++ show passCount
    putStrLn $ "Failed: " ++ show (length failures)
    
    if null failures
        then do
            putStrLn "All tests passed!"
            exitSuccess
        else do
            putStrLn "Failed tests:"
            mapM_ putStrLn failures
            exitFailure