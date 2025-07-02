{-# LANGUAGE ScopedTypeVariables #-}

import System.Process
import System.IO  
import System.Directory
import Control.Monad
import Data.List (isPrefixOf, sort, isInfixOf)
import Data.Char (isDigit)
import Control.Exception (SomeException, catch)

-- Extract numeric part from filename like "input-001.txt" -> "001"
extractNumber :: String -> Maybe String
extractNumber filename =
  let
    parts = drop 6 filename  -- drops "input-"
    numberPart = takeWhile isDigit parts
  in
    if not (null numberPart) then Just numberPart else Nothing

-- Run a single test case by calling ws.hs
runTestCase :: String -> String -> IO Bool
runTestCase inputFile expectedPath = do
  -- Copy the input file to current directory temporarily
  let inputPath = "input/" ++ inputFile
  copyFile inputPath inputFile
  
  -- Run ws.hs with just the filename
  let outputFile = "output" ++ drop 5 inputFile  -- This will be "output-001.txt" etc.
  
  -- Execute ws.hs
  _ <- system $ "runghc ws.hs " ++ inputFile
  
  -- Clean up the temporary input file
  removeFile inputFile
  
  -- Check if output file was created
  outputExists <- doesFileExist outputFile
  if not outputExists then do
    putStrLn $ "FAIL: " ++ inputFile ++ " (Output file not created at " ++ outputFile ++ ")"
    return False
  else do
    -- Read the generated output file
    actual <- readFile outputFile
    
    -- Read expected output
    expected <- readFile expectedPath
    
    -- Compare and report
    let trimmed_actual = unwords . words $ actual  -- normalize whitespace
    let trimmed_expected = unwords . words $ expected  -- normalize whitespace
    
    if trimmed_actual == trimmed_expected then do
      putStrLn $ "PASS: " ++ inputFile
      return True
    else do
      putStrLn $ "FAIL: " ++ inputFile
      putStrLn $ "  Expected: " ++ trimmed_expected
      putStrLn $ "  Actual:   " ++ trimmed_actual
      putStrLn $ "  Output file: " ++ outputFile
      return False

-- Clean up all output files in the current directory
cleanupOutputFiles :: IO ()
cleanupOutputFiles = do
  currentDir <- getCurrentDirectory
  files <- getDirectoryContents currentDir
  let outputFiles = filter (\f -> "output-" `isPrefixOf` f && ".txt" `isInfixOf` f) files
  forM_ outputFiles $ \file -> do
    removeFile file `catch` \(e :: SomeException) -> 
      putStrLn $ "Could not remove " ++ file

-- Run all test cases
runAllTests :: Bool -> IO ()
runAllTests cleanup = do
  -- Check if ws.hs exists
  wsExists <- doesFileExist "ws.hs"
  if not wsExists then do
    putStrLn "Error: ws.hs not found in current directory"
    error "Missing program file"
  else do
    -- Get all input files
    inputFiles <- getDirectoryContents "input" `catch` handleError
    let validInputFiles = filter (\f -> "input-" `isPrefixOf` f && ".txt" `isInfixOf` f) inputFiles
    
    -- Process and check each file
    results <- forM (sort validInputFiles) $ \inputFile -> do
      case extractNumber inputFile of
        Just num -> do
          let expectedFile = "expected-" ++ num ++ ".txt"  -- Changed from "expected-" to "output-"
          let expectedPath = "expected/" ++ expectedFile
          
          -- Check if expected file exists
          expectedExists <- doesFileExist expectedPath
          if expectedExists then
            runTestCase inputFile expectedPath
          else do
            putStrLn $ "SKIP: " ++ inputFile ++ " (No matching expected file found at " ++ expectedPath ++ ")"
            return False
        Nothing -> do
          putStrLn $ "SKIP: " ++ inputFile ++ " (Invalid filename format)"
          return False
    
    -- Summary
    let passCount = length . filter id $ results
    let totalCount = length results
    putStrLn $ "\nSummary: " ++ show passCount ++ "/" ++ show totalCount ++ " tests passed"
    
    -- Cleanup if requested
    when cleanup $ do
      putStrLn "\nCleaning up output files..."
      cleanupOutputFiles
    
    -- Exit with appropriate status
    if passCount == totalCount then
      putStrLn "All tests passed!"
    else
      error "Some tests failed!"
    
  where
    handleError :: SomeException -> IO [FilePath]
    handleError e = do
      putStrLn $ "Error reading input directory: " ++ show e
      return []

main :: IO ()
main = do
  -- Create directories if they don't exist
  createDirectoryIfMissing True "input"
  createDirectoryIfMissing True "expected"
  
  -- Run the tests (change to True if you want automatic cleanup)
  runAllTests False  -- Set to True to automatically clean up output files