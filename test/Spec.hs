module Main (main) where

import Parse
import System.Console.ANSI
import System.Directory (listDirectory)
import System.Exit
import System.FilePath ((</>))
import TCGen


trySingleFile :: FilePath -> IO Bool
trySingleFile path = do
  fileText <- readFile path
  let parserResult = parserMain fileText
  case parserResult of
    Left x -> do
      print path
      print "Error: Could not parse provided file."
      return False
    Right y -> do
      print path
      var <- checker y
      -- print $ if var then "Valid" else "Invalid"
      putStrLn $ "The file " <> path <> " is " <> (if var then "valid." else "invalid.")
      return var

main :: IO ()
main = do
  -- Pos cases
  let posdirectory = "programs/pos"
  posNames <- listDirectory posdirectory
  let posfilePaths = (posdirectory </>) <$> posNames
  rspos <- mapM trySingleFile posfilePaths
  let trueCount = length $ filter id rspos
  print $ "Total correct pos: " ++ show trueCount
  -- Neg cases
  let negdirectory = "programs/neg"
  negNames <- listDirectory negdirectory
  let negfilePaths = (negdirectory </>) <$> negNames
  rsneg <- mapM trySingleFile negfilePaths
  let falseCount = 4 - (length $ filter id rsneg)
  print $ "Total correct neg: " ++ show falseCount
  -- Pos cases
  let posParsedirectory = "programs/posParse"
  posParseNames <- listDirectory posParsedirectory
  let posParsefilePaths = (posParsedirectory </>) <$> posParseNames
  rsposParse <- mapM trySingleFile posParsefilePaths
  let trueParseCount = length $ filter id rsposParse
  print $ "Total correct pos parse: " ++ show trueParseCount
  -- Neg cases
  let negParsedirectory = "programs/negParse"
  negParseNames <- listDirectory negParsedirectory
  let negParsefilePaths = (negParsedirectory </>) <$> negParseNames
  rsnegParse <- mapM trySingleFile negParsefilePaths
  let falseParseCount = 11 - (length $ filter id rsnegParse)
  print $ "Total correct neg: " ++ show falseParseCount

  if 4 == trueCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of pos cases 4, output:" ++ show trueCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of pos cases 4, output:" ++ show trueCount
      setSGR [Reset]
  if 4 == falseCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of pos cases 4, output:" ++ show falseCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of pos cases 4, output:" ++ show falseCount
      setSGR [Reset]
  if 2 == trueParseCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of pos cases 2, output:" ++ show trueParseCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of pos cases 2, output:" ++ show trueParseCount
      setSGR [Reset]
  if 11 == falseParseCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of pos cases 11, output:" ++ show falseParseCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of pos cases 11, output:" ++ show falseParseCount
      setSGR [Reset]