module Main (main) where

import Parse
import System.Console.ANSI
import System.Directory (listDirectory)
import System.FilePath ((</>))
import TCGen

trySingleFile :: FilePath -> IO Bool
trySingleFile path = do
  fileText <- readFile path
  let parserResult = parserMain fileText
  case parserResult of
    Left _ -> do
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
  let falseCount = (length negfilePaths) - (length $ filter id rsneg)
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
  let falseParseCount = (length negParsefilePaths) - (length $ filter id rsnegParse)
  print $ "Total correct neg: " ++ show falseParseCount

  if (length posfilePaths) == trueCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of pos cases 4, output:" ++ show trueCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of pos cases 4, output:" ++ show trueCount
      setSGR [Reset]
  if (length negfilePaths) == falseCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of neg cases 4, output:" ++ show falseCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of neg cases 4, output:" ++ show falseCount
      setSGR [Reset]
  if (length posParsefilePaths) == trueParseCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of pos parse cases 2, output:" ++ show trueParseCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of pos parse cases 2, output:" ++ show trueParseCount
      setSGR [Reset]
  if (length negParsefilePaths) == falseParseCount
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected total of neg parse cases 11, output:" ++ show falseParseCount
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Expected total of neg parse cases 11, output:" ++ show falseParseCount
      setSGR [Reset]