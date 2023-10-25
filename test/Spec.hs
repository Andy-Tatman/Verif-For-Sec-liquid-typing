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
      putStrLn $ "Error: Could not parse " <> path <> "."
      return False
    Right y -> do
      print path
      var <- checker y
      putStrLn $ "The file " <> path <> " is " <> (if var then "valid." else "invalid.")
      return var

parseSingleFile :: FilePath -> IO Bool
parseSingleFile path = do
  fileText <- readFile path
  let parserResult = parserMain fileText
  case parserResult of
    Left _ -> do
      putStrLn $ "Error: Could not parse " <> path <> "."
      return False
    Right _ -> do
      putStrLn $ "Successfully parsed " <> path <> "."
      return True 

reportResults :: Int -> Int -> String -> IO ()
reportResults expected actual whichTestsStr = do
    if expected == actual then setSGR [SetColor Foreground Vivid Green] else setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "Expected total of " <> whichTestsStr <> " cases " <> show expected <> ", actual:" <> show actual 
    setSGR [Reset]


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
  rsposParse <- mapM parseSingleFile posParsefilePaths
  let trueParseCount = length $ filter id rsposParse
  print $ "Total correct pos parse: " ++ show trueParseCount

  -- Neg cases
  let negParsedirectory = "programs/negParse"
  negParseNames <- listDirectory negParsedirectory
  let negParsefilePaths = (negParsedirectory </>) <$> negParseNames
  rsnegParse <- mapM parseSingleFile negParsefilePaths
  let falseParseCount = (length negParsefilePaths) - (length $ filter id rsnegParse)
  print $ "Total correct neg parse: " ++ show falseParseCount


  reportResults (length posfilePaths) trueCount "pos"

  reportResults (length negfilePaths) falseCount "neg"

  reportResults (length posParsefilePaths) trueParseCount "pos parse"

  reportResults (length negParsefilePaths) falseParseCount "neg parse"