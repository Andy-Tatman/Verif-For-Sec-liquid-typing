module Main (main) where

import Config
import Parse
import System.Directory (listDirectory)
import System.Exit
import System.FilePath ((</>))
import TCGen

resultExit :: [Bool] -> ExitCode
resultExit rs | and rs = ExitSuccess
resultExit _ = ExitFailure 1

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

  -- To end
  let rs = rspos ++ rsneg ++ rsposParse ++ rsnegParse
  exitWith $ resultExit rs