module Main (main) where

import Config
import Parse
import System.Exit
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
      -- ERROR
      putStrLn $ "Error: Could not parse " <> path <> "."
      print x
      return False
    Right y -> do
      -- Parsed correctly
      print "Succesfully parsed provided file!"
      var <- checker y
      putStrLn $ "The file " <> path <> (if var then " is valid." else " has NOT been proven valid.")
      return var

main :: IO ()
main = do
  cfg <- readConfig
  rs <- mapM trySingleFile $ files cfg
  exitWith $ resultExit rs
