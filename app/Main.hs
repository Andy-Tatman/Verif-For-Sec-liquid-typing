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
      -- print y
      var <- checker y
      -- print $ if var then "Valid" else "Invalid"
      putStrLn $ "The file " <> path <> (if var then " is valid." else " has NOT been proven valid.")
      return var

main :: IO ()
-- main = someFunc
-- main = print $ incr (-1)
-- main = do
--     print "Begin"
--     fileText <- readFile "test.txt" -- TODO: have this be a call argu
--     -- let parserResult = parserMain fileText
--     -- case parserResult of
--     --     Nothing -> print "Failed to parse provided string."
--     let parserResult = parserMain fileText
--     case parserResult of
--         Left x -> do -- ERROR
--             print "Error: Could not parse provided file."
--             print x
--             -- TODO: do next stuff
--         Right y -> do -- Parsed correctly
--             print "Succesfully parsed provided file!"
--             print y
--             var <- checker y
--             print $ if var then "Valid" else "Invalid"
--             -- print "Checker success"
main = do
  cfg <- readConfig
  rs <- mapM trySingleFile $ files cfg
  exitWith $ resultExit rs

-- print "End"