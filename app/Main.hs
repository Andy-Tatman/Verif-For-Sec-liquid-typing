module Main (main) where

-- import Lib
import Parse
import TCGen
import Config 
import System.Exit

resultExit :: [Bool] -> ExitCode
resultExit rs | and rs = ExitSuccess
resultExit _ = ExitFailure 1

trySingleFile :: FilePath -> IO Bool
trySingleFile path = do
    fileText <- readFile path 
    let parserResult = parserMain fileText
    case parserResult of
        Left x -> do -- ERROR
            print "Error: Could not parse provided file."
            print x
            return False
        Right y -> do -- Parsed correctly
            print "Succesfully parsed provided file!"
            -- print y
            var <- checker y
            -- print $ if var then "Valid" else "Invalid"
            putStrLn $ "The file " <> path <> " is "<> (if var then "valid." else "invalid.") 
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