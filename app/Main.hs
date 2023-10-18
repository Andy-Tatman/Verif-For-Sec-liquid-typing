module Main (main) where

-- import Lib
import Parse

main :: IO ()
-- main = someFunc
-- main = print $ incr (-1)
main = do
    print "Begin"
    fileText <- readFile "test.txt" -- TODO: have this be a call argu
    -- let parserResult = parserMain fileText
    -- case parserResult of 
    --     Nothing -> print "Failed to parse provided string."
    let parserResult = parserMain fileText
    case parserResult of
        Left x -> do -- ERROR
            print "Error: Could not parse provided file."
            print x
            -- TODO: do next stuff
        Right y -> do -- Parsed correctly
            print "Succesfully parsed provided file!"
            print y

    print "End"