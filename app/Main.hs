module Main (main) where

import Lib
import Parse

main :: IO ()
-- main = someFunc
-- main = print $ incr (-1)
main = do
    print "Begin"
    parserMain "test.txt"
    print "End"