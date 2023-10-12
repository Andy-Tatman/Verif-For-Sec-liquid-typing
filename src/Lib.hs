module Lib
    ( someFunc, incr
    ) where


someFunc :: IO ()
someFunc = putStrLn "someFunc"

incr :: Int -> Int
incr x = x + 1
