-- import Control.Monad
-- import Control.Monad.Trans.Maybe
-- import Grade
-- import qualified ParseSpec
-- import Test.HUnit
-- import Test.Hrubric
-- import Test.Hspec
-- rubric :: Rubric
-- rubric = do
--   criterion "It compiles" (1 / 10) . passOrFail $
--     it "..." $
--       True @?= True
--   criterion "Parse" (9 / 10) ParseSpec.rubric
-- main :: IO ()
-- main = void . runMaybeT $ do
--   result <- MaybeT $ hrubric rubric
--   pretty result
--   autograde result
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main (main) where

-- Import modules defining HTF tests like this:
import {-@ HTF_TESTS @-} ParseSpec

main :: IO ()
main = htfMain htf_importedTests -- all tests in modules imported via {-@ HTF_TESTS @-} are available in htf_importedTests