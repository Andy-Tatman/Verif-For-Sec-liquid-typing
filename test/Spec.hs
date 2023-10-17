import Control.Monad
import Control.Monad.Trans.Maybe
import Grade
import qualified ParseSpec
import Test.HUnit
import Test.Hrubric
import Test.Hspec

rubric :: Rubric
rubric = do
  criterion "It compiles" (1 / 10) . passOrFail $
    it "..." $
      True @?= True
  criterion "Parse" (9 / 10) ParseSpec.rubric

main :: IO ()
main = void . runMaybeT $ do
  result <- MaybeT $ hrubric rubric
  pretty result
  autograde result
