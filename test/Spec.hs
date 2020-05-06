import Test.Hspec
import Test.QuickCheck
import Rendering
import Logic
import Game
import Data.Array
main :: IO ()
main = hspec $ 
  describe "transposed" $ do
    it "should swap the elements of a tuple in a list" $do
      
      transposed [(1,2),(2,3)] `shouldBe` [(2,1),(3,2)]
      
    describe "solution" $ do
      it "should make a solution from a list of shapes" $ do
        solution [Just Square] `shouldBe` (array ((0, 0), (4,4)) $ zip (transposed (range ((0,0),(4,4))))([Just Square] ++ repeat Nothing))
        
