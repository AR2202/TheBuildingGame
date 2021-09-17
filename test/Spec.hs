import           Data.Array
import           Game
import           Logic
import           Rendering
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main =
  hspec $
  describe "transposed" $ do
    it "should swap the elements of a tuple in a list" $
      property $ \tuplelist ->
        transposed (transposed tuplelist) == (tuplelist :: [(Int, Int)])
    describe "solution" $ do
      it "should make a solution from a list of shapes" $ do
        solution [Just Square] `shouldBe`
          (array ((0, 0), (4, 4)) $
           zip
             (transposed (range ((0, 0), (4, 4))))
             ([Just Square] ++ repeat Nothing))
