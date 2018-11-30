import Prelude hiding (Monoid,mappend)
import Test.Hspec
import Fp5
import Fp6

main :: IO()
main = hspec $ do
    describe "msort" $ do
        it "sort list of one item" $
            (msort [1]) `shouldBe` SortedList {getSorted = [1]}
        it "sort list of two items" $
            (msort [2,1]) `shouldBe` SortedList {getSorted = [1,2]}
        it "sort list of ordered items" $
            (msort [1,2,3,4,5]) `shouldBe` SortedList {getSorted = [1,2,3,4,5]}
        it "sort list of unordered items" $
            (msort [5,3,2,1,4]) `shouldBe` SortedList {getSorted = [1,2,3,4,5]}
