import Test.Hspec
import Fp5

main :: IO()
main = hspec $ do
    describe "sorted list monoid" $ do
        it "merge empty list with sorted list" $
            ((SortedList []) `mappend` (SortedList [2,4,6])) `shouldBe` SortedList {getSorted = [2,4,6]}
        it "merge sorted list with empty list" $
            ((SortedList [1,3,5]) `mappend` (SortedList [])) `shouldBe` SortedList {getSorted = [1,3,5]}
        it "merge two sorted lists" $
            ((SortedList [1,3,5]) `mappend` (SortedList [2,4,6])) `shouldBe` SortedList {getSorted = [1,2,3,4,5,6]}
        it "merge two equal sorted lists" $
            ((SortedList [1,3,5]) `mappend` (SortedList [1,3,5])) `shouldBe` SortedList {getSorted = [1,1,3,3,5,5]}
        it "merge two sorted lists of inequal length" $
            ((SortedList [1,3,5,7]) `mappend` (SortedList [2,4,6])) `shouldBe` SortedList {getSorted = [1,2,3,4,5,6,7]}
