import Test.Hspec
import Fp2

main :: IO()
main = hspec $ do
    describe "indices" $ do
        it "returns empty list when given an empty list" $
            null (indices []) `shouldBe` True
        it "returns pairs (index,item) when given [Int]" $
            indices [1,2,3] `shouldBe` [(0,1),(1,2),(2,3)]
        it "returns pairs (index,item) when given [Char]" $
            indices ['a','b','c'] `shouldBe` [(0,'a'),(1,'b'),(2,'c')]

    describe "zeroBy" $ do
        it "replaces the values which are not meet a condition test 1" $
            zeroBy ["a","ab","abc","abcd"] (\x -> (length x) < 3) `shouldBe` ["a","ab","",""]
        it "replaces the values which are not meet a condition test 2" $
            zeroBy ["a","ab","abc","abcd"] (\x -> (length x) > 2) `shouldBe` ["","","abc","abcd"]

    describe "triplewiseSum" $ do
        it "returns empty list when given an empty lists" $
            null (triplewiseSum [] [] []) `shouldBe` True
        it "returns empty list when given at least one empty list" $
            null (triplewiseSum [] [1] [2]) `shouldBe` True
        it "returns sum with lists of equal length" $
            triplewiseSum [1,2] [2,3] [3,4] `shouldBe` [6,9]
        it "returns sum with lists of inequal length" $
            triplewiseSum [1,2] [2,3,4] [3,4,5,6] `shouldBe` [6,9]
