import Test.Hspec
import Fp1

main :: IO()
main = hspec $ do
    describe "circShiftL" $ do
        it "returns empty list when given an empty list" $
            null (circShiftL 1 []) `shouldBe` True
        it "returns left shifted list when given positive n" $
            circShiftL 1 [1..4] `shouldBe` [2, 3, 4, 1]
        it "returns left shifted list when given positive n > 1" $
            circShiftL 2 [1..4] `shouldBe` [3, 4, 1, 2]
        it "returns left shifted list when given positive n > length of list" $
            circShiftL 5 [1..4] `shouldBe` [2, 3, 4, 1]
        it "returns right shifted list when given negative n" $
            circShiftL (-1) [1..4] `shouldBe` [4, 1, 2, 3]
        it "returns right shifted list when given negative n < -1" $
            circShiftL (-2) [1..4] `shouldBe` [3, 4, 1, 2]
        it "returns right shifted list when given negative n and abs(n) > length of list" $
            circShiftL (-5) [1..4] `shouldBe` [4, 1, 2, 3]
        it "returns same list when given zero n" $
            circShiftL 0 [1..4] `shouldBe` [1,2, 3, 4]
