import Test.Hspec
import Data.Ratio
import Fp4

main :: IO()
main = hspec $ do
    describe "seriesK" $ do
        it "returns take 3 $ seriesK 2" $
            (take 3 $ seriesK 2) `shouldBe` [1 % 1,1 % 2,1 % 4]
        it "returns take 4 $ seriesK 3" $
            (take 4 $ seriesK 3) `shouldBe` [1 % 1,1 % 3,1 % 9,1 % 27]
