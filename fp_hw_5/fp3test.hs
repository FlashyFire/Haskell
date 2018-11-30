import Test.Hspec
import Fp3

main :: IO()
main = hspec $ do
    describe "revRange" $ do
        it "returns single char when given one char range" $
            revRange ('a','a') `shouldBe` "a"
        it "returns reverse range of (a,b)" $
            revRange ('a','b') `shouldBe` "ba"
        it "returns reverse range of (a,c)" $
            revRange ('a','c') `shouldBe` "cba"
        it "returns reverse range of (k,n)" $
            revRange ('k','n') `shouldBe` "nmlk"
        it "returns reverse range of ('\NUL','\SOH')" $
            revRange ('\NUL','\SOH') `shouldBe` "\SOH\NUL"
