import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "day20-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
