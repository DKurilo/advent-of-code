import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "day16-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
