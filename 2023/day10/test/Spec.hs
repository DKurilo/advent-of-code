import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "day10-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
