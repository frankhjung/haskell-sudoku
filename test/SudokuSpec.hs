module SudokuSpec (spec) where

import           Sudoku     (boxs, choices, cols, group, rows, singleton,
                             ungroup, valid)

import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let m = ["534678912"
          ,"672195348"
          ,"198342567"
          ,"859761423"
          ,"426853791"
          ,"713924856"
          ,"961537284"
          ,"287419635"
          ,"345286179"
          ]
      b = ["123", "456", "789"]
      t = ["147", "258", "369"]

  describe "check properties" $ do
    it "rows leaves grid unchanged" $
      rows m `shouldBe` m
    it "applying rows twice leaves grid unchanged" $
      (rows . rows) m `shouldBe` m
    it "cols transposes cols to rows" $
      cols b `shouldBe` t
    it "applying cols twice leaves grid unchanged" $
      (cols . cols) m `shouldBe` m
    it "applying boxs twice leaves grid unchanged" $
      (boxs . boxs) m `shouldBe` m
    it "expect valid matrix" $
      valid m `shouldBe` True
    it "is a singleton" $
      singleton "3" `shouldBe` True
    it "not a singleton" $
      singleton "34" `shouldBe` False
    it "applying un/group leaves grid unchanged" $
      (ungroup . group) m `shouldBe` m
    it "applying un/group over both rows and columns leaves grid unchanged" $
      (map ungroup . ungroup . group . map group) m `shouldBe` m
    it "choices replace unknowns in row" $
      (concat . choices) ["406"] `shouldBe` ["4","123456789","6"]
