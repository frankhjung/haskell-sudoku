module SudokuSpec (spec) where

import           Sudoku     (boxs, cols, rows, valid)

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
      b = ["123","456","789"]
      t = ["147","258","369"]

  describe "test sudoku functions " $ do
    it "rows = rows" $ rows m `shouldBe` m
    it "rows . rows = id" $ (rows . rows) m `shouldBe` m
    it "cols = transpose" $ cols b `shouldBe` t
    it "cols . cols = id" $ (cols . cols) m `shouldBe` m
    it "boxs . boxs = id" $ (boxs . boxs) m `shouldBe` m
    it "valid matrix" $ valid m `shouldBe` True
