
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.BinarySearchVec where


import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Sandwich


spec :: TopSpec
spec = do
  it "zero element list" $ do
    binarySearchVec [] 5 `shouldBe` (0, False)

  it "one element list" $ do
    binarySearchVec [3] 0 `shouldBe` (0, False)
    binarySearchVec [3] 3 `shouldBe` (0, True)
    binarySearchVec [3] 5 `shouldBe` (1, False)

  it "two element list" $ do
    binarySearchVec [10, 20] 0 `shouldBe` (0, False)
    binarySearchVec [10, 20] 10 `shouldBe` (0, True)

    binarySearchVec [10, 20] 15 `shouldBe` (1, False)



main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
