
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.BinarySearchVec where


import Language.LSP.Notebook.FrontSifter
import Language.LSP.Notebook.FrontSifter.Util
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Sandwich


spec :: TopSpec
spec = describe "binary search in vector" $ do
  it "zero element list" $ do
    binarySearchVec [] (-5) `shouldBe` (0, False)
    binarySearchVec [] 5 `shouldBe` (0, False)

  it "one element list" $ do
    binarySearchVec [3] 0 `shouldBe` (0, False)
    binarySearchVec [3] 3 `shouldBe` (0, True)
    binarySearchVec [3] 5 `shouldBe` (1, False)

  it "two element list" $ do
    binarySearchVec [10, 20] 0 `shouldBe` (0, False)
    binarySearchVec [10, 20] 10 `shouldBe` (0, True)

    binarySearchVec [10, 20] 15 `shouldBe` (1, False)
    binarySearchVec [10, 20] 20 `shouldBe` (1, True)

    binarySearchVec [10, 20] 25 `shouldBe` (2, False)

  it "three element list" $ do
    binarySearchVec [10, 20, 30] 0 `shouldBe` (0, False)
    binarySearchVec [10, 20, 30] 10 `shouldBe` (0, True)

    binarySearchVec [10, 20, 30] 15 `shouldBe` (1, False)
    binarySearchVec [10, 20, 30] 20 `shouldBe` (1, True)

    binarySearchVec [10, 20, 30] 25 `shouldBe` (2, False)
    binarySearchVec [10, 20, 30] 30 `shouldBe` (2, True)

    binarySearchVec [10, 20, 30] 35 `shouldBe` (3, False)


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
