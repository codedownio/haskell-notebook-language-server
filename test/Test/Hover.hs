{-# LANGUAGE OverloadedLists #-}

module Test.Hover where

import Language.LSP.Notebook
import Language.LSP.Transformer
import Test.Sandwich
import Transform.ClientRsp.Hover


spec :: TopSpec
spec = describe "Hover" $ do
  describe "fixes up document references" $ do
    let (_, transformer) = project transformerParams (listToDoc ["foo = 42", "import Data.Aeson"])

    it "Basic case" $ do
      fixupDocumentReferences' (mkDocRegex "main.ipynb") transformer "Defined at: main.ipynb:1:1"
        `shouldBe` "Defined at: main.ipynb:0:1"

    it "Works with weird PCRE characters in the file name" $ do
      fixupDocumentReferences' (mkDocRegex "ma$in.i?pynb") transformer "Defined at: ma$in.i?pynb:1:1"
        `shouldBe` "Defined at: ma$in.i?pynb:0:1"


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
