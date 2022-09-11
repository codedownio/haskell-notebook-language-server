{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hover where

import Data.String.Interpolate
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich

import Transform.ClientRsp.Hover
import Transform.Util


spec :: TopSpec
spec = describe "Hover" $ do
  it "fixes up document references" $ do
    let (_, transformer) = project transformerParams ["foo = 42", "import Data.Aeson"]

    fixupDocumentReferences' "main.ipynb" transformer "Defined at: main.ipynb:1:1"
      `shouldBe` "Defined at: main.ipynb:0:1"


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
