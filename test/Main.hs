{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Sandwich

import qualified Test.BinarySearchVec
import qualified Test.FrontendSifter

spec :: TopSpec
spec = do
  Test.BinarySearchVec.spec
  Test.FrontendSifter.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
