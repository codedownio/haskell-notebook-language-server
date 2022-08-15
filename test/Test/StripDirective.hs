{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.StripDirective where

import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich


spec :: TopSpec
spec = do
  it "strips out GHCi commands" $ do
    let (ls, ed@(StripDirective affectedLines)) = project SDParams ["foo = 42", ":t foo"]
    ls `shouldBe` ["foo = 42", ""]
    affectedLines `shouldBe` [1]

    -- transformAndUntransform (EDParams 10) (Position 0 0) (Position 0 17) ed
    -- transformAndUntransform (EDParams 10) (Position 0 1) (Position 0 18) ed
    -- transformAndUntransform (EDParams 10) (Position 1 0) (Position 1 0) ed



main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
