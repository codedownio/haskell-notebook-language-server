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
  it "strips out GHCi directives" $ do
    let (ls, ed@(StripDirective affectedLines)) = project SDParams ["foo = 42", ":t foo"]
    ls `shouldBe` ["foo = 42", ""]
    affectedLines `shouldBe` [1]

    transformPosition SDParams ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition SDParams ed (Position 1 0) `shouldBe` (Position 1 0)


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
