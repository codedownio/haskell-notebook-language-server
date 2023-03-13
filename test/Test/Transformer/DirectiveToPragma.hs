{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.DirectiveToPragma where

import Language.LSP.Notebook.DirectiveToPragma
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich


spec :: TopSpec
spec = describe "DirectiveToPragma" $ do
  it "Converts :set -XFoo directive to LANGUAGE pragma" $ do
    let (ls, ed@(DirectiveToPragma affectedLines)) = project DTPParams ["foo = 42", ":set -XFoo"]
    ls `shouldBe` ["foo = 42", "{-# LANGUAGE Foo #-}"]
    affectedLines `shouldBe` [1]

    transformPosition DTPParams ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition DTPParams ed (Position 1 0) `shouldBe` (Position 1 0)

  it "Converts :set -XFoo -XBar directive to LANGUAGE pragmas" $ do
    let (ls, ed@(DirectiveToPragma affectedLines)) = project DTPParams ["foo = 42", ":set -XFoo -XBar"]
    ls `shouldBe` ["foo = 42", "{-# LANGUAGE Foo Bar #-}"]
    affectedLines `shouldBe` [1]

    transformPosition DTPParams ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition DTPParams ed (Position 1 0) `shouldBe` (Position 1 0)


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
