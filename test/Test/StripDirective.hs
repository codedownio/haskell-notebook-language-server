{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.StripDirective where

import Data.IntMap as IM hiding (toList)
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich
import Test.Sandwich.QuickCheck


spec :: TopSpec
spec = introduceQuickCheck $ do
  it "strips out GHCi directives" $ do
    let (ls, ed@(StripDirective affectedLines)) = project SDParams ["foo = 42", ":t foo"]
    ls `shouldBe` ["foo = 42", ""]
    affectedLines `shouldBe` (IM.fromList [(1, "foo = 42")])

    transformPosition SDParams ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition SDParams ed (Position 1 0) `shouldBe` (Position 1 0)

  prop "Does handleDiff for single line changes correctly" $ do
    testChange @StripDirective SDParams docLines <$> arbitrarySingleLineChange docLines

  prop "Does handleDiff for arbitrary changes correctly" $ do
    testChange @StripDirective SDParams docLines <$> arbitraryChange docLines


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
