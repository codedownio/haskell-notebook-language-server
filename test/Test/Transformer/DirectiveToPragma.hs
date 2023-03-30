{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.DirectiveToPragma where

import Data.String.Interpolate
import Data.Text as T
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Notebook.DirectiveToPragma
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Generators


spec :: TopSpec
spec = describe "DirectiveToPragma" $ do
  it "Converts :set -XFoo directive to LANGUAGE pragma" $ do
    let (ls, dp@(DirectiveToPragma affectedLines)) = project DTPParams (listToDoc ["foo = 42", ":set -XFoo"])
    ls `shouldBe` (listToDoc ["foo = 42", "{-# LANGUAGE Foo #-}"])
    affectedLines `shouldBe` [1]

    transformPosition DTPParams dp (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition DTPParams dp (Position 1 0) `shouldBe` (Position 1 0)

  it "Converts :set -XFoo -XBar directive to LANGUAGE pragmas" $ do
    let (ls, dp@(DirectiveToPragma affectedLines)) = project DTPParams (listToDoc ["foo = 42", ":set -XFoo -XBar"])
    ls `shouldBe` (listToDoc ["foo = 42", "{-# LANGUAGE Foo Bar #-}"])
    affectedLines `shouldBe` [1]

    transformPosition DTPParams dp (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition DTPParams dp (Position 1 0) `shouldBe` (Position 1 0)

  it "Does a simple single line change" $ do
    let changes = [mkChange (4, 16) (4, 16) Nothing "2"]
    let (_projectedBefore, dp@(DirectiveToPragma affectedLines)) = project DTPParams docLines
    let before = docLines
    let (changes', _transformer') = handleDiffMulti DTPParams before changes dp
    changes' `shouldBe` changes

  describe "QuickCheck" $ introduceQuickCheck $ do
    prop "Does handleDiff for single line changes correctly" $ do
      testChange @DirectiveToPragma DTPParams docLines <$> arbitrarySingleLineChange docLines

doc :: Text
doc = [i|

:set -XFoo

foo = putStrLn 4

|]

docLines :: Rope
docLines = Rope.fromText doc

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
