{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.DirectiveToPragma where

import ApplyChanges
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Notebook.DirectiveToPragma
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich
import TestLib.Generators


spec :: TopSpec
spec = describe "DirectiveToPragma" $ do
  it "Converts :set -XFoo directive to LANGUAGE pragma" $ do
    let (ls, dp@(DirectiveToPragma affectedLines)) = project DTPParams ["foo = 42", ":set -XFoo"]
    ls `shouldBe` ["foo = 42", "{-# LANGUAGE Foo #-}"]
    affectedLines `shouldBe` [1]

    transformPosition DTPParams dp (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition DTPParams dp (Position 1 0) `shouldBe` (Position 1 0)

  it "Converts :set -XFoo -XBar directive to LANGUAGE pragmas" $ do
    let (ls, dp@(DirectiveToPragma affectedLines)) = project DTPParams ["foo = 42", ":set -XFoo -XBar"]
    ls `shouldBe` ["foo = 42", "{-# LANGUAGE Foo Bar #-}"]
    affectedLines `shouldBe` [1]

    transformPosition DTPParams dp (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition DTPParams dp (Position 1 0) `shouldBe` (Position 1 0)

  it "Does a simple single line change" $ do
    let changes = [mkChange (4, 16) (4, 16) (Just 0) "2"]
    let (_projectedBefore, dp@(DirectiveToPragma affectedLines)) = project DTPParams docLines
    let before = docLines
    let after = applyChangesTextSilent changes docLines
    let (before', after', changes', _transformer') = handleDiff DTPParams before after changes dp
    changes' `shouldBe` changes

doc :: Text
doc = [i|

:set -XFoo

foo = putStrLn 4

|]

docLines :: [Text]
docLines = T.splitOn "\n" doc

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec