{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.StripDirective where

import qualified GHC.Paths
import Language.LSP.Notebook.StripDirective
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich


spec :: TopSpec
spec = describe "StripDirective" $ do
  it "strips out GHCi directives" $ do
    let (ls, ed@(StripDirective affectedLines)) = project (SDParams GHC.Paths.libdir) (listToDoc ["foo = 42", ":t foo"])
    ls `shouldBe` (listToDoc ["foo = 42", ""])
    affectedLines `shouldBe` [1]

    transformPosition (SDParams GHC.Paths.libdir) ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition (SDParams GHC.Paths.libdir) ed (Position 1 0) `shouldBe` (Just (Position 1 0))


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
