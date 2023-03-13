{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.ImportSifter where

import Language.LSP.Notebook.FrontSifter
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich


spec :: TopSpec
spec = describe "ImportSifter" $ do
  it "projects and transforms a single import" $ do
    let (ls, sifter@(ImportSifter indices)) = project () (listToDoc ["putStrLn 42", "import Bar"])
    ls `shouldBe` (listToDoc ["import Bar", "putStrLn 42"])
    indices `shouldBe` [1]

    transformAndUntransform () (Position 0 0) (Position 1 0) sifter
    transformAndUntransform () (Position 1 0) (Position 0 0) sifter

  it "projects and transforms two imports" $ do
    let (ls, sifter@(ImportSifter indices)) = project () (listToDoc ["putStrLn 42", "import Bar", "foo = 42", "import Foo", "bar = 24"])
    ls `shouldBe` (listToDoc ["import Bar", "import Foo", "putStrLn 42", "foo = 42", "bar = 24"])
    indices `shouldBe` [1, 3]

    -- TODO: write as cycle [0, 2, 3, 1]?
    transformAndUntransform () (Position 0 0) (Position 2 0) sifter
    transformAndUntransform () (Position 1 0) (Position 0 0) sifter
    transformAndUntransform () (Position 2 0) (Position 3 0) sifter
    transformAndUntransform () (Position 3 0) (Position 1 0) sifter
    transformAndUntransform () (Position 4 0) (Position 4 0) sifter


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
