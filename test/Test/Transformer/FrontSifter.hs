{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.FrontSifter where

import Language.LSP.Notebook.FrontSifter
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich


spec :: TopSpec
spec = describe "FrontSifter" $ do
  it "projects and transforms a single import" $ do
    let (ls, sifter@(FrontSifter indices)) = project () ["putStrLn 42", "import Bar"]
    ls `shouldBe` ["import Bar", "putStrLn 42"]
    indices `shouldBe` [1]

    transformAndUntransform () (Position 0 0) (Position 1 0) sifter
    transformAndUntransform () (Position 1 0) (Position 0 0) sifter

  it "projects and transforms a two imports" $ do
    let (ls, sifter@(FrontSifter indices)) = project () ["putStrLn 42", "import Bar", "foo = 42", "import Foo", "bar = 24"]
    ls `shouldBe` ["import Bar", "import Foo", "putStrLn 42", "foo = 42", "bar = 24"]
    indices `shouldBe` [1, 3]

    -- TODO: write as cycle [0, 2, 3, 1]?
    transformAndUntransform () (Position 0 0) (Position 2 0) sifter
    transformAndUntransform () (Position 1 0) (Position 0 0) sifter
    transformAndUntransform () (Position 2 0) (Position 3 0) sifter
    transformAndUntransform () (Position 3 0) (Position 1 0) sifter
    transformAndUntransform () (Position 4 0) (Position 4 0) sifter


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
