{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.PragmaSifter where

import Language.LSP.Notebook.FrontSifter
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich


spec :: TopSpec
spec = describe "PragmaSifter" $ do
  it "projects and transforms a single pragma" $ do
    let (ls, sifter@(PragmaSifter indices)) = project () ["putStrLn 42", "{-# LANGUAGE Foo #-}"]
    ls `shouldBe` ["{-# LANGUAGE Foo #-}", "putStrLn 42"]
    indices `shouldBe` [1]

    transformAndUntransform () (Position 0 0) (Position 1 0) sifter
    transformAndUntransform () (Position 1 0) (Position 0 0) sifter

  it "projects and transforms two pragmas" $ do
    let (ls, sifter@(PragmaSifter indices)) = project () ["putStrLn 42", "{-# LANGUAGE Bar #-}", "foo = 42", "{-# LANGUAGE Foo #-}", "bar = 24"]
    ls `shouldBe` ["{-# LANGUAGE Bar #-}", "{-# LANGUAGE Foo #-}", "putStrLn 42", "foo = 42", "bar = 24"]
    indices `shouldBe` [1, 3]

    -- TODO: write as cycle [0, 2, 3, 1]?
    transformAndUntransform () (Position 0 0) (Position 2 0) sifter
    transformAndUntransform () (Position 1 0) (Position 0 0) sifter
    transformAndUntransform () (Position 2 0) (Position 3 0) sifter
    transformAndUntransform () (Position 3 0) (Position 1 0) sifter
    transformAndUntransform () (Position 4 0) (Position 4 0) sifter


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
