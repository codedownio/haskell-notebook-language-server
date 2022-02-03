{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.FrontendSifter where

import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Sandwich


spec :: TopSpec
spec = do
  it "projects and transforms a single import" $ do
    let (ls, sifter@(FrontSifter indices)) = project () ["putStrLn 42", "import Bar"]
    ls `shouldBe` ["import Bar", "putStrLn 42"]
    indices `shouldBe` [1]

    transformPosition () sifter (Position 0 0) `shouldBe` (Just (Position 1 0))
    transformPosition () sifter (Position 1 0) `shouldBe` (Just (Position 0 0))

    untransformPosition () sifter (Position 0 0) `shouldBe` (Position 1 0)
    untransformPosition () sifter (Position 1 0) `shouldBe` (Position 0 0)

  it "projects and transforms a two imports" $ do
    let (ls, sifter@(FrontSifter indices)) = project () ["putStrLn 42", "import Bar", "foo = 42", "import Foo", "bar = 24"]
    ls `shouldBe` ["import Bar", "import Foo", "putStrLn 42", "foo = 42", "bar = 24"]
    indices `shouldBe` [1, 3]

    transformPosition () sifter (Position 0 0) `shouldBe` (Just (Position 2 0))
    transformPosition () sifter (Position 1 0) `shouldBe` (Just (Position 0 0))


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
