{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.ExpressionToDeclaration where

import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Common
import Test.Sandwich


spec :: TopSpec
spec = describe "ExpressionToDeclaration" $ do
  it "projects and transforms a single expression" $ do
    let (ls, ed@(ExpressionToDeclaration affectedLines)) = project (EDParams 10) ["putStrLn 42", "foo = 42"]
    ls `shouldBe` ["expr0000000000 = putStrLn 42", "foo = 42"]
    affectedLines `shouldBe` [0]

    transformAndUntransform (EDParams 10) (Position 0 0) (Position 0 17) ed
    transformAndUntransform (EDParams 10) (Position 0 1) (Position 0 18) ed
    transformAndUntransform (EDParams 10) (Position 1 0) (Position 1 0) ed

  it "projects and transforms a multiline expression" $ do
    let (ls, ed@(ExpressionToDeclaration affectedLines)) = project (EDParams 10) ["putStrLn [42", "  ]", "foo = 42"]
    ls `shouldBe` ["expr0000000000 = putStrLn [42"
                 , "                   ]"
                 , "foo = 42"]
    affectedLines `shouldBe` [0, 1]

    transformAndUntransform (EDParams 10) (Position 0 0) (Position 0 17) ed
    transformAndUntransform (EDParams 10) (Position 0 1) (Position 0 18) ed
    transformAndUntransform (EDParams 10) (Position 1 0) (Position 1 17) ed
    transformAndUntransform (EDParams 10) (Position 1 1) (Position 1 18) ed
    transformAndUntransform (EDParams 10) (Position 2 0) (Position 2 0) ed

  it "numbers expressions incrementally" $ do
    let (ls, ed@(ExpressionToDeclaration affectedLines)) = project (EDParams 10) ["putStrLn 42", "putStrLn 43", "foo = 42"]
    ls `shouldBe` ["expr0000000000 = putStrLn 42"
                 , "expr0000000001 = putStrLn 43"
                 , "foo = 42"]
    affectedLines `shouldBe` [0, 1]



main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
