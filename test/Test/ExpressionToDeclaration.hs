{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ExpressionToDeclaration where

import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Sandwich


spec :: TopSpec
spec = do
  it "projects and transforms a single expression" $ do
    let (ls, sifter@(ExpressionToDeclaration affectedLines)) = project (EDParams 10) ["putStrLn 42", "foo = 42"]
    ls `shouldBe` ["expr0000000000 = putStrLn 42", "foo = 42"]
    affectedLines `shouldBe` [0]

  it "projects and transforms a multiline expression" $ do
    let (ls, sifter@(ExpressionToDeclaration affectedLines)) = project (EDParams 10) ["putStrLn [42", "  ]", "foo = 42"]
    ls `shouldBe` ["expr0000000000 = putStrLn [42"
                 , "                   ]"
                 , "foo = 42"]
    affectedLines `shouldBe` [0, 1]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
