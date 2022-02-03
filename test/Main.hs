{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}


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



main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
