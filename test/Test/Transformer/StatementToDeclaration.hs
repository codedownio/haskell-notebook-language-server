{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.StatementToDeclaration where

import Data.Map as M
import qualified GHC.Paths
import Language.LSP.Notebook.StatementToDeclaration
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Common
import Test.Sandwich


spec :: TopSpec
spec = describe "StatementToDeclaration" $ do
  it "projects and transforms a single expression" $ do
    let (ls, sd@(StatementToDeclaration affectedLines)) = project (STDParams GHC.Paths.libdir) (listToDoc ["foo <- readLn", "foo = 42"])
    ls `shouldBe` (listToDoc ["foo = unsafePerformIO $  readLn", "foo = 42"])
    affectedLines `shouldBe` (M.fromList [(0, LineInfo 4)])

    transformAndUntransform (STDParams GHC.Paths.libdir) (Position 0 0) (Position 0 0) sd
    transformAndUntransform (STDParams GHC.Paths.libdir) (Position 0 8) (Position 0 28) sd
    transformAndUntransform (STDParams GHC.Paths.libdir) (Position 0 5) (Position 0 5) sd

  -- it "projects and transforms a multiline expression" $ do
  --   let (ls, sd@(StatementToDeclaration affectedLines)) = project (STDParams GHC.Paths.libdir) (listToDoc ["putStrLn [42", "  ]", "foo = 42"])
  --   ls `shouldBe` (listToDoc ["expr0000000000 = putStrLn [42"
  --                            , "                   ]"
  --                            , "foo = 42"])
  --   affectedLines `shouldBe` [0, 1]

  --   transformAndUntransform (STDParams GHC.Paths.libdir) (Position 0 0) (Position 0 17) sd
  --   transformAndUntransform (STDParams GHC.Paths.libdir) (Position 0 1) (Position 0 18) sd
  --   transformAndUntransform (STDParams GHC.Paths.libdir) (Position 1 0) (Position 1 17) sd
  --   transformAndUntransform (STDParams GHC.Paths.libdir) (Position 1 1) (Position 1 18) sd
  --   transformAndUntransform (STDParams GHC.Paths.libdir) (Position 2 0) (Position 2 0) sd



main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
