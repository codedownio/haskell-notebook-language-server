{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.StatementToDeclaration where

import Data.Map as M
import GHC
import GHC.IO (unsafePerformIO)
import qualified GHC.Paths
import Language.LSP.Notebook.StatementToDeclaration
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Common
import Test.Sandwich


{-# NOINLINE flagsToUse #-}
flagsToUse :: DynFlags
flagsToUse = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) getSessionDynFlags

spec :: TopSpec
spec = describe "StatementToDeclaration" $ do
  it "projects and transforms a single expression" $ do
    (ls, sd@(StatementToDeclaration affectedLines)) <- project (STDParams flagsToUse) (listToDoc ["foo <- readLn", "foo = 42"])
    ls `shouldBe` (listToDoc ["foo = unsafePerformIO $  readLn", "foo = 42"])
    affectedLines `shouldBe` (M.fromList [(0, LineInfo 4)])

    transformAndUntransform (STDParams flagsToUse) (Position 0 0) (Position 0 0) sd
    transformAndUntransform (STDParams flagsToUse) (Position 0 8) (Position 0 28) sd
    transformAndUntransform (STDParams flagsToUse) (Position 0 5) (Position 0 5) sd

  -- it "projects and transforms a multiline expression" $ do
  --   (ls, sd@(StatementToDeclaration affectedLines)) <- project (STDParams flagsToUse) (listToDoc ["putStrLn [42", "  ]", "foo = 42"])
  --   ls `shouldBe` (listToDoc ["expr0000000000 = putStrLn [42"
  --                            , "                   ]"
  --                            , "foo = 42"])
  --   affectedLines `shouldBe` [0, 1]

  --   transformAndUntransform (STDParams flagsToUse) (Position 0 0) (Position 0 17) sd
  --   transformAndUntransform (STDParams flagsToUse) (Position 0 1) (Position 0 18) sd
  --   transformAndUntransform (STDParams flagsToUse) (Position 1 0) (Position 1 17) sd
  --   transformAndUntransform (STDParams flagsToUse) (Position 1 1) (Position 1 18) sd
  --   transformAndUntransform (STDParams flagsToUse) (Position 2 0) (Position 2 0) sd



main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
