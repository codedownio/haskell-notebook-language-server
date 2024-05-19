{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.ExpressionToDeclaration where

import GHC
import GHC.IO (unsafePerformIO)
import qualified GHC.Paths
import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Common
import Test.Sandwich


{-# NOINLINE flagsToUse #-}
flagsToUse :: DynFlags
flagsToUse = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) getSessionDynFlags

spec :: TopSpec
spec = describe "ExpressionToDeclaration" $ do
  it "projects and transforms a single expression" $ do
    (ls, ed@(ExpressionToDeclaration affectedLines)) <- project (EDParams 10 flagsToUse) (listToDoc ["putStrLn 42", "foo = 42"])
    ls `shouldBe` (listToDoc ["expr0000000000 = putStrLn 42", "foo = 42"])
    affectedLines `shouldBe` [0]

    transformAndUntransform (EDParams 10 flagsToUse) (Position 0 0) (Position 0 17) ed
    transformAndUntransform (EDParams 10 flagsToUse) (Position 0 1) (Position 0 18) ed
    transformAndUntransform (EDParams 10 flagsToUse) (Position 1 0) (Position 1 0) ed

  it "projects and transforms a multiline expression" $ do
    (ls, ed@(ExpressionToDeclaration affectedLines)) <- project (EDParams 10 flagsToUse) (listToDoc ["putStrLn [42", "  ]", "foo = 42"])
    ls `shouldBe` (listToDoc ["expr0000000000 = putStrLn [42"
                             , "                   ]"
                             , "foo = 42"])
    affectedLines `shouldBe` [0, 1]

    transformAndUntransform (EDParams 10 flagsToUse) (Position 0 0) (Position 0 17) ed
    transformAndUntransform (EDParams 10 flagsToUse) (Position 0 1) (Position 0 18) ed
    transformAndUntransform (EDParams 10 flagsToUse) (Position 1 0) (Position 1 17) ed
    transformAndUntransform (EDParams 10 flagsToUse) (Position 1 1) (Position 1 18) ed
    transformAndUntransform (EDParams 10 flagsToUse) (Position 2 0) (Position 2 0) ed

  it "numbers expressions incrementally" $ do
    (ls, (ExpressionToDeclaration affectedLines)) <- project (EDParams 10 flagsToUse) (listToDoc ["putStrLn 42", "putStrLn 43", "foo = 42"])
    ls `shouldBe` (listToDoc ["expr0000000000 = putStrLn 42"
                             , "expr0000000001 = putStrLn 43"
                             , "foo = 42"])
    affectedLines `shouldBe` [0, 1]



main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
