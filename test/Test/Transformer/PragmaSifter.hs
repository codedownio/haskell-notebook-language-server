{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.PragmaSifter where

import GHC
import GHC.IO (unsafePerformIO)
import qualified GHC.Paths
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Common
import Test.Sandwich


{-# NOINLINE flagsToUse #-}
flagsToUse :: DynFlags
flagsToUse = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) getSessionDynFlags

spec :: TopSpec
spec = describe "PragmaSifter" $ do
  it "projects and transforms a single pragma" $ do
    (ls, sifter@(PragmaSifter indices)) <- project flagsToUse (listToDoc ["putStrLn 42", "{-# LANGUAGE Foo #-}"])
    ls `shouldBe` (listToDoc ["{-# LANGUAGE Foo #-}", "putStrLn 42"])
    indices `shouldBe` [1]

    transformAndUntransform flagsToUse (Position 0 0) (Position 1 0) sifter
    transformAndUntransform flagsToUse (Position 1 0) (Position 0 0) sifter

  it "projects and transforms two pragmas" $ do
    (ls, sifter@(PragmaSifter indices)) <- project flagsToUse (listToDoc ["putStrLn 42", "{-# LANGUAGE Bar #-}", "foo = 42", "{-# LANGUAGE Foo #-}", "bar = 24"])
    ls `shouldBe` (listToDoc ["{-# LANGUAGE Bar #-}", "{-# LANGUAGE Foo #-}", "putStrLn 42", "foo = 42", "bar = 24"])
    indices `shouldBe` [1, 3]

    -- TODO: write as cycle [0, 2, 3, 1]?
    transformAndUntransform flagsToUse (Position 0 0) (Position 2 0) sifter
    transformAndUntransform flagsToUse (Position 1 0) (Position 0 0) sifter
    transformAndUntransform flagsToUse (Position 2 0) (Position 3 0) sifter
    transformAndUntransform flagsToUse (Position 3 0) (Position 1 0) sifter
    transformAndUntransform flagsToUse (Position 4 0) (Position 4 0) sifter


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
