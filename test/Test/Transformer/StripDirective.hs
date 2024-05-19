{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.StripDirective where

import GHC
import GHC.IO (unsafePerformIO)
import qualified GHC.Paths
import Language.LSP.Notebook.StripDirective
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich


{-# NOINLINE flagsToUse #-}
flagsToUse :: DynFlags
flagsToUse = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) getSessionDynFlags

spec :: TopSpec
spec = describe "StripDirective" $ do
  it "strips out GHCi directives" $ do
    (ls, ed@(StripDirective affectedLines)) <- project (SDParams flagsToUse) (listToDoc ["foo = 42", ":t foo"])
    ls `shouldBe` (listToDoc ["foo = 42", ""])
    affectedLines `shouldBe` [1]

    transformPosition (SDParams flagsToUse) ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition (SDParams flagsToUse) ed (Position 1 0) `shouldBe` (Just (Position 1 0))


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
