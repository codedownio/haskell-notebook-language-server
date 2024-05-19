{-# LANGUAGE OverloadedLists #-}

module Test.Hover where

import Data.String.Interpolate
import qualified Data.Text as T
import GHC
import GHC.IO (unsafePerformIO)
import qualified GHC.Paths
import Language.LSP.Notebook
import Language.LSP.Transformer
import Test.Sandwich
import Transform.ServerRsp.Hover


{-# NOINLINE flagsToUse #-}
flagsToUse :: DynFlags
flagsToUse = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) getSessionDynFlags

spec :: TopSpec
spec = describe "Hover" $ do
  describe "fixes up document references" $ do
    it "Basic case" $ do
      (_projected, transformer) <- project (transformerParams flagsToUse) (listToDoc hoverLines)
      fixupDocumentReferences' flagsToUse (mkDocRegex "main.ipynb") transformer "Defined at: main.ipynb:3:1"
        >>= (`shouldBe` "Defined at: main.ipynb:1:1")

    it "Works with a long path with spaces and stuff" $ do
      (_projected, transformer) <- project (transformerParams flagsToUse) (listToDoc hoverLines)
      let longPath = "/home/tom/tools/codedown-languages/tests/test_runs/2023-04-04T22_19_24.564991588Z/results/0_command line options/Introduce parallel semaphore/Tests/Parallel/claim semaphore/Haskell haskell-ghc8107/Haskell Nix environment/Jupyter runner/LSP/Hover/0_hovers foo/haskell-language-server-fc572b32aa084825/main.ipynb.hs"
      let regex = mkDocRegex longPath
      fixupDocumentReferences' flagsToUse regex transformer [i|Defined at: #{longPath}:3:1|]
        >>= (`shouldBe` [i|Defined at: #{longPath}:1:1|])

    it "Works with weird PCRE characters in the file name" $ do
      (_projected, transformer) <- project (transformerParams flagsToUse) (listToDoc hoverLines)
      fixupDocumentReferences' flagsToUse (mkDocRegex "ma$in.i?pynb") transformer "Defined at: ma$in.i?pynb:3:1"
        >>= (`shouldBe` "Defined at: ma$in.i?pynb:1:1")

hoverLines :: [T.Text]
hoverLines = [
  "foo = 42"
  , "putStrLn foo"
  , "import Data.Aeson"
  ]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
