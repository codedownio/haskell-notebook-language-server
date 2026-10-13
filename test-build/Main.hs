{-# LANGUAGE CPP #-}
module Main where

import Control.Monad
import Data.String.Interpolate
import System.Exit
import Test.Sandwich
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Process


targets :: [String]
targets = ["ghc92", "ghc94", "ghc96", "ghc98", "ghc910", "ghc912", "ghc914"]

allTargets :: [String]
allTargets =
  [x <> "-static" | x <- targets]
  <> ["packages.aarch64-darwin." <> x | x <- targets]
  <> ["packages.x86_64-darwin." <> x | x <- targets]

stackYamls :: [String]
stackYamls = [
  "stack/stack-9.2.8.yaml"
  , "stack/stack-9.4.8.yaml"
  , "stack/stack-9.6.7.yaml"
  , "stack/stack-9.8.4.yaml"
  , "stack/stack-9.10.2.yaml"
  , "stack/stack-9.12.2.yaml"
  , "stack/stack-9.14.1.yaml"
  ]

spec :: TopSpec
spec = parallel $ do
  describe "nix" $ parallelN 5 $ forM_ allTargets $ \target -> it target $ do
    dir <- getCurrentDirectory
    info [i|Current directory: #{dir}|]

    path <- getEnv "PATH"
    info [i|PATH: #{path}|]

    nix <- findExecutable "nix"
    info [i|nix: #{nix}|]

    p <- createProcessWithLogging (proc "nix" ["build", [i|.\##{target}|]])
    waitForProcess (handleOf p) >>= (`shouldBe` ExitSuccess)

  describe "stack" $ parallelN 5 $ forM_ stackYamls $ \stackYaml -> it stackYaml $ do
    dir <- getCurrentDirectory
    info [i|Current directory: #{dir}|]

    path <- getEnv "PATH"
    info [i|PATH: #{path}|]

    stack <- findExecutable "stack"
    info [i|stack: #{stack}|]

    p <- createProcessWithLogging (proc "stack" ["build", "--stack-yaml", stackYaml])
    waitForProcess (handleOf p) >>= (`shouldBe` ExitSuccess)


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec

-- | sandwich 0.3.1.0 made createProcessWithLogging also return the Async reading the output.
#if MIN_VERSION_sandwich(0,3,1)
handleOf :: (ProcessHandle, a) -> ProcessHandle
handleOf = fst
#else
handleOf :: ProcessHandle -> ProcessHandle
handleOf = id
#endif
