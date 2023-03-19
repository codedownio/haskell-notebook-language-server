
module Language.LSP.Diff where

import Data.String.Interpolate
import Data.Text
import qualified Data.Text.IO as T
import System.FilePath
import UnliftIO.Concurrent
import UnliftIO.Process
import UnliftIO.Temporary


main = do
  (exitCode, sout, serr) <- withSystemTempDirectory "haskell-notebook-diff" $ \dir -> do
    let path1 = dir </> "file1"
    let path2 = dir </> "file2"

    T.writeFile path1 file1
    T.writeFile path2 file2

    readCreateProcessWithExitCode (proc "git" ["diff", "--word-diff", path1, path2]) ""

  putStrLn [i|Got output: #{sout}|]
  putStrLn [i|Got err: #{serr}|]


file1 :: Text
file1 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       abc

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]

file2 :: Text
file2 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       abcd

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]
