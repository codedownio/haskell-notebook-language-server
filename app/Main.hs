{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Sequence hiding (zip)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook
import Language.LSP.Transformer
import Lib
import System.IO.Unsafe (unsafePerformIO)


main :: IO ()
main = do
  let text = reorder

  locatedCodeBlocks <- runGhc (Just GHC.Paths.libdir) $ parseString $ T.unpack text
  putStrLn [i|Got parsed: #{locatedCodeBlocks}|]

  let projected@(ls, FrontSifter indices) = project () (T.splitOn "\n" text)
  putStrLn [i|Got projected: #{T.intercalate "\n" ls}|]


many :: String
many =
  [__i|putStrLn $ "HI"
                <> "THERE"


       import Foo.Bar
       z
       putStrLn "THERE"
      |]

reorder :: Text
reorder =
  [__i|foo = putStrLn $ "HI" <> "THERE"
       {-\# LANGUAGE RankNTypes \#-}
       import Foo.Bar
       import Foo.Baz (
         FooBar
         )
       bar = 42
      |]
