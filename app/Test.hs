{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Test where

import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser


main :: IO ()
main = do
  let text = ghciCommand

  locatedCodeBlocks <- runGhc (Just GHC.Paths.libdir) $ parseString $ T.unpack text
  forM_ locatedCodeBlocks print


many :: Text
many =
  [__i|putStrLn $ "HI"
                <> "THERE"


       import Foo.Bar
       z
       putStrLn "THERE"
       foo = putStrLn "foo"
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

ghciCommand :: Text
ghciCommand =
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
