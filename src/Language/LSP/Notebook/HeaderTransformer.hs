{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module Language.LSP.Notebook.HeaderTransformer where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Transformer
import Language.LSP.Types


newtype HeaderTransformer = HeaderTransformer UInt
  deriving (Show)

instance Transformer HeaderTransformer where
  type Params HeaderTransformer = [Text]

  project [] ls = (ls, HeaderTransformer 0)
  project initialLines ls = ((Rope.fromText $ T.intercalate "\n" initialLines) <> "\n" <> ls, HeaderTransformer (fromIntegral (L.length initialLines)))

  transformPosition _ (HeaderTransformer n) (Position l c) = Just $ Position (l + n) c
  untransformPosition _ (HeaderTransformer n) (Position l c) = Position (l - n) c
