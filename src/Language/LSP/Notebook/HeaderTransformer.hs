{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module Language.LSP.Notebook.HeaderTransformer where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


newtype HeaderTransformer = HeaderTransformer UInt
  deriving (Show)

instance Transformer HeaderTransformer where
  type Params HeaderTransformer = [Text]


  transformPosition (HeaderTransformer n) (Position l c) = Just $ Position (l + n) c
  untransformPosition (HeaderTransformer n) (Position l c) = Just $ Position (l - n) c

instance Monad m => ProjectIn m HeaderTransformer where
  project [] ls = pure (ls, HeaderTransformer 0, Right ())
  project initialLines ls = pure ((Rope.fromText $ T.intercalate "\n" initialLines) <> "\n" <> ls, HeaderTransformer (fromIntegral (L.length initialLines)), Right ())
