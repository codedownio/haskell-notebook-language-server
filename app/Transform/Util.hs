{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.Util where

import Control.Lens hiding ((:>))
import Control.Monad.Logger
import qualified Data.Char as C
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath


whenNotebook :: (MonadLoggerIO n, Lens.HasTextDocument a TextDocumentItem) => a -> n a -> n a
whenNotebook params notebookParams = case parseURIReference (T.unpack (getUri (params ^. (textDocument . uri)))) of
  Nothing -> do
    logInfoN [i|Failed to parse URI|]
    return params
  Just (URI {..}) -> do
    logInfoN [i|Got uriPath: #{uriPath}|]
    if | fmap C.toLower (takeExtension uriPath) == ".ipynb" -> notebookParams
       | fmap C.toLower (takeExtension uriPath) == ".ipynb.hs" -> notebookParams
       | otherwise -> return params
