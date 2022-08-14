{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.Util where

import Control.Lens hiding ((:>))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Char as C
import Data.Map as M
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath
import UnliftIO.MVar


whenNotebook :: (MonadLoggerIO n, HasTextDocument a TextDocumentItem) => a -> n a -> n a
whenNotebook params notebookParams = whenNotebook'' (params ^. (textDocument . uri)) params notebookParams

whenNotebook' :: (MonadLoggerIO n, HasTextDocument a TextDocumentIdentifier) => a -> n a -> n a
whenNotebook' params notebookParams = whenNotebook'' undefined params notebookParams

whenNotebook'' :: (MonadLoggerIO n) => Uri -> a -> n a -> n a
whenNotebook'' uri params notebookParams = case parseURIReference (T.unpack (getUri uri)) of
  Nothing -> do
    logInfoN [i|Failed to parse URI|]
    return params
  Just (URI {..}) -> do
    logInfoN [i|Got uriPath: #{uriPath}|]
    if | fmap C.toLower (takeExtension uriPath) == ".ipynb" -> notebookParams
       | fmap C.toLower (takeExtension uriPath) == ".ipynb.hs" -> notebookParams
       | otherwise -> return params

type TransformerMonad n = (MonadLoggerIO n, MonadReader TransformerState n, MonadUnliftIO n)

-- * TransformerState

data TransformerState = TransformerState {
  transformerDocuments :: MVar (Map Text SomeTransformer)
  }

newTransformerState :: (MonadIO m) => m TransformerState
newTransformerState = TransformerState
  <$> newMVar mempty

transformerParams = (EDParams 10) :> ()
