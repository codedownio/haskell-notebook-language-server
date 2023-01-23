{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.Util where

import Control.Lens hiding ((:>), (<.>))
import Control.Lens.Regex.Text
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Char as C
import qualified Data.List as L
import Data.Map as M
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath
import UnliftIO.MVar


whenNotebook :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> (Uri -> n a) -> n a
whenNotebook params = whenNotebook' (params ^. (textDocument . uri)) params

whenNotebookResult :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> c -> (Uri -> n c) -> n c
whenNotebookResult params = whenNotebook' (params ^. (textDocument . uri))

whenNotebook' :: (MonadLoggerIO n) => Uri -> a -> (Uri -> n a) -> n a
whenNotebook' uri params notebookParams = case parseURIReference (T.unpack (getUri uri)) of
  Nothing -> return params
  Just (URI {..}) -> do
    if | fmap C.toLower (takeExtension uriPath) == ".ipynb" -> notebookParams uri
       | otherwise -> return params

type TransformerMonad n = (
  MonadLoggerIO n
  , MonadReader TransformerState n
  , MonadUnliftIO n
  , MonadFail n
  )

-- * TransformerState

data DocumentState = DocumentState {
  transformer :: HaskellNotebookTransformer
  , curLines :: [Text]
  , newUri :: Uri
  , referenceRegex :: Regex
  }

data TransformerState = TransformerState {
  transformerDocuments :: MVar (Map Text DocumentState)
  }

newTransformerState :: (MonadIO m) => m TransformerState
newTransformerState = TransformerState
  <$> newMVar mempty


expressionToDeclarationParams :: Params ExpressionToDeclaration
expressionToDeclarationParams = EDParams 10

transformerParams :: Params HaskellNotebookTransformer
transformerParams = SDParams :> expressionToDeclarationParams :> ()

lookupTransformer :: TransformerMonad m => Uri -> m (Maybe DocumentState)
lookupTransformer uri = do
  TransformerState {..} <- ask
  M.lookup (getUri uri) <$> readMVar transformerDocuments

withTransformer :: (TransformerMonad n) => a -> (DocumentState -> n a) -> Uri -> n a
withTransformer def cb uri = do
  lookupTransformer uri >>= \case
    Nothing -> do
      logWarnN [i|Couldn't find expected transformer for uri #{uri}|]
      return def
    Just tx -> cb tx

modifyTransformer :: (TransformerMonad n) => a -> (DocumentState -> n (DocumentState, a)) -> Uri -> n a
modifyTransformer def cb uri = do
  TransformerState {..} <- ask
  modifyMVar transformerDocuments $ \m -> case M.lookup (getUri uri) m of
    Nothing -> return (m, def)
    Just tx -> do
      (tx', ret) <- cb tx
      return (M.insert (getUri uri) tx' m, ret)

addExtensionToUri :: (MonadLogger m) => String -> Uri -> m Uri
addExtensionToUri ext u@(Uri t) = case parseURIReference (T.unpack t) of
  Nothing -> do
    logErrorN [i|Couldn't parse URI: #{t}|]
    return u
  Just uri -> return $ Uri $ T.pack $ show $ uri { uriPath = uriPath uri <.> ext }
