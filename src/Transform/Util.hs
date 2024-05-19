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
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import GHC (DynFlags)
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Transformer
import Network.URI
import System.FilePath
import UnliftIO.MVar


whenNotebook :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> (Uri -> n a) -> n a
whenNotebook params = whenNotebook' (params ^. (textDocument . uri)) params

whenNotebookUri :: (MonadLoggerIO n, HasUri a Uri) => a -> (Uri -> n a) -> n a
whenNotebookUri params = whenNotebook' (params ^. uri) params

whenNotebookByInitialParams :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> c -> (Uri -> n c) -> n c
whenNotebookByInitialParams params = whenNotebook' (params ^. (textDocument . uri))

whenNotebook' :: (MonadLoggerIO n) => Uri -> a -> (Uri -> n a) -> n a
whenNotebook' uri params notebookParams = case parseURIReference (T.unpack (getUri uri)) of
  Nothing -> return params
  Just (URI {..}) -> do
    if | ".ipynb" `L.isSuffixOf` fmap C.toLower uriPath -> notebookParams uri
       | otherwise -> return params

-- * whenNotebookResult

whenNotebookResultUri :: (MonadLoggerIO n, HasUri a Uri) => a -> (Uri -> n a) -> n a
whenNotebookResultUri params = whenNotebookResult' (params ^. uri) params

-- | Note that this takes in server URIs (.ipynb.hs) and calls the callback with original URIs (.ipynb)
-- TODO: do a lot less String/Text conversion here
whenNotebookResult' :: (MonadLoggerIO n) => Uri -> a -> (Uri -> n a) -> n a
whenNotebookResult' (Uri uriText) params notebookParams = case parseURIReference (T.unpack uriText) of
  Nothing -> return params
  Just networkUri@(URI {..}) -> do
    if | ".ipynb.hs" `L.isSuffixOf` fmap C.toLower uriPath -> do
           let correctedNetworkUri = networkUri { uriPath = T.unpack (T.dropEnd 3 (T.pack uriPath)) }
           notebookParams (Uri (T.pack (uriToString Prelude.id correctedNetworkUri "")))
       | otherwise -> return params

-- * TransformerMonad

type TransformerMonad n = (
  MonadLoggerIO n
  , MonadReader TransformerState n
  , MonadUnliftIO n
  , MonadFail n
  )

-- * TransformerState

data DocumentState = DocumentState {
  transformer :: HaskellNotebookTransformer
  , curLines :: Doc
  , origUri :: Uri
  , newUri :: Uri
  , newPath :: FilePath
  , referenceRegex :: Regex
  }

data AppConfig = AppConfig {
  appConfigWriteFileOnChange :: Bool
  , appConfigDynFlags :: DynFlags
  }

data TransformerState = TransformerState {
  transformerDocuments :: MVar (M.Map Text DocumentState)
  , transformerInitializeParams :: MVar (Maybe InitializeParams)
  , transformerInitializeResult :: MVar (Maybe InitializeResult)
  , transformerConfig :: AppConfig
  }

-- * Transformers

newTransformerState :: (MonadIO m) => AppConfig -> m TransformerState
newTransformerState config = TransformerState
  <$> newMVar mempty
  <*> newMVar Nothing
  <*> newMVar Nothing
  <*> pure config

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

-- * Checking server capabilities

whenServerCapabilitiesSatisfy :: TransformerMonad n => (ServerCapabilities -> (Bool, a)) -> (a -> n ()) -> n ()
whenServerCapabilitiesSatisfy cb action = do
  initializeResultVar <- asks transformerInitializeResult
  readMVar initializeResultVar >>= \case
    Just ((cb . (^. capabilities)) -> (True, sideValue)) -> action sideValue
    _ -> return ()

supportsWillSave :: ServerCapabilities -> (Bool, ())
supportsWillSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_willSave=(Just True)}))) }) = (True, ())
supportsWillSave _ = (False, ())

supportsSave :: ServerCapabilities -> (Bool, Maybe SaveOptions)
supportsSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_save}))) }) = case _save of
  Nothing -> (False, Nothing)
  Just (InL x) -> (x, Nothing)
  Just (InR saveOptions) -> (True, Just saveOptions)
supportsSave _ = (False, Nothing)

-- * Misc

addExtensionToUri :: (MonadLogger m) => String -> Uri -> m Uri
addExtensionToUri ext u@(Uri t) = case parseURIReference (T.unpack t) of
  Nothing -> do
    logErrorN [i|Couldn't parse URI: #{t}|]
    return u
  Just uri -> return $ Uri $ T.pack $ show $ uri { uriPath = uriPath uri <.> ext }

flipTuple :: (b, a) -> (a, b)
flipTuple (x, y) = (y, x)
