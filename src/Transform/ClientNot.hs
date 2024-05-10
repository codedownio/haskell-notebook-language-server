{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Transform.ClientNot where

import Control.Lens hiding ((:>), List)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Rope as Rope
import Data.Time
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Transform.ServerRsp.Hover (mkDocRegex)
import Transform.Util
import UnliftIO.MVar

#if MIN_VERSION_mtl(2,3,0)
import Control.Monad (when)
#endif


type ClientNotMethod m = SMethod (m :: Method 'ClientToServer 'Notification)

type SendExtraNotificationFn n = forall (o :: Method 'ClientToServer 'Notification). ToJSON (TNotificationMessage o) => TNotificationMessage o -> n ()

transformClientNot :: (
  TransformerMonad n, HasJSON (TNotificationMessage m)
  ) => SendExtraNotificationFn n
    -> ClientNotMethod m
    -> TNotificationMessage m
    -> n (TNotificationMessage m)
transformClientNot sendExtraNotification meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientNot' sendExtraNotification meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming client not #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientNot' :: (
  TransformerMonad n
  ) => (forall (o :: Method 'ClientToServer 'Notification). ToJSON (TNotificationMessage o) => TNotificationMessage o -> n ())
    -> ClientNotMethod m
    -> MessageParams m
    -> n (MessageParams m)

transformClientNot' _ SMethod_TextDocumentDidOpen params = whenNotebook params $ \u -> do
  let ls = Rope.fromText (params ^. (textDocument . text))
  TransformerState {..} <- ask
  let (ls', transformer' :: HaskellNotebookTransformer) = project (transformerParams (appConfigGhcLibPath transformerConfig)) ls
  newUri <- addExtensionToUri ".hs" u
  referenceRegex <- case uriToFilePath newUri of
    Just s -> pure $ mkDocRegex (T.pack s)
    Nothing -> do
      logWarnN [i|Couldn't convert URI to file path: '#{newUri}'|]
      pure $ mkDocRegex (getUri newUri)

  modifyMVar_ transformerDocuments $ \x -> return $! M.insert (getUri u) (
    DocumentState {
        transformer = transformer',
        curLines = ls,
        origUri = u,
        newUri = newUri,
        newPath = case uriToFilePath newUri of Nothing -> "ERROR_URI_PARSE_FAILURE"; Just x -> x,
        referenceRegex = referenceRegex
        }) x

  return $ params
         & set (textDocument . text) (Rope.toText ls')
         & set (textDocument . uri) newUri

transformClientNot' sendExtraNotification SMethod_TextDocumentDidChange params = whenNotebook params $ modifyTransformer params $ \ds@(DocumentState {transformer=tx, curLines=before, newUri, newPath}) -> do
  let changeEvents = params ^. contentChanges
  AppConfig {..} <- asks transformerConfig
  let (changeEvents', tx') = handleDiffMulti (transformerParams appConfigGhcLibPath) before changeEvents tx
  let after = applyChanges changeEvents before

  when appConfigWriteFileOnChange $ do
    whenServerCapabilitiesSatisfy supportsWillSave $ \_ ->
      sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentWillSave $ WillSaveTextDocumentParams {
        _textDocument = TextDocumentIdentifier newUri
        , _reason = TextDocumentSaveReason_AfterDelay
        }

    liftIO $ T.writeFile newPath (Rope.toText after)

    whenServerCapabilitiesSatisfy supportsSave $ \maybeSaveOptions ->
      sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentDidSave $ DidSaveTextDocumentParams {
        _textDocument = TextDocumentIdentifier newUri
        , _text = case maybeSaveOptions of
            Just (SaveOptions {_includeText=(Just True)}) -> Just (Rope.toText after)
            _ -> Nothing
        }

  return (ds { transformer = tx', curLines = after }, params & set contentChanges changeEvents'
                                                             & set (textDocument . uri) newUri)

transformClientNot' _ SMethod_TextDocumentDidClose params = whenNotebook params $ \u -> do
  TransformerState {..} <- ask
  maybeDocumentState <- modifyMVar transformerDocuments (return . flipTuple . M.updateLookupWithKey (\_ _ -> Nothing) (getUri u))
  newUri <- case maybeDocumentState of
    Just (DocumentState {..}) -> pure newUri
    Nothing -> addExtensionToUri ".hs" u -- The client shouldn't be closing a non-open doc
  return $ params
         & set (textDocument . uri) newUri

transformClientNot' _ _ params = return params
