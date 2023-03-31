{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.ClientNot where

import Control.Lens hiding ((:>), List)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Map as M
import Data.String.Interpolate
import qualified Data.Text.Rope as Rope
import Data.Time
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.ClientRsp.Hover (mkDocRegex)
import Transform.Util
import UnliftIO.MVar


type ClientNotMethod m = SMethod (m :: Method FromClient Notification)


transformClientNot :: (TransformerMonad n, HasJSON (NotificationMessage m)) => ClientNotMethod m -> NotificationMessage m -> n (NotificationMessage m)
transformClientNot meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientNot' meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logInfoN [i|Transforming client not #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientNot' :: (TransformerMonad n) => ClientNotMethod m -> MessageParams m -> n (MessageParams m)

transformClientNot' STextDocumentDidOpen params = whenNotebook params $ \u -> do
  let ls = Rope.fromText (params ^. (textDocument . text))
  let (ls', transformer' :: HaskellNotebookTransformer) = project transformerParams ls
  TransformerState {..} <- ask
  Uri newUri <- addExtensionToUri ".hs" u
  modifyMVar_ transformerDocuments (\x -> return $! M.insert (getUri u) (DocumentState transformer' ls u (Uri newUri) (mkDocRegex newUri)) x)
  return $ params
         & set (textDocument . text) (Rope.toText ls')
         & set (textDocument . uri) (Uri newUri)
transformClientNot' STextDocumentDidChange params = whenNotebook params $ modifyTransformer params $ \ds@(DocumentState {transformer=tx, curLines=before, newUri}) -> do
  let (List changeEvents) = params ^. contentChanges
  let (changeEvents', tx') = handleDiffMulti transformerParams before changeEvents tx
  let after = applyChanges changeEvents before
  return (ds { transformer = tx', curLines = after }, params & set contentChanges (List changeEvents')
                                                             & set (textDocument . uri) newUri)
transformClientNot' STextDocumentDidClose params = whenNotebook params $ \u -> do
  TransformerState {..} <- ask
  maybeDocumentState <- modifyMVar transformerDocuments (return . flipTuple . M.updateLookupWithKey (\_ _ -> Nothing) (getUri u))
  newUri <- case maybeDocumentState of
    Just (DocumentState {..}) -> pure newUri
    Nothing -> addExtensionToUri ".hs" u -- The client shouldn't be closing a non-open doc
  return $ params
         & set (textDocument . uri) newUri

transformClientNot' _ params = return params
