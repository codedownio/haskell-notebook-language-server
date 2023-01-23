{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.ClientNot where

import ApplyChanges
import Control.Lens hiding ((:>), List)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.Char as C
import Data.Map as M
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Utf16.Lines as Lines
import qualified Data.Text.Utf16.Rope as Rope
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath
import Transform.ClientRsp.Hover (mkDocRegex)
import Transform.Util
import UnliftIO.MVar


type ClientNotMethod m = SMethod (m :: Method FromClient Notification)


transformClientNot :: (TransformerMonad n, HasJSON (NotificationMessage m)) => ClientNotMethod m -> NotificationMessage m -> n (NotificationMessage m)
transformClientNot meth msg = do
  p' <- transformClientNot' meth (msg ^. params)
  let msg' = set params p' msg
  when (msg' /= msg) $ logInfoN [i|Transforming client not #{meth}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientNot' :: (TransformerMonad n) => ClientNotMethod m -> MessageParams m -> n (MessageParams m)

transformClientNot' STextDocumentDidOpen params = whenNotebook params $ \u -> do
  let ls = T.lines (params ^. (textDocument . text))
  let (ls', transformer' :: HaskellNotebookTransformer) = project transformerParams ls
  TransformerState {..} <- ask
  Uri newUri <- addExtensionToUri ".hs" u
  modifyMVar_ transformerDocuments (\x -> return $! M.insert (getUri u) (DocumentState transformer' ls u (Uri newUri) (mkDocRegex newUri)) x)
  return $ params
         & set (textDocument . text) (T.intercalate "\n" ls')
         & set (textDocument . uri) (Uri newUri)
transformClientNot' STextDocumentDidChange params = whenNotebook params $ modifyTransformer params $ \ds@(DocumentState {transformer=tx, curLines=before, newUri}) -> do
  let (List changeEvents) = params ^. contentChanges
  after <- applyChangesText changeEvents before
  let (_before', _after', changeEvents', tx') = handleDiff transformerParams before after changeEvents tx
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
