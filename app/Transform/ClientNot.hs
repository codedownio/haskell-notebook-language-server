{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.ClientNot where

import Control.Lens hiding ((:>), List)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.Char as C
import Data.Foldable as F
import Data.Map as M hiding (toList)
import Data.Maybe
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
import Transform.Util
import UnliftIO.MVar


type ClientNotMethod m = SMethod (m :: Method FromClient Notification)


transformClientNot :: (TransformerMonad n) => ClientNotMethod m -> NotificationMessage m -> n (NotificationMessage m)
transformClientNot meth msg = do
  logInfoN [i|Transforming client not #{meth}|]
  p' <- transformClientNot' meth (msg ^. params)

  case (meth, msg) of
    (STextDocumentDidChange, (^. params) -> p) -> do
      let originalRanges = fmap (^. range) (p ^. contentChanges)
      let newRanges = toList $ fmap (^. range) (p' ^. contentChanges)

      when (F.all isJust originalRanges && not (F.all isJust newRanges)) $ do
        logWarnN [i|Introduced a full text replace for changes: #{A.encode (p ^. contentChanges)}|]

    _ -> return ()

  return $ set params p' msg

transformClientNot' :: (TransformerMonad n) => ClientNotMethod m -> MessageParams m -> n (MessageParams m)

transformClientNot' STextDocumentDidOpen params = whenNotebook params $ \uri -> do
  let ls = T.lines (params ^. (textDocument . text))
  let (ls', transformer' :: HaskellNotebookTransformer) = project transformerParams ls
  TransformerState {..} <- ask
  modifyMVar_ transformerDocuments (\x -> return $! M.insert (getUri uri) (transformer', ls) x)
  return $ set (textDocument . text) (T.intercalate "\n" ls') params
transformClientNot' STextDocumentDidChange params = whenNotebook params $ modifyTransformer params $ \(tx, before) -> do
  let (List changeEvents) = params ^. contentChanges
  after <- applyChangesText changeEvents before
  let (changeEvents', tx') = handleDiff transformerParams changeEvents tx
  return ((tx', after), set contentChanges (List changeEvents') params)
transformClientNot' STextDocumentDidClose params = whenNotebook params $ \uri -> do
  TransformerState {..} <- ask
  modifyMVar_ transformerDocuments (\x -> return $! M.delete (getUri uri) x)
  return params

transformClientNot' _ params = return params
