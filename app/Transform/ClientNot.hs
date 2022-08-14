{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.ClientNot where

import Control.Lens hiding ((:>))
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
import Transform.Util
import UnliftIO.MVar


type ClientNotMethod m = SMethod (m :: Method FromClient Notification)


transformClientNot :: (TransformerMonad n) => ClientNotMethod m -> NotificationMessage m -> n (NotificationMessage m)
transformClientNot meth msg = do
  logInfoN [i|Transforming #{meth}|]
  p' <- transformClientNot' meth (msg ^. params)
  return $ set params p' msg

transformClientNot' :: (TransformerMonad n) => ClientNotMethod m -> MessageParams m -> n (MessageParams m)
transformClientNot' STextDocumentDidOpen params = whenNotebook params $ \uri -> do
  let (ls', transformer' :: HaskellNotebookTransformer) = project transformerParams (T.lines (params ^. (textDocument . text)))
  TransformerState {..} <- ask
  modifyMVar_ transformerDocuments (\x -> return $! M.insert (getUri uri) transformer' x)
  return $ set (textDocument . text) (T.intercalate "\n" ls') params
transformClientNot' STextDocumentDidClose params = whenNotebook params $ \uri -> do
  TransformerState {..} <- ask
  modifyMVar_ transformerDocuments (\x -> return $! M.delete (getUri uri) x)
  return params
transformClientNot' _ params = return params
