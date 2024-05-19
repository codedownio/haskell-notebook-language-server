{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Transform.ClientReq where

import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.String.Interpolate
import Data.Time
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Transform.Common
import Transform.Util
import UnliftIO.Concurrent


type ClientReqMethod m = SMethod (m :: Method 'ClientToServer 'Request)


transformClientReq :: (TransformerMonad n, HasJSON (TRequestMessage m)) => ClientReqMethod m -> TRequestMessage m -> n (TRequestMessage m)
transformClientReq meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientReq' meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming client req #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientReq' :: forall m n. (TransformerMonad n) => ClientReqMethod m -> MessageParams m -> n (MessageParams m)

transformClientReq' SMethod_Initialize params = do
  -- Store the non-modified params, so we can access the unmodified rootUri
  asks transformerInitializeParams >>= flip modifyMVar_ (\_ -> return $ Just params)
  pure params

transformClientReq' SMethod_TextDocumentCodeAction params = whenNotebook params $ withTransformer params $ doTransformUriAndRange @m params
transformClientReq' SMethod_TextDocumentCodeLens params = whenNotebook params $ withTransformer params $ doTransformUri @m params
transformClientReq' SMethod_TextDocumentCompletion params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' SMethod_TextDocumentDefinition params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' SMethod_TextDocumentDocumentHighlight params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' SMethod_TextDocumentDocumentSymbol params = whenNotebook params $ withTransformer params $ doTransformUri @m params
transformClientReq' SMethod_TextDocumentHover params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' SMethod_TextDocumentImplementation params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' SMethod_TextDocumentTypeDefinition params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' _ params = return params


doTransformUriAndPosition :: forall m n a. (
  TransformerMonad n, HasPosition (MessageParams m) Position, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUriAndPosition params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  AppConfig {..} <- asks transformerConfig
  case transformPosition (transformerParams appConfigDynFlags) tx (params ^. position) of
    Nothing -> do
      logWarnN [i|Couldn't transform position #{params ^. position}|]
      return params
    Just pos' -> return $ set position pos' params

doTransformUriAndRange :: forall m n a. (
  TransformerMonad n, HasRange (MessageParams m) Range, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUriAndRange params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  AppConfig {..} <- asks transformerConfig
  case transformRange appConfigDynFlags tx (params ^. range) of
    Nothing -> do
      logWarnN [i|Couldn't transform range #{params ^. range}|]
      return params
    Just pos' -> return $ set range pos' params

doTransformUri :: forall m n a. (
  TransformerMonad n, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUri params' (DocumentState {newUri}) = return $ params' & set (textDocument . uri) newUri
