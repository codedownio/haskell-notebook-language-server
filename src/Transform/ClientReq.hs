{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Transform.ClientReq where

import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.String.Interpolate
import Data.Time
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Common
import Transform.Util


type ClientReqMethod m = SMethod (m :: Method 'FromClient 'Request)


transformClientReq :: (TransformerMonad n, HasJSON (RequestMessage m)) => ClientReqMethod m -> RequestMessage m -> n (RequestMessage m)
transformClientReq meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientReq' meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming client req #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientReq' :: forall m n. (TransformerMonad n) => ClientReqMethod m -> MessageParams m -> n (MessageParams m)
transformClientReq' STextDocumentCodeAction params = whenNotebook params $ withTransformer params $ doTransformUriAndRange @m params
transformClientReq' STextDocumentCodeLens params = whenNotebook params $ withTransformer params $ doTransformUri @m params
transformClientReq' STextDocumentCompletion params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentDefinition params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentDocumentHighlight params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentDocumentSymbol params = whenNotebook params $ withTransformer params $ doTransformUri @m params
transformClientReq' STextDocumentHover params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentImplementation params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentTypeDefinition params = whenNotebook params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' _ params = return params


doTransformUriAndPosition :: forall m n a. (
  TransformerMonad n, HasPosition (MessageParams m) Position, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUriAndPosition params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  case transformPosition transformerParams tx (params ^. position) of
    Nothing -> do
      logWarnN [i|Couldn't transform position #{params ^. position}|]
      return params
    Just pos' -> return $ set position pos' params

doTransformUriAndRange :: forall m n a. (
  TransformerMonad n, HasRange (MessageParams m) Range, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUriAndRange params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  case transformRange tx (params ^. range) of
    Nothing -> do
      logWarnN [i|Couldn't transform range #{params ^. range}|]
      return params
    Just pos' -> return $ set range pos' params

doTransformUri :: forall m n a. (
  TransformerMonad n, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUri params' (DocumentState {newUri}) = return $ params' & set (textDocument . uri) newUri
