{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Transform.ClientReq where

import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Logger
import Data.Aeson as A
import Data.String.Interpolate
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Util


type ClientReqMethod m = SMethod (m :: Method FromClient Request)


transformClientReq :: (TransformerMonad n, HasJSON (RequestMessage m)) => ClientReqMethod m -> RequestMessage m -> n (RequestMessage m)
transformClientReq meth msg = do
  p' <- transformClientReq' meth (msg ^. params)
  let msg' = set params p' msg
  when (msg' /= msg) $ logInfoN [i|Transforming client req #{meth}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientReq' :: forall m n. (TransformerMonad n) => ClientReqMethod m -> MessageParams m -> n (MessageParams m)
transformClientReq' STextDocumentDocumentHighlight params = whenNotebook params $ withTransformer params $ doTransformPositionAndUri @m params
transformClientReq' STextDocumentHover params = whenNotebook params $ withTransformer params $ doTransformPositionAndUri @m params

transformClientReq' STextDocumentDefinition params = whenNotebook params $ withTransformer params $ doTransformPositionAndUri @m params
transformClientReq' STextDocumentTypeDefinition params = whenNotebook params $ withTransformer params $ doTransformPositionAndUri @m params
transformClientReq' STextDocumentImplementation params = whenNotebook params $ withTransformer params $ doTransformPositionAndUri @m params

transformClientReq' STextDocumentDocumentSymbol params = whenNotebook params $ withTransformer params $ doTransformUri @m params
transformClientReq' STextDocumentCodeAction params = whenNotebook params $ withTransformer params $ doTransformUri @m params

transformClientReq' _ params = return params



doTransformPositionAndUri :: forall m n a. (
  TransformerMonad n, HasPosition (MessageParams m) Position, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformPositionAndUri params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  case transformPosition transformerParams tx (params ^. position) of
    Nothing -> do
      logWarnN [i|Couldn't transform position #{params ^. position}|]
      return params
    Just pos' -> return $ set position pos' params

doTransformUri :: forall m n a. (
  TransformerMonad n, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUri params' (DocumentState {newUri}) = return $ params' & set (textDocument . uri) newUri
