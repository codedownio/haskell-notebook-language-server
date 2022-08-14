{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ServerRsp where

import Control.Lens
import Control.Monad.Logger
import Data.String.Interpolate
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Util


type ServerRspMethod m = SMethod (m :: Method FromClient Request)

transformServerRsp :: (TransformerMonad n) => ServerRspMethod m -> MessageParams m -> ResponseMessage m -> n (ResponseMessage m)
transformServerRsp meth initialParams msg = do
  logInfoN [i|Transforming server response #{meth}|]
  case msg ^. result of
    Left err -> return msg
    Right ret -> do
      p' <- transformServerRsp' meth initialParams ret
      return $ set result (Right p') msg

transformServerRsp' :: (TransformerMonad n) => ServerRspMethod m -> MessageParams m -> ResponseResult m -> n (ResponseResult m)
transformServerRsp' STextDocumentDocumentHighlight initialParams result = whenNotebookResult initialParams result $ \uri -> do
  lookupTransformer uri >>= \case
    Nothing -> do
      logWarnN [i|Couldn't find expected transformer for uri #{uri}|]
      return result
    Just tx -> do
      let transform :: DocumentHighlight -> DocumentHighlight
          transform x = x
                      & over (range . start) (untransformPosition transformerParams tx)
                      & over (range . end) (untransformPosition transformerParams tx)

      return $ fmap transform result

transformServerRsp' _ _ result = return result
