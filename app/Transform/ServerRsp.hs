{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ServerRsp where

import Control.Lens
import Control.Monad.Logger
import Data.String.Interpolate
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Util


type ServerRspMethod m = SMethod (m :: Method FromClient Request)

transformServerRsp :: (TransformerMonad n) => ServerRspMethod m -> ResponseMessage m -> n (ResponseMessage m)
transformServerRsp meth msg = do
  logInfoN [i|Transforming server response #{meth}|]
  case msg ^. result of
    Left err -> return msg
    Right ret -> do
      p' <- transformServerRsp' meth ret
      return $ set result (Right p') msg

transformServerRsp' :: (TransformerMonad n) => ServerRspMethod m -> ResponseResult m -> n (ResponseResult m)
transformServerRsp' STextDocumentDocumentHighlight result = undefined
  -- whenNotebook result $ \uri -> do
  --   undefined
transformServerRsp' _ result = return result
