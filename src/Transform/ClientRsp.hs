{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Transform.ClientRsp where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.String.Interpolate
import Data.Time
import Language.LSP.Types
import Transform.Util


type ClientRspMethod m = SMethod (m :: Method 'FromServer 'Request)

transformClientRsp :: (TransformerMonad n, HasJSON (ResponseMessage m)) => ClientRspMethod m -> ResponseMessage m -> n (ResponseMessage m)
transformClientRsp meth msg = do
  start <- liftIO getCurrentTime
  msg' <- transformClientRsp' meth msg
  stop <- liftIO getCurrentTime
  when (msg' /= msg) $ logDebugN [i|Transforming client resp #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'


transformClientRsp' :: (TransformerMonad n) => ClientRspMethod m -> ResponseMessage m -> n (ResponseMessage m)
transformClientRsp' _meth msg = pure msg
