{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Transform.ClientRsp where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.String.Interpolate
import Data.Time
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (trace)
import Transform.ClientRsp.Hover
import Transform.Util


transformClientRsp :: (TransformerMonad n, HasJSON (ResponseMessage m)) => SMethod m -> ResponseMessage m -> n (ResponseMessage m)
transformClientRsp meth msg = do
  start <- liftIO getCurrentTime
  msg' <- transformClientRsp' meth msg
  stop <- liftIO getCurrentTime
  when (msg' /= msg) $ logDebugN [i|Transforming client resp #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'


transformClientRsp' :: (TransformerMonad n) => SMethod m -> ResponseMessage m -> n (ResponseMessage m)
transformClientRsp' STextDocumentHover msg = traverseOf (result . _Right . _Just) fixupHoverText msg
transformClientRsp' _meth msg = pure msg
