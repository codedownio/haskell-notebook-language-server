{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Transform.ServerNot where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import Data.Time
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message
import Transform.Common
import Transform.Util


transformServerNot :: (TransformerMonad n, HasJSON (TNotificationMessage m)) => SMethod m -> TNotificationMessage m -> n (TNotificationMessage m)
transformServerNot meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformServerNot' meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming server not #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'


transformServerNot' :: (TransformerMonad n) => SMethod m -> MessageParams m -> n (MessageParams m)

transformServerNot' SMethod_TextDocumentPublishDiagnostics params = whenNotebookResultUri params $ withTransformer params $ \(DocumentState {transformer=tx, ..}) -> do
  return $ params
         & set uri origUri
         & over diagnostics (mapMaybe (traverseOf range (untransformRange tx)))

transformServerNot' _meth msg = pure msg
