{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Transform.ServerNot where

import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Data.Aeson as A
import Data.String.Interpolate
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens
import Transform.Common
import Transform.Util


transformServerNot :: (TransformerMonad n, HasJSON (NotificationMessage m)) => SMethod m -> NotificationMessage m -> n (NotificationMessage m)
transformServerNot meth msg = do
  p' <- transformServerNot' meth (msg ^. params)
  let msg' = set params p' msg
  when (msg' /= msg) $ logInfoN [i|Transforming server not #{meth}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'


transformServerNot' :: (TransformerMonad n) => SMethod m -> MessageParams m -> n (MessageParams m)

transformServerNot' STextDocumentPublishDiagnostics params = whenNotebookUri params $ withTransformer params $ \(DocumentState {transformer=tx, ..}) -> do
  return $ params
         & set uri origUri
         & over diagnostics (fmap (over range (untransformRange tx)))

transformServerNot' meth msg = pure msg
