{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.ClientReq where

import Control.Lens hiding ((:>))
import Control.Monad.Logger
import qualified Data.Char as C
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath


type ClientReqMethod m = SMethod (m :: Method FromClient Request)


transformClientReq :: (MonadLoggerIO n) => ClientReqMethod m -> RequestMessage m -> n (RequestMessage m)
transformClientReq meth msg = do
  logInfoN [i|Transforming #{meth}|]
  p' <- transformClientReq' meth (msg ^. params)
  return $ set params p' msg

transformClientReq' :: (MonadLoggerIO n) => ClientReqMethod m -> MessageParams m -> n (MessageParams m)
transformClientReq' _ params = return params
