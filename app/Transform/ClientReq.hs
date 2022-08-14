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
import Transform.Util


type ClientReqMethod m = SMethod (m :: Method FromClient Request)


transformClientReq :: (TransformerMonad n) => ClientReqMethod m -> RequestMessage m -> n (RequestMessage m)
transformClientReq meth msg = do
  logInfoN [i|Transforming client req #{meth}|]
  p' <- transformClientReq' meth (msg ^. params)
  return $ set params p' msg

transformClientReq' :: (TransformerMonad n) => ClientReqMethod m -> MessageParams m -> n (MessageParams m)
transformClientReq' STextDocumentDocumentHighlight params = whenNotebook params $ \uri -> do
  lookupTransformer uri >>= \case
    Nothing -> do
      logWarnN [i|Couldn't find expected transformer for uri #{uri}|]
      return params
    Just tx -> do
      case transformPosition transformerParams tx (params ^. position) of
        Nothing -> do
          logWarnN [i|Couldn't transform position #{params ^. position} for uri #{uri}|]
          return params
        Just pos' -> return $ set position pos' params
transformClientReq' _ params = return params
