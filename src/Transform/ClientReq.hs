{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

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

transformClientReq' :: forall m n. (TransformerMonad n) => ClientReqMethod m -> MessageParams m -> n (MessageParams m)
transformClientReq' STextDocumentDocumentHighlight params = whenNotebook params $ withTransformer params $ doTransformPosition @m params
transformClientReq' STextDocumentHover params = whenNotebook params $ withTransformer params $ doTransformPosition @m params

transformClientReq' STextDocumentDefinition params = whenNotebook params $ withTransformer params $ doTransformPosition @m params
transformClientReq' STextDocumentTypeDefinition params = whenNotebook params $ withTransformer params $ doTransformPosition @m params
transformClientReq' STextDocumentImplementation params = whenNotebook params $ withTransformer params $ doTransformPosition @m params

-- transformClientReq' STextDocumentCodeAction params = whenNotebook params $ withTransformer params $ doTransformPosition @m params

transformClientReq' _ params = return params



doTransformPosition :: forall m n. (TransformerMonad n, (HasPosition (MessageParams m) Position)) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformPosition params (DocumentState {transformer=tx}) = case transformPosition transformerParams tx (params ^. position) of
  Nothing -> do
    logWarnN [i|Couldn't transform position #{params ^. position}|]
    return params
  Just pos' -> return $ set position pos' params
