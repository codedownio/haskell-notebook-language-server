{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module RequestMap where

import Data.IxMap
import Data.Kind
import Language.LSP.Types


-- * Client

data SMethodAndParams m = SMethodAndParams (SMethod m) (MessageParams m)

type ClientRequestMap = IxMap LspId (SMethodAndParams :: Method FromClient Request -> Type)

newClientRequestMap :: ClientRequestMap
newClientRequestMap = emptyIxMap

updateClientRequestMap :: ClientRequestMap -> LspId m -> SMethodAndParams (m :: Method FromClient Request) -> Maybe ClientRequestMap
updateClientRequestMap reqMap id x = insertIxMap id x reqMap

lookupClientRequestMap :: ClientRequestMap -> LspId m -> Maybe (SMethodAndParams (m :: Method FromClient Request))
lookupClientRequestMap reqMap id = lookupIxMap id reqMap

-- * Server

type ServerRequestMap = IxMap LspId (SMethodAndParams :: Method FromServer Request -> Type)

newServerRequestMap :: ServerRequestMap
newServerRequestMap = emptyIxMap

updateServerRequestMap :: ServerRequestMap -> LspId m -> (SMethodAndParams (m :: Method FromServer Request)) -> Maybe ServerRequestMap
updateServerRequestMap reqMap id x = insertIxMap id x reqMap

lookupServerRequestMap :: ServerRequestMap -> LspId m -> Maybe (SMethodAndParams (m :: Method FromServer Request))
lookupServerRequestMap reqMap id = lookupIxMap id reqMap
