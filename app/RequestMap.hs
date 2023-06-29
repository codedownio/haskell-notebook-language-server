{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module RequestMap where

import Data.IxMap
import Data.Kind
import Language.LSP.Protocol.Message


-- * Client

data SMethodAndParams m = SMethodAndParams (SMethod m) (MessageParams m)

type ClientRequestMap = IxMap LspId (SMethodAndParams :: Method 'ClientToServer 'Request -> Type)

newClientRequestMap :: ClientRequestMap
newClientRequestMap = emptyIxMap

updateClientRequestMap :: ClientRequestMap -> LspId m -> SMethodAndParams (m :: Method 'ClientToServer 'Request) -> Maybe ClientRequestMap
updateClientRequestMap reqMap id x = insertIxMap id x reqMap

lookupClientRequestMap :: ClientRequestMap -> LspId m -> Maybe (SMethodAndParams (m :: Method 'ClientToServer 'Request))
lookupClientRequestMap reqMap id = lookupIxMap id reqMap

-- * Server

type ServerRequestMap = IxMap LspId (SMethodAndParams :: Method 'ServerToClient 'Request -> Type)

newServerRequestMap :: ServerRequestMap
newServerRequestMap = emptyIxMap

updateServerRequestMap :: ServerRequestMap -> LspId m -> (SMethodAndParams (m :: Method 'ServerToClient 'Request)) -> Maybe ServerRequestMap
updateServerRequestMap reqMap id x = insertIxMap id x reqMap

lookupServerRequestMap :: ServerRequestMap -> LspId m -> Maybe (SMethodAndParams (m :: Method 'ServerToClient 'Request))
lookupServerRequestMap reqMap id = lookupIxMap id reqMap
