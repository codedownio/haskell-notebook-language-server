{-# LANGUAGE PolyKinds #-}

module Transform.ServerReq where

import Language.LSP.Protocol.Message


transformServerReq :: SMethod m -> TRequestMessage m -> TRequestMessage m
transformServerReq _meth msg = msg
