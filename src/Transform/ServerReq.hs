{-# LANGUAGE PolyKinds #-}

module Transform.ServerReq where

import Language.LSP.Types


transformServerReq :: SMethod m -> RequestMessage m -> RequestMessage m
transformServerReq meth msg = msg
