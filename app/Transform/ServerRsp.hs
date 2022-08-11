{-# LANGUAGE PolyKinds #-}

module Transform.ServerRsp where

import Language.LSP.Types


transformServerRsp :: SMethod m -> ResponseMessage m -> ResponseMessage m
transformServerRsp meth msg = msg
