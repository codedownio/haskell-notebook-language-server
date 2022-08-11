{-# LANGUAGE PolyKinds #-}

module Transform.ClientRsp where

import Language.LSP.Types


transformClientRsp :: SMethod m -> ResponseMessage m -> ResponseMessage m
transformClientRsp meth msg = msg
