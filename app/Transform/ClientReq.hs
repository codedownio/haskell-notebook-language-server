{-# LANGUAGE PolyKinds #-}

module Transform.ClientReq where

import Language.LSP.Types


transformClientReq :: SMethod m -> RequestMessage m -> RequestMessage m
transformClientReq meth msg = msg
