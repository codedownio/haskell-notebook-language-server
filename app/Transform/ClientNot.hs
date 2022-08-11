{-# LANGUAGE PolyKinds #-}

module Transform.ClientNot where

import Language.LSP.Types


transformClientNot :: SMethod m -> NotificationMessage m -> NotificationMessage m
transformClientNot meth msg = msg
