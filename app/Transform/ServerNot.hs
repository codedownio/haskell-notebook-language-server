{-# LANGUAGE PolyKinds #-}

module Transform.ServerNot where

import Language.LSP.Types


transformServerNot :: SMethod m -> NotificationMessage m -> NotificationMessage m
transformServerNot meth msg = msg
