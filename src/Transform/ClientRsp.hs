{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Transform.ClientRsp where

import Control.Lens hiding (List)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (trace)
import Transform.ClientRsp.Hover
import Transform.Util


transformClientRsp :: (TransformerMonad n) => SMethod m -> ResponseMessage m -> n (ResponseMessage m)

transformClientRsp STextDocumentHover msg = traverseOf (result . _Right . _Just) fixupHoverText msg

transformClientRsp _meth msg = pure msg
