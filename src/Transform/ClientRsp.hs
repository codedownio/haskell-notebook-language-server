{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Transform.ClientRsp where

import Control.Lens hiding (List)
import Control.Lens.Regex.Text
import Control.Monad.Reader
import Data.Function
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text
import Language.LSP.Notebook (HaskellNotebookTransformer)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (trace)
import Text.Regex.PCRE.Light
import UnliftIO.MVar

import Transform.Util
import Transform.ClientRsp.Hover


transformClientRsp :: (TransformerMonad n) => SMethod m -> ResponseMessage m -> n (ResponseMessage m)

transformClientRsp STextDocumentHover msg = traverseOf (result . _Right . _Just) fixupHoverText msg

transformClientRsp meth msg = pure msg
