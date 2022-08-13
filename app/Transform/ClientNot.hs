{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.ClientNot where

import Control.Lens hiding ((:>))
import Control.Monad.Logger
import qualified Data.Char as C
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath
import Transform.Util


type ClientNotMethod m = SMethod (m :: Method FromClient Notification)


transformClientNot :: (MonadLoggerIO n) => ClientNotMethod m -> NotificationMessage m -> n (NotificationMessage m)
transformClientNot meth msg = do
  logInfoN [i|Transforming #{meth}|]
  p' <- transformClientNot' meth (msg ^. params)
  return $ set params p' msg

transformClientNot' :: (MonadLoggerIO n) => ClientNotMethod m -> MessageParams m -> n (MessageParams m)
transformClientNot' STextDocumentDidOpen params = whenNotebook params $ do
  return $ over (textDocument . text) transformText params
transformClientNot' _ params = return params

transformText :: Text -> Text
transformText (T.lines -> ls) = T.intercalate "\n" ls'
  where (ls', transformer' :: HaskellNotebookTransformer) = project ((EDParams 10) :> ()) ls
