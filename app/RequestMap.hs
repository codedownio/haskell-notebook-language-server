{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module RequestMap where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as A hiding (Options)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IxMap
import Data.Kind
import Data.Maybe
import Data.Sequence hiding (zip)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens
import Options.Applicative
import Streams
import System.IO
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process


-- * Client

type ClientRequestMap = IxMap LspId (SMethod :: Method FromClient Request -> Type)

newClientRequestMap :: ClientRequestMap
newClientRequestMap = emptyIxMap

updateClientRequestMap :: ClientRequestMap -> LspId m -> SClientMethod m -> Maybe ClientRequestMap
updateClientRequestMap reqMap id method = insertIxMap id method reqMap

lookupClientRequestMap :: ClientRequestMap -> LspId m -> Maybe (SMethod (m :: Method FromClient Request))
lookupClientRequestMap reqMap id = lookupIxMap id reqMap

-- * Server

type ServerRequestMap = IxMap LspId (SMethod :: Method FromServer Request -> Type)

newServerRequestMap :: ServerRequestMap
newServerRequestMap = emptyIxMap

updateServerRequestMap :: ServerRequestMap -> LspId m -> SServerMethod m -> Maybe ServerRequestMap
updateServerRequestMap reqMap id method = insertIxMap id method reqMap

lookupServerRequestMap :: ServerRequestMap -> LspId m -> Maybe (SMethod (m :: Method FromServer Request))
lookupServerRequestMap reqMap id = lookupIxMap id reqMap
