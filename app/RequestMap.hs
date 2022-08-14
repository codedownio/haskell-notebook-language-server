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

data SMethodAndParams m = SMethodAndParams (SMethod m) (MessageParams m)

type ClientRequestMap = IxMap LspId (SMethodAndParams :: Method FromClient Request -> Type)

newClientRequestMap :: ClientRequestMap
newClientRequestMap = emptyIxMap

updateClientRequestMap :: ClientRequestMap -> LspId m -> SMethodAndParams (m :: Method FromClient Request) -> Maybe ClientRequestMap
updateClientRequestMap reqMap id x = insertIxMap id x reqMap

lookupClientRequestMap :: ClientRequestMap -> LspId m -> Maybe (SMethodAndParams (m :: Method FromClient Request))
lookupClientRequestMap reqMap id = lookupIxMap id reqMap

-- * Server

type ServerRequestMap = IxMap LspId (SMethodAndParams :: Method FromServer Request -> Type)

newServerRequestMap :: ServerRequestMap
newServerRequestMap = emptyIxMap

updateServerRequestMap :: ServerRequestMap -> LspId m -> (SMethodAndParams (m :: Method FromServer Request)) -> Maybe ServerRequestMap
updateServerRequestMap reqMap id x = insertIxMap id x reqMap

lookupServerRequestMap :: ServerRequestMap -> LspId m -> Maybe (SMethodAndParams (m :: Method FromServer Request))
lookupServerRequestMap reqMap id = lookupIxMap id reqMap
