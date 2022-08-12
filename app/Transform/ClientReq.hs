{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ClientReq where

import Control.Lens hiding ((:>))
import qualified Data.Char as C
import Data.Text
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath


transformClientReq :: SMethod m -> RequestMessage m -> RequestMessage m
transformClientReq meth = over Lens.params (transformClientReq' meth)

transformClientReq' :: SMethod m -> MessageParams m -> MessageParams m
transformClientReq' STextDocumentDidOpen params = whenNotebook params (over (textDocument . text) transformText params)
transformClientReq' _ params = params


whenNotebook :: Lens.HasTextDocument a TextDocumentItem => a -> a -> a
whenNotebook params notebookParams = case parseURIReference (T.unpack (getUri (params ^. (textDocument . uri)))) of
  Nothing -> params
  Just (URI {..})
    | fmap C.toLower (takeExtension uriPath) == ".ipynb" -> notebookParams
    | fmap C.toLower (takeExtension uriPath) == ".ipynb.hs" -> notebookParams
    | otherwise -> params


transformText :: Text -> Text
transformText (T.lines -> ls) = T.intercalate "\n" ls'
  where (ls', transformer' :: HaskellNotebookTransformer) = project ((EDParams 10) :> ()) ls
