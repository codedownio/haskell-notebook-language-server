{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Transform.ClientRsp.Hover (
  fixupHoverText

  -- For testing
  , fixupDocumentReferences'
  ) where

import Control.Lens hiding (List)
import Control.Lens.Regex.Text
import Control.Monad.Reader
import Data.Function
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text
import Data.Text as T
import Language.LSP.Notebook (HaskellNotebookTransformer)
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (trace)
import Safe
import Text.Regex.PCRE.Light
import Transform.Util
import UnliftIO.MVar


fixupHoverText :: (TransformerMonad n) => Hover -> n Hover
fixupHoverText initialHover = do
  documentsMap <- ask >>= (readMVar . transformerDocuments)
  flip fix (initialHover, M.toList documentsMap) $ \loop args -> case args of
    (paramsInProgress, []) -> return paramsInProgress
    (paramsInProgress, (docName, (transformer, curLines)):xs) -> loop (newParams, xs)
      where newParams = over contents (fixupDocumentReferences docName transformer curLines) paramsInProgress

fixupDocumentReferences :: Text -> HaskellNotebookTransformer -> [Text] -> HoverContents -> HoverContents
fixupDocumentReferences docName transformer curLines (HoverContents (MarkupContent k t)) = HoverContents (MarkupContent k (fixupDocumentReferences' docName transformer t))
fixupDocumentReferences docName transformer curLines (HoverContentsMS mss) = HoverContentsMS (fmap transformMarkedString mss)
  where
    transformMarkedString (PlainString t) = PlainString (fixupDocumentReferences' docName transformer t)
    transformMarkedString (CodeString (LanguageString l t)) = CodeString (LanguageString l (fixupDocumentReferences' docName transformer t))

fixupDocumentReferences' :: Text -> HaskellNotebookTransformer -> Text -> Text
fixupDocumentReferences' docName transformer t = t & (regexing regex) . groups %~ transformGroup docName transformer
  where
    regex :: Regex
    regex = compile [i|#{docName}:(\\d+):(\\d+)|] [utf8]

    transformGroup :: Text -> HaskellNotebookTransformer -> [Text] -> [Text]
    transformGroup _docName transformer matches@[(readMay . T.unpack) -> Just line, (readMay . T.unpack) -> Just ch] = [T.pack $ show line', T.pack $ show ch']
      where
        (Position line' ch') = untransformPosition transformerParams transformer (Position line ch)

    transformGroup _ _ matches = matches
