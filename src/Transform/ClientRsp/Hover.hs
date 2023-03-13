{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Transform.ClientRsp.Hover (
  fixupHoverText

  , mkDocRegex

  -- For testing
  , fixupDocumentReferences'
  ) where

import Control.Lens hiding (List)
import Control.Lens.Regex.Text
import Control.Monad.Reader
import Data.Function (fix)
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import Data.Text.Rope (Rope)
import Language.LSP.Notebook (HaskellNotebookTransformer)
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (id, trace)
import Safe
import Text.Regex.PCRE.Light
import Transform.Util
import UnliftIO.MVar


fixupHoverText :: (TransformerMonad n) => Hover -> n Hover
fixupHoverText initialHover = do
  documentsMap <- ask >>= (readMVar . transformerDocuments)
  flip fix (initialHover, M.toList documentsMap) $ \loop args -> case args of
    (paramsInProgress, []) -> return paramsInProgress
    (paramsInProgress, (_, DocumentState {..}):xs) -> loop (newParams, xs)
      where newParams = over contents (fixupDocumentReferences referenceRegex transformer curLines) paramsInProgress

fixupDocumentReferences :: Regex -> HaskellNotebookTransformer -> Rope -> HoverContents -> HoverContents
fixupDocumentReferences docRegex transformer _curLines (HoverContents (MarkupContent k t)) = HoverContents (MarkupContent k (fixupDocumentReferences' docRegex transformer t))
fixupDocumentReferences docRegex transformer _curLines (HoverContentsMS mss) = HoverContentsMS (fmap transformMarkedString mss)
  where
    transformMarkedString (PlainString t) = PlainString (fixupDocumentReferences' docRegex transformer t)
    transformMarkedString (CodeString (LanguageString l t)) = CodeString (LanguageString l (fixupDocumentReferences' docRegex transformer t))

fixupDocumentReferences' :: Regex -> HaskellNotebookTransformer -> Text -> Text
fixupDocumentReferences' docRegex transformer t = t & (regexing docRegex) . groups %~ transformGroup transformer
  where
    transformGroup :: HaskellNotebookTransformer -> [Text] -> [Text]
    transformGroup transformer matches@[(readMay . T.unpack) -> Just line, (readMay . T.unpack) -> Just ch] = [T.pack $ show line', T.pack $ show ch']
      where
        (Position line' ch') = untransformPosition transformerParams transformer (Position line ch)

    transformGroup _ matches = matches

mkDocRegex :: Text -> Regex
mkDocRegex docName = compile [i|#{escapedName}:(\\d+):(\\d+)|] [utf8]
  where
    escapedName = docName
      & (\x -> if filePrefix `T.isPrefixOf` x then T.drop (T.length filePrefix) x else x)
      & L.foldl' (.) id [T.replace c ("\\" <> c) | c <- pcreChars]

    filePrefix = "file://"

    pcreChars = [".", "^", "$", "*", "+", "?", "(", ")", "[", "{", "\\", "|"]
