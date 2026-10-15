{-# LANGUAGE RankNTypes #-}

module Transform.Config (haskellNotebookConfig) where

import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import GHC (DynFlags)
import Language.LSP.Notebook
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Types
import Language.LSP.Transformer (Doc)
import Text.Regex.PCRE.Light (Regex)


haskellNotebookConfig :: DynFlags -> NotebookConfig HaskellNotebookTransformer
haskellNotebookConfig flags = NotebookConfig {
  notebookConfigHandles = isNotebook
  , notebookConfigDidSaveDebounceMs = Nothing
  , notebookConfigHandlesFormatting = False
  , notebookConfigTransformerParamsFor = const (transformerParams flags)
  , notebookConfigMakeShadowDocument = makeShadowDocument
  , notebookConfigUnopenedUri = addExtensionToUri ".hs"
  , notebookConfigOnDocumentsChanged = pure ()
  , notebookConfigTransformInitialize = pure
  , notebookConfigCustomUriMethods = []
  , notebookConfigIgnoreSymbol = ignoreSymbol
  , notebookConfigLanguageName = "haskell"
  }
  where
    -- The imports symbol doesn't make much sense in notebooks, where imports can appear
    -- anywhere. It also gives away our hidden unsafePerformIO import at the top of the file.
    ignoreSymbol n = isExpressionVariable (expressionToDeclarationParams flags) n || n == "imports"

makeShadowDocument :: TransformerMonad HaskellNotebookTransformer n => Uri -> Doc -> n (Uri, FilePath, Regex)
makeShadowDocument origUri _projected = do
  newUri <- addExtensionToUri ".hs" origUri
  case uriToFilePath newUri of
    Just path -> pure (newUri, path, mkDocRegex (T.pack path))
    Nothing -> do
      logWarnN [i|Couldn't convert URI to file path: '#{newUri}'|]
      pure (newUri, "ERROR_URI_PARSE_FAILURE", mkDocRegex (getUri newUri))
