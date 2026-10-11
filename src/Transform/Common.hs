{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import qualified Data.Map as M
import Data.Maybe
import GHC (DynFlags)
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Transform.Util


-- * Transform

transformRange :: DynFlags -> HaskellNotebookTransformer -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (HasRange a Range) => DynFlags -> HaskellNotebookTransformer -> a -> Maybe a
transformRanged flags tx x = x
  & traverseOf (range . start) (transformPosition (transformerParams flags) tx)
  >>= traverseOf (range . end) (transformPosition (transformerParams flags) tx)

-- * Untransform

untransformRange :: DynFlags -> HaskellNotebookTransformer -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (HasRange a Range) => DynFlags -> HaskellNotebookTransformer -> a -> Maybe a
untransformRanged flags tx x = x
  & traverseOf (range . start) (untransformPosition (transformerParams flags) tx)
  >>= traverseOf (range . end) (untransformPosition (transformerParams flags) tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => DynFlags -> HaskellNotebookTransformer -> a -> Maybe a
untransformRangedMaybe flags tx x = x
  & traverseOf (range . _Just . start) (untransformPosition (transformerParams flags) tx)
  >>= traverseOf (range . _Just . end) (untransformPosition (transformerParams flags) tx)

-- | An inlay hint sits at a position rather than over a range, and carries its own edits for
-- when the user accepts it. Drop the hint if its position isn't in the notebook; drop
-- individual edits that aren't.
untransformInlayHint :: DynFlags -> HaskellNotebookTransformer -> InlayHint -> Maybe InlayHint
untransformInlayHint flags tx hint =
  hint
    & traverseOf position (untransformPosition (transformerParams flags) tx)
    & fmap (over (textEdits . _Just) (mapMaybe (untransformRanged flags tx)))

untransformWorkspaceEdit :: DynFlags -> DocumentState -> WorkspaceEdit -> Maybe WorkspaceEdit
untransformWorkspaceEdit flags (DocumentState {transformer=tx, origUri=notebookUri, newUri=shadowUri}) wsEdit = do
  changes' <- traverse untransformChanges (wsEdit ^. changes)
  documentChanges' <- traverse (traverse untransformDocumentChange) (wsEdit ^. documentChanges)
  pure $ wsEdit
    & set changes changes'
    & set documentChanges documentChanges'
  where
    untransformChanges m = M.fromList <$> traverse untransformEntry (M.toList m)

    untransformEntry (u, es)
      | u == shadowUri = (,) notebookUri <$> traverse (untransformRanged flags tx) es
      | otherwise = Nothing

    untransformDocumentChange (InL textDocumentEdit) = InL <$> untransformTextDocumentEdit textDocumentEdit
    untransformDocumentChange (InR _createRenameOrDelete) = Nothing

    untransformTextDocumentEdit tde
      | tde ^. textDocument . uri /= shadowUri = Nothing
      | otherwise = tde
          & set (textDocument . uri) notebookUri
          & set (textDocument . version) (InR Null)
          & traverseOf (edits . traverse) untransformEdit

    untransformEdit (InL textEdit) = InL <$> untransformRanged flags tx textEdit
    untransformEdit (InR annotatedTextEdit) = InR <$> untransformRanged flags tx annotatedTextEdit

untransformCodeAction :: DynFlags -> DocumentState -> CodeAction -> Maybe CodeAction
untransformCodeAction flags ds codeAction = case codeAction ^. edit of
  Nothing -> Just codeAction
  Just wsEdit -> (\wsEdit' -> codeAction & set edit (Just wsEdit')) <$> untransformWorkspaceEdit flags ds wsEdit

workspaceEditUris :: WorkspaceEdit -> [Uri]
workspaceEditUris wsEdit =
  M.keys (fromMaybe mempty (wsEdit ^. changes))
  <> [tde ^. textDocument . uri | InL tde <- fromMaybe [] (wsEdit ^. documentChanges)]

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
