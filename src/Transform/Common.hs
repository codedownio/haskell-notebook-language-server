{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import Data.Maybe
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


-- * Transform

transformRange :: HaskellNotebookTransformer -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (HasRange a Range) => HaskellNotebookTransformer -> a -> Maybe a
transformRanged tx x = x
  & traverseOf (range . start) (transformPosition tx)
  >>= traverseOf (range . end) (transformPosition tx)

-- * Untransform

untransformRange :: HaskellNotebookTransformer -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (HasRange a Range) => HaskellNotebookTransformer -> a -> Maybe a
untransformRanged tx x = x
  & traverseOf (range . start) (untransformPosition tx)
  >>= traverseOf (range . end) (untransformPosition tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => HaskellNotebookTransformer -> a -> Maybe a
untransformRangedMaybe tx x = x
  & traverseOf (range . _Just . start) (untransformPosition tx)
  >>= traverseOf (range . _Just . end) (untransformPosition tx)

-- | An inlay hint sits at a position rather than over a range, and carries its own edits for
-- when the user accepts it. Drop the hint if its position isn't in the notebook; drop
-- individual edits that aren't.
untransformInlayHint :: HaskellNotebookTransformer -> InlayHint -> Maybe InlayHint
untransformInlayHint tx hint =
  hint
    & traverseOf position (untransformPosition tx)
    & fmap (over (textEdits . _Just) (mapMaybe (untransformRanged tx)))

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
