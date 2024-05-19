{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import GHC (DynFlags)
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


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

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
