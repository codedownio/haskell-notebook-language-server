{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


-- * Transform

transformRange :: FilePath -> HaskellNotebookTransformer -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (HasRange a Range) => FilePath -> HaskellNotebookTransformer -> a -> Maybe a
transformRanged ghcLibPath tx x = x
  & traverseOf (range . start) (transformPosition (transformerParams ghcLibPath) tx)
  >>= traverseOf (range . end) (transformPosition (transformerParams ghcLibPath) tx)

-- * Untransform

untransformRange :: FilePath -> HaskellNotebookTransformer -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (HasRange a Range) => FilePath -> HaskellNotebookTransformer -> a -> Maybe a
untransformRanged ghcLibPath tx x = x
  & traverseOf (range . start) (untransformPosition (transformerParams ghcLibPath) tx)
  >>= traverseOf (range . end) (untransformPosition (transformerParams ghcLibPath) tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => FilePath -> HaskellNotebookTransformer -> a -> Maybe a
untransformRangedMaybe ghcLibPath tx x = x
  & traverseOf (range . _Just . start) (untransformPosition (transformerParams ghcLibPath) tx)
  >>= traverseOf (range . _Just . end) (untransformPosition (transformerParams ghcLibPath) tx)

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
