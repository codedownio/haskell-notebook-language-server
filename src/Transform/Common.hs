
module Transform.Common where

import Control.Lens hiding (List)
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens


-- * Transform

transformRange :: HaskellNotebookTransformer -> Range -> Maybe Range
transformRange tx x = case (transformPosition transformerParams tx (x ^. start), transformPosition transformerParams tx (x ^. end)) of
  (Just start', Just end') -> Just (Range start' end')
  _ -> Nothing

-- * Untransform

untransformRange :: HaskellNotebookTransformer -> Range -> Range
untransformRange tx x = x
  & over start (untransformPosition transformerParams tx)
  & over end (untransformPosition transformerParams tx)

untransformRanged :: (HasRange a Range) => HaskellNotebookTransformer -> a -> a
untransformRanged tx x = x
  & over (range . start) (untransformPosition transformerParams tx)
  & over (range . end) (untransformPosition transformerParams tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => HaskellNotebookTransformer -> a -> a
untransformRangedMaybe tx x = x
  & over (range . _Just . start) (untransformPosition transformerParams tx)
  & over (range . _Just . end) (untransformPosition transformerParams tx)
