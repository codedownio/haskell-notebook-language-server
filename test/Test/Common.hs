
module Test.Common where

import Language.LSP.Transformer
import Test.Sandwich


transformAndUntransform params from to x = do
  transformPosition params x from `shouldBe` (Just to)
  untransformPosition params x to `shouldBe` from
