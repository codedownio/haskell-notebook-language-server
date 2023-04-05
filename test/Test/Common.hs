
module Test.Common where

import Control.Monad.Catch
import Language.LSP.Transformer
import Language.LSP.Types
import Test.Sandwich


transformAndUntransform :: (MonadThrow m, Transformer a) => Params a -> Position -> Position -> a -> m ()
transformAndUntransform params from to x = do
  transformPosition params x from `shouldBe` (Just to)
  untransformPosition params x to `shouldBe` (Just from)
