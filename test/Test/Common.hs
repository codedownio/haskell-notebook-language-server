
module Test.Common where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich


transformAndUntransform :: (MonadIO m, MonadThrow m, Transformer a) => Position -> Position -> a -> m ()
transformAndUntransform from to x = do
  transformPosition x from `shouldBe` (Just to)
  untransformPosition x to `shouldBe` (Just from)
