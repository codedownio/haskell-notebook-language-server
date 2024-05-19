
module Language.LSP.Parse (
  parseCodeString
  ) where

import Control.Monad.IO.Class
import GHC
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHCParser


parseCodeString :: MonadIO m => FilePath -> String -> m [GHCParser.Located CodeBlock]
parseCodeString ghcLibDir = liftIO . runGhc (Just ghcLibDir) . parseString
