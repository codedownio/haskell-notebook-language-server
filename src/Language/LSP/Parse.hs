
module Language.LSP.Parse (
  parseCodeString
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import GHC
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHCParser


parseCodeString :: MonadIO m => DynFlags -> String -> m [GHCParser.Located CodeBlock]
parseCodeString flags s = liftIO $ evalStateT (parseString s) flags
