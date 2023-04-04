
module Language.LSP.Parse (
  parseCodeString
  ) where

import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHCParser
import System.IO.Unsafe (unsafePerformIO)


parseCodeString :: String -> [GHCParser.Located CodeBlock]
parseCodeString = unsafePerformIO . runGhc (Just GHC.Paths.libdir) . parseString
