
module Language.LSP.Parse (
  parseCodeString
  ) where

import GHC
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHCParser
import System.IO.Unsafe (unsafePerformIO)


parseCodeString :: FilePath -> String -> [GHCParser.Located CodeBlock]
parseCodeString ghcLibDir = unsafePerformIO . runGhc (Just ghcLibDir) . parseString
