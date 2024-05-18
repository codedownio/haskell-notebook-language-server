{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  module Language.LSP.Notebook.ExpressionToDeclaration
  , module Language.LSP.Notebook.StatementToDeclaration
  , module Language.LSP.Notebook.FrontSifter

  , HaskellNotebookTransformer
  , expressionToDeclarationParams
  , transformerParams
  ) where

import Language.LSP.Notebook.DirectiveToPragma
import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Notebook.HeaderTransformer
import Language.LSP.Notebook.StatementToDeclaration
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer


type HaskellNotebookTransformer =
  -- DirectiveToPragma -- Convert :set -XFoo directives to LANGUAGE pragmas
  -- :> StripDirective -- Strip all remaining directives
  -- :> ExpressionToDeclaration -- Convert naked expressions to declarations
  -- :> StatementToDeclaration -- Convert naked statements to declarations
  -- :> ImportSifter -- Sift imports to the top
  HeaderTransformer -- Add unsafePerformIO import at top
  -- :> PragmaSifter -- Sift pragmas to the top (above imports)

expressionToDeclarationParams :: FilePath -> Params ExpressionToDeclaration
expressionToDeclarationParams = EDParams 10

transformerParams :: FilePath -> Params HaskellNotebookTransformer
transformerParams ghcLibDir =
  -- DTPParams ghcLibDir
  -- :> SDParams ghcLibDir
  -- :> expressionToDeclarationParams ghcLibDir
  -- :> STDParams ghcLibDir
  -- :> ghcLibDir
  ["import System.IO.Unsafe (unsafePerformIO)"]
  -- :> ghcLibDir
