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
  DirectiveToPragma -- Convert :set -XFoo directives to LANGUAGE pragmas
  :> StripDirective -- Strip all remaining directives
  :> ExpressionToDeclaration -- Convert naked expressions to declarations
  :> StatementToDeclaration -- Convert naked statements to declarations
  :> ImportSifter -- Sift imports to the top
  :> HeaderTransformer -- Add unsafePerformIO import at top
  :> PragmaSifter -- Sift pragmas to the top (above imports)

expressionToDeclarationParams :: Params ExpressionToDeclaration
expressionToDeclarationParams = EDParams 10

transformerParams :: Params HaskellNotebookTransformer
transformerParams =
  DTPParams
  :> SDParams
  :> expressionToDeclarationParams
  :> STDParams
  :> ()
  :> ["import System.IO.Unsafe (unsafePerformIO)"]
  :> ()
