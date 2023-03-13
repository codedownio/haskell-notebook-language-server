{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  module Language.LSP.Notebook.ExpressionToDeclaration
  , module Language.LSP.Notebook.FrontSifter
  , HaskellNotebookTransformer
  ) where

import Language.LSP.Notebook.DirectiveToPragma
import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer


type HaskellNotebookTransformer =
  DirectiveToPragma -- Convert :set -XFoo directives to LANGUAGE pragmas
  :> StripDirective -- Strip all remaining directives
  :> ExpressionToDeclaration -- Convert naked expressions do declarations
  :> ImportSifter -- Sift imports to the top
  :> PragmaSifter -- Sift pragmas to the top (above imports)
