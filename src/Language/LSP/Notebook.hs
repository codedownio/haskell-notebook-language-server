{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  module Language.LSP.Notebook.ExpressionToDeclaration
  , module Language.LSP.Notebook.StatementToDeclaration
  , module Language.LSP.Notebook.FrontSifter

  , HaskellNotebookTransformer
  , expressionToDeclarationParams
  , transformerParams
  ) where

import GHC (DynFlags)
import Language.LSP.Notebook.DirectiveToPragma
import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Notebook.HeaderTransformer
import Language.LSP.Notebook.StatementToDeclaration
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer


type HaskellNotebookTransformer =
  DirectiveToPragma -- Convert :set -XFoo directives to LANGUAGE pragmas -- 11.3ms using normal ihaskell
  :> StripDirective -- Strip all remaining directives -- 14.09ms
  :> ExpressionToDeclaration -- Convert naked expressions to declarations -- 13.2ms
  :> StatementToDeclaration -- Convert naked statements to declarations -- 13.16ms
  :> ImportSifter -- Sift imports to the top -- 12.24ms
  :> HeaderTransformer -- Add unsafePerformIO import at top -- 4.715us
  :> PragmaSifter -- Sift pragmas to the top (above imports) -- 12ms

expressionToDeclarationParams :: DynFlags -> Params ExpressionToDeclaration
expressionToDeclarationParams = EDParams 10

transformerParams :: DynFlags -> Params HaskellNotebookTransformer
transformerParams flags =
  DTPParams flags
  :> SDParams flags
  :> expressionToDeclarationParams flags
  :> STDParams flags
  :> flags
  :> ["import System.IO.Unsafe (unsafePerformIO)"]
  :> flags
