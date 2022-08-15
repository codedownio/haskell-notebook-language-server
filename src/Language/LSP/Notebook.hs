{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  module Language.LSP.Notebook.ExpressionToDeclaration
  , module Language.LSP.Notebook.FrontSifter
  , HaskellNotebookTransformer
  ) where

import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer


type HaskellNotebookTransformer = StripDirective :> ExpressionToDeclaration :> FrontSifter
