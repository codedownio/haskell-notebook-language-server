{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook where

import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Transformer


type HaskellNotebookTransformer = ExpressionToDeclaration :> FrontSifter
