
module Language.LSP.Notebook.Util where

import IHaskell.Eval.Parser


isImportLine :: CodeBlock -> Bool
isImportLine (Import {}) = True
isImportLine _ = False

isPragmaLine :: CodeBlock -> Bool
isPragmaLine (Pragma {}) = True
isPragmaLine _ = False
