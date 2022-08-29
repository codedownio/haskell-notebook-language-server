
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Sandwich

import qualified Test.BinarySearchVec
import qualified Test.ExpressionToDeclaration
import qualified Test.FrontSifter
import qualified Test.StripDirective

spec :: TopSpec
spec = do
  Test.BinarySearchVec.spec
  Test.ExpressionToDeclaration.spec
  Test.FrontSifter.spec
  Test.StripDirective.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
