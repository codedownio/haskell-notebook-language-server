
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types hiding (line)
import Test.Sandwich

import qualified Test.BinarySearchVec

import qualified Test.Transformer.ExpressionToDeclaration
import qualified Test.Transformer.FrontSifter
import qualified Test.Transformer.StripDirective

import qualified Test.Hover


spec :: TopSpec
spec = do
  describe "Util" $ do
    Test.BinarySearchVec.spec

  describe "Transformers" $ do
    Test.Transformer.ExpressionToDeclaration.spec
    Test.Transformer.FrontSifter.spec
    Test.Transformer.StripDirective.spec

  Test.Hover.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
