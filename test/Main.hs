
import Test.Sandwich

import qualified Test.BinarySearchVec

import qualified Test.Transformer.DirectiveToPragma
import qualified Test.Transformer.ExpressionToDeclaration
import qualified Test.Transformer.StatementToDeclaration
import qualified Test.Transformer.ImportSifter
import qualified Test.Transformer.PragmaSifter
import qualified Test.Transformer.StripDirective

import qualified Test.Hover


spec :: TopSpec
spec = do
  describe "Util" $ do
    Test.BinarySearchVec.spec

  describe "Transformers" $ do
    Test.Transformer.DirectiveToPragma.spec
    Test.Transformer.ExpressionToDeclaration.spec
    Test.Transformer.StatementToDeclaration.spec
    Test.Transformer.ImportSifter.spec
    Test.Transformer.PragmaSifter.spec
    Test.Transformer.StripDirective.spec

  Test.Hover.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
