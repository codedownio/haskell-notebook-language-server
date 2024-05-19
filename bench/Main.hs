{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.Diff.Myers
import qualified Data.Diff.Types as DT
import Data.Row.Records
import Data.String.Interpolate
import qualified Data.Text.Rope as Rope
import GHC.Generics
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHCParser
import Language.LSP.Notebook
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Parse
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


-- (doc, tx :: HaskellNotebookTransformer) = project (transformerParams GHC.Paths.libdir) (Rope.fromText "foo = p")
addU = TextDocumentContentChangeEvent $ InL (#range .== (Range (p 0 7) (p 0 7)) .+ #rangeLength .== Nothing .+ #text .== "u")

p :: Int -> Int -> Position
p l c = Position (fromIntegral l) (fromIntegral c)


repackChangeEvent (DT.ChangeEvent range text) = TextDocumentContentChangeEvent $ InL $ #range .== repackRange range .+ #rangeLength .== Nothing .+ #text .== text
repackRange (DT.Range (DT.Position l1 c1) (DT.Position l2 c2)) = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))

deriving instance Generic PragmaType
deriving instance NFData PragmaType

deriving instance Generic StringLoc
deriving instance NFData StringLoc

deriving instance Generic DirectiveType
deriving instance NFData DirectiveType

deriving instance Generic CodeBlock
deriving instance NFData CodeBlock

deriving instance Generic (Located CodeBlock)
deriving instance NFData (Located CodeBlock)

testGroup :: Benchmark
testGroup =
  bgroup [i|Parsing|] [
    bench "parse foo = putSt" $ nfIO $ parseCodeString GHC.Paths.libdir "foo = putSt"
    , bench "parse foo = putStrLn" $ nfIO $ parseCodeString GHC.Paths.libdir "foo = putStrLn \"hi\""

    -- bench "diffTextsToChangeEventsConsolidate" $ nf (fmap repackChangeEvent . diffTextsToChangeEventsConsolidate "foo = p") "foo = pu"

    -- , bench "defaultHandleDiff 1 change" $ nf (fst . defaultHandleDiff (transformerParams GHC.Paths.libdir) doc addU) tx

    , bench "project" $ nfIO (fst <$> project @HaskellNotebookTransformer (transformerParams GHC.Paths.libdir) "foo = p")
    , bench "projectChosenLines" $ nfIO (projectChosenLines GHC.Paths.libdir isLanguagePragmaCodeBlock (Rope.fromText "foo = p"))
    , bench "after" $ nf (applyChanges [addU]) (Rope.fromText "foo = p")


    -- , bench "handleDiffMulti no changes" $ nf (fst . handleDiffMulti (transformerParams GHC.Paths.libdir) doc []) tx
    -- , bench "handleDiffMulti 1 change" $ nf (fst . handleDiffMulti (transformerParams GHC.Paths.libdir) doc [addU]) tx
    ]


main :: IO ()
main = do
  defaultMain [
    testGroup
    ]
