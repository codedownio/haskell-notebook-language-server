{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Criterion
import Criterion.Main
import Data.Diff.Myers
import qualified Data.Diff.Types as DT
import Data.Row.Records
import Data.String.Interpolate
import qualified Data.Text.Rope as Rope
import qualified GHC.Paths
import Language.LSP.Notebook
import Language.LSP.Notebook.FrontSifter
import Language.LSP.Parse
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


(doc, tx :: HaskellNotebookTransformer) = project (transformerParams GHC.Paths.libdir) (Rope.fromText "foo = p")
addU = TextDocumentContentChangeEvent $ InL (#range .== (Range (p 0 7) (p 0 7)) .+ #rangeLength .== Nothing .+ #text .== "u")

p :: Int -> Int -> Position
p l c = Position (fromIntegral l) (fromIntegral c)


repackChangeEvent (DT.ChangeEvent range text) = TextDocumentContentChangeEvent $ InL $ #range .== repackRange range .+ #rangeLength .== Nothing .+ #text .== text
repackRange (DT.Range (DT.Position l1 c1) (DT.Position l2 c2)) = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))

testGroup :: Benchmark
testGroup =
  bgroup [i|Parsing|] [
    -- bench "parse foo = putSt" $ nf parseCodeString "foo = putSt"
    -- , bench "parse foo = putStrLn" $ nf parseCodeString "foo = putStrLn \"hi\""

    -- bench "diffTextsToChangeEventsConsolidate" $ nf (fmap repackChangeEvent . diffTextsToChangeEventsConsolidate "foo = p") "foo = pu"

    -- , bench "defaultHandleDiff 1 change" $ nf (fst . defaultHandleDiff (transformerParams GHC.Paths.libdir) doc addU) tx

    bench "project" $ nf (fst . project @HaskellNotebookTransformer (transformerParams GHC.Paths.libdir)) "foo = p"
    , bench "projectChosenLines" $ nf (projectChosenLines GHC.Paths.libdir isLanguagePragmaCodeBlock) (Rope.fromText "foo = p")
    , bench "projectChosenLines'" $ nf (projectChosenLines' GHC.Paths.libdir isLanguagePragmaCodeBlock) (Rope.fromText "foo = p")
    , bench "projectChosenLines''" $ nf (projectChosenLines'' GHC.Paths.libdir isLanguagePragmaCodeBlock) (Rope.fromText "foo = p")
    -- , bench "after" $ nf (applyChanges [addU]) (Rope.fromText "foo = p")


    -- , bench "handleDiffMulti no changes" $ nf (fst . handleDiffMulti (transformerParams GHC.Paths.libdir) doc []) tx
    -- , bench "handleDiffMulti 1 change" $ nf (fst . handleDiffMulti (transformerParams GHC.Paths.libdir) doc [addU]) tx
    ]


main :: IO ()
main = do
  defaultMain [
    testGroup
    ]
