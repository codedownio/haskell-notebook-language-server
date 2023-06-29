{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.StripDirective where

import qualified Data.List as L
import Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.Util
import Language.LSP.Parse
import Language.LSP.Transformer
import Language.LSP.Protocol.Types


newtype StripDirective = StripDirective (Set UInt)
  deriving Show

data SDParams = SDParams { }

instance Transformer StripDirective where
  type Params StripDirective = SDParams

  project :: Params StripDirective -> Doc -> (Doc, StripDirective)
  project SDParams (docToList -> ls) = (listToDoc $ go 0 (zip ls [0 ..]) directiveIndices, StripDirective (Set.fromList $ fromIntegral <$> mconcat directiveIndices))
    where
      go :: Int -> [(Text, Int)] -> [[Int]] -> [Text]
      go _ [] _ = []
      go _ xs [] = fmap fst xs
      go counter ((l, i):xs) (group@(i1:is):remainingGroups)
        | i == i1 = let
            (extraLinesToProcess, remainingLines) = L.splitAt (L.length is) xs
            in "" : ["" | _ <- extraLinesToProcess] <> go (counter + 1) remainingLines remainingGroups
        | otherwise = l : go counter xs (group:remainingGroups)

      directiveIndices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                         | locatedCodeBlock@(unloc -> Directive _ t) <- parseCodeString (T.unpack (T.intercalate "\n" ls))]

  transformPosition :: Params StripDirective -> StripDirective -> Position -> Maybe Position
  transformPosition SDParams (StripDirective affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c

  untransformPosition :: Params StripDirective -> StripDirective -> Position -> Maybe Position
  untransformPosition SDParams (StripDirective affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c
