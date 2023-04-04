{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.StatementToDeclaration where

import Data.Bifunctor
import qualified Data.List as L
import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.Util
import Language.LSP.Parse
import Language.LSP.Transformer
import Language.LSP.Types


data LineInfo = LineInfo {
  lineInfoDivideAt :: UInt
  , lineInfoHasClosingParen :: Bool
  } deriving (Show, Eq)

newtype StatementToDeclaration = StatementToDeclaration (Map UInt LineInfo)
  deriving (Show, Semigroup, Monoid)

data STDParams = STDParams

instance Transformer StatementToDeclaration where
  type Params StatementToDeclaration = STDParams

  project :: Params StatementToDeclaration -> Doc -> (Doc, StatementToDeclaration)
  project (STDParams) (docToList -> ls) = first listToDoc $ go 0 (zip ls [0 ..]) indices
    where
      go :: Int -> [(Text, Int)] -> [[Int]] -> ([Text], StatementToDeclaration)
      go _ [] _ = ([], mempty)
      go _ xs [] = (fmap fst xs, mempty)
      go _ _ ([]:_) = error "Empty group"
      go counter ((l, i):xs) (group@(i1:is):remainingGroups)
        | i == i1 = let
            (lhs, T.drop 2 -> rhs) = T.breakOn "<-" l

            (extraLines, remainingLines) = L.splitAt (L.length is) xs

            hasExtraLines = not $ L.null extraLines

            newFirstLine = lhs <> "= unsafePerformIO (" <> rhs <> (if not hasExtraLines then ")" else "")
            newLines = newFirstLine : fmap fst extraLines

            params = StatementToDeclaration $ M.fromList [(fromIntegral i, LineInfo (fromIntegral (T.length lhs)) (not hasExtraLines))]

            (restLines, restParams) = go (counter + 1) remainingLines remainingGroups

            in
              (newLines <> restLines, params <> restParams)

        | otherwise = let
            (restLines, restParams) = go counter xs (group:remainingGroups)
            in
              (l : restLines, restParams)

      indices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                | locatedCodeBlock@(unloc -> Statement t) <- parseCodeString (T.unpack (T.intercalate "\n" ls))]

  transformPosition :: Params StatementToDeclaration -> StatementToDeclaration -> Position -> Maybe Position
  transformPosition (STDParams) (StatementToDeclaration affectedLines) (Position l c) = case l `M.lookup` affectedLines of
    Nothing -> Just $ Position l c
    Just (LineInfo leftEnd _hasClosingParen)
      | c <= leftEnd + 1 -> Just $ Position l c
      | otherwise -> Just $ Position l (c + insertedLen)

  untransformPosition :: Params StatementToDeclaration -> StatementToDeclaration -> Position -> Position
  untransformPosition (STDParams) (StatementToDeclaration affectedLines) (Position l c) = case l `M.lookup` affectedLines of
    Nothing -> Position l c
    Just (LineInfo leftEnd _hasClosingParen)
      | c <= leftEnd + 1 -> Position l c
      | c >= leftEnd + insertedLen -> Position l (c - insertedLen)
      | otherwise -> Position l leftEnd

insertedLen :: UInt
insertedLen = 19 -- Length of "= unsafePerformIO ("
