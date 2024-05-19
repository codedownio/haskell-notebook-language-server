{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.StatementToDeclaration where

import Control.Monad.IO.Class
import Data.Bifunctor
import qualified Data.List as L
import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.Util
import Language.LSP.Parse
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


data LineInfo = LineInfo {
  lineInfoDivideAt :: UInt
  } deriving (Show, Eq)

newtype StatementToDeclaration = StatementToDeclaration (Map UInt LineInfo)
  deriving (Show, Semigroup, Monoid)

data STDParams = STDParams {
  stdGhcLibDir :: FilePath
  }

instance Transformer StatementToDeclaration where
  type Params StatementToDeclaration = STDParams

  project :: MonadIO m => Params StatementToDeclaration -> Doc -> m (Doc, StatementToDeclaration)
  project (STDParams {..}) (docToList -> ls) = do
    parsed <- parseCodeString stdGhcLibDir (T.unpack (T.intercalate "\n" ls))

    let indices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                  | locatedCodeBlock@(unloc -> Statement t) <- parsed]

    return (first listToDoc $ go 0 (zip ls [0 ..]) indices)
    where
      go :: Int -> [(Text, Int)] -> [[Int]] -> ([Text], StatementToDeclaration)
      go _ [] _ = ([], mempty)
      go _ xs [] = (fmap fst xs, mempty)
      go _ _ ([]:_) = error "Empty group"
      go counter ((l, i):xs) (group@(i1:is):remainingGroups)
        | i == i1 = let
            (lhs, T.drop 2 -> rhs) = T.breakOn "<-" l

            (extraLines, remainingLines) = L.splitAt (L.length is) xs

            newFirstLine = lhs <> "= unsafePerformIO $ " <> rhs
            newLines = newFirstLine : fmap fst extraLines

            params = StatementToDeclaration $ M.fromList [(fromIntegral i, LineInfo (fromIntegral (T.length lhs)))]

            (restLines, restParams) = go (counter + 1) remainingLines remainingGroups

            in
              (newLines <> restLines, params <> restParams)

        | otherwise = let
            (restLines, restParams) = go counter xs (group:remainingGroups)
            in
              (l : restLines, restParams)

  transformPosition :: Params StatementToDeclaration -> StatementToDeclaration -> Position -> Maybe Position
  transformPosition (STDParams {}) (StatementToDeclaration affectedLines) (Position l c) = case l `M.lookup` affectedLines of
    Nothing -> Just $ Position l c
    Just (LineInfo leftEnd)
      | c <= leftEnd + 1 -> Just $ Position l c
      | otherwise -> Just $ Position l (c + insertedLen)

  untransformPosition :: Params StatementToDeclaration -> StatementToDeclaration -> Position -> Maybe Position
  untransformPosition (STDParams {}) (StatementToDeclaration affectedLines) (Position l c) = case l `M.lookup` affectedLines of
    Nothing -> Just $ Position l c
    Just (LineInfo leftEnd)
      | c <= leftEnd + 1 -> Just $ Position l c
      | c >= leftEnd + insertedLen -> Just $ Position l (c - insertedLen)
      | otherwise -> Nothing

insertedLen :: UInt
insertedLen = 20 -- Length of "= unsafePerformIO $ "
