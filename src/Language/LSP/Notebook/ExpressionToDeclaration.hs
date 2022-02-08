{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Language.LSP.Notebook.ExpressionToDeclaration where

import qualified Data.List as L
import Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector as V hiding (zip)
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Transformer
import Language.LSP.Types
import System.IO.Unsafe (unsafePerformIO)


newtype ExpressionToDeclaration = ExpressionToDeclaration (Set Int)
  deriving Show

data EDParams = EDParams { numberPadding :: Int }

instance Transformer ExpressionToDeclaration where
  type Params ExpressionToDeclaration = EDParams

  project :: Params ExpressionToDeclaration -> [Text] -> ([Text], ExpressionToDeclaration)
  project (EDParams {..}) ls = (go 0 (zip ls [0..]) exprIndices, ExpressionToDeclaration (Set.fromList $ mconcat exprIndices))
    where
      locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (T.intercalate "\n" ls))

      go :: Int -> [(Text, Int)] -> [[Int]] -> [Text]
      go _ [] _ = []
      go _ xs [] = fmap fst xs
      go _ xs ([]:_) = error "Empty group"
      go counter ((l, i):xs) (group@(i1:is):remainingGroups)
        | i == i1 = let
            prefix = "expr" <> paddedNumber <> " = "
            paddedNumber = T.replicate numZerosNeeded "0" <> T.pack (show counter)
            numZerosNeeded = numberPadding - L.length (show counter)
            prefixLen = 4 + numberPadding + 3

            (extraLinesToProcess, remainingLines) = L.splitAt (L.length is) xs
            blankPadding = T.replicate prefixLen " "
            extraLines = fmap ((blankPadding <>) . fst) extraLinesToProcess

            in
              (prefix <> l) : extraLines <> go (counter + 1) remainingLines remainingGroups
        | otherwise = l : go counter xs (group:remainingGroups)

      exprIndices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                    | locatedCodeBlock@(unloc -> Expression t) <- locatedCodeBlocks]

      getLinesStartingAt :: String -> Int -> [Int]
      getLinesStartingAt t startingAt = [startingAt..(startingAt + countNewLines t)]

      countNewLines ('\n':xs) = 1 + countNewLines xs
      countNewLines (_:xs) = countNewLines xs
      countNewLines [] = 0

  handleDiff :: Params ExpressionToDeclaration -> [Text] -> [Text] -> [TextDocumentContentChangeEvent] -> ExpressionToDeclaration -> ([Text], [Text], [TextDocumentContentChangeEvent], ExpressionToDeclaration)
  handleDiff (EDParams {..}) before after changes x@(ExpressionToDeclaration indices) = undefined

  transformPosition :: Params ExpressionToDeclaration -> ExpressionToDeclaration -> Position -> Maybe Position
  transformPosition (EDParams {..}) (ExpressionToDeclaration indices) (Position l c) = undefined

  untransformPosition :: Params ExpressionToDeclaration -> ExpressionToDeclaration -> Position -> Position
  untransformPosition (EDParams {..}) (ExpressionToDeclaration indices) (Position l c) = undefined
