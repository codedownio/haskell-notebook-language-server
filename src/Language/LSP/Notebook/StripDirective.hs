{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.StripDirective where

import Control.Monad.IO.Class
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Foldable
import Data.IntMap as IM hiding (toList)
import qualified Data.List as L
import Data.Sequence hiding (zip)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V hiding (zip)
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Transformer
import Language.LSP.Types
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Base (defaultExecOpt)
import Text.Regex.PCRE.Text (Regex, compile, compBlank, execute)


newtype StripDirective = StripDirective (IntMap Text)
  deriving (Show, Eq)

data SDParams = SDParams { }

instance Transformer StripDirective where
  type Params StripDirective = SDParams

  project :: Params StripDirective -> [Text] -> ([Text], StripDirective)
  project SDParams ls = (go 0 (zip [0 ..] ls) directiveIndices, StripDirective (IM.fromList $ mconcat directiveIndices))
    where
      locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (T.intercalate "\n" ls))

      go :: Int -> [(Int, Text)] -> [[(Int, Text)]] -> [Text]
      go _ [] _ = []
      go _ xs [] = fmap snd xs
      go _ xs ([]:_) = error "Empty group"
      go counter ((i, l):xs) (group@((i1,t1):is):remainingGroups)
        | i == i1 = let
            (extraLinesToProcess, remainingLines) = L.splitAt (L.length is) xs
            in "" : ["" | _ <- extraLinesToProcess] <> go (counter + 1) remainingLines remainingGroups
        | otherwise = l : go counter xs (group:remainingGroups)

      directiveIndices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                         | locatedCodeBlock@(unloc -> Directive _ t) <- locatedCodeBlocks]

      getLinesStartingAt :: String -> Int -> [(Int, Text)]
      getLinesStartingAt t startingAt = zip [startingAt..] (T.splitOn "\n" (T.pack t))

  handleDiff params changes transformer@(StripDirective indices) = (toList finalChanges, StripDirective finalIndices)
    where
      (finalChanges, finalIndices) = L.foldl' (handleSingleDiff params) (mempty, indices) changes

  transformPosition :: Params StripDirective -> StripDirective -> Position -> Maybe Position
  transformPosition SDParams (StripDirective affectedLines) (Position l c)
    | fromIntegral l `IM.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c

  untransformPosition :: Params StripDirective -> StripDirective -> Position -> Position
  untransformPosition SDParams (StripDirective affectedLines) (Position l c)
    | fromIntegral l `IM.member` affectedLines = Position l 0
    | otherwise = Position l c


handleSingleDiff ::
  Params StripDirective
  -> (Seq TextDocumentContentChangeEvent, IntMap Text)
  -> TextDocumentContentChangeEvent
  -> (Seq TextDocumentContentChangeEvent, IntMap Text)
handleSingleDiff params (changesSoFar, indices) (TextDocumentContentChangeEvent Nothing rangeLen newText) = (change' <| changesSoFar, indices')
  where
    (projectedLines, StripDirective indices') = project params (T.splitOn "\n" newText)
    change' = TextDocumentContentChangeEvent Nothing rangeLen (T.intercalate "\n" projectedLines)
handleSingleDiff params (changesSoFar, indices) (TextDocumentContentChangeEvent (Just (Range (Position l1 c1) (Position l2 c2))) rangeLen newText)
  | l1 == l2 = undefined
      where
        newLineText = undefined
