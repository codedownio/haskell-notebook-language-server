{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.List as L
import Data.Set as Set
import Data.String.Interpolate
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
import Text.Regex.Base (defaultExecOpt)
import Text.Regex.PCRE.Text (Regex, compile, compBlank, execute)


newtype StripDirective = StripDirective (Set UInt)
  deriving (Show, Eq)

data SDParams = SDParams { }

instance Transformer StripDirective where
  type Params StripDirective = SDParams

  project :: Params StripDirective -> [Text] -> ([Text], StripDirective)
  project SDParams ls = (go 0 (zip ls [0 ..]) directiveIndices, StripDirective (Set.fromList $ fromIntegral <$> mconcat directiveIndices))
    where
      locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (T.intercalate "\n" ls))

      go :: Int -> [(Text, Int)] -> [[Int]] -> [Text]
      go _ [] _ = []
      go _ xs [] = fmap fst xs
      go _ xs ([]:_) = error "Empty group"
      go counter ((l, i):xs) (group@(i1:is):remainingGroups)
        | i == i1 = let
            (extraLinesToProcess, remainingLines) = L.splitAt (L.length is) xs
            in "" : ["" | _ <- extraLinesToProcess] <> go (counter + 1) remainingLines remainingGroups
        | otherwise = l : go counter xs (group:remainingGroups)

      directiveIndices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                         | locatedCodeBlock@(unloc -> Directive _ t) <- locatedCodeBlocks]

      getLinesStartingAt :: String -> Int -> [Int]
      getLinesStartingAt t startingAt = [startingAt..(startingAt + countNewLines t)]

      countNewLines ('\n':xs) = 1 + countNewLines xs
      countNewLines (_:xs) = countNewLines xs
      countNewLines [] = 0

  handleDiff (SDParams {}) changes transformer@(StripDirective indices) = undefined

  transformPosition :: Params StripDirective -> StripDirective -> Position -> Maybe Position
  transformPosition SDParams (StripDirective affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c

  untransformPosition :: Params StripDirective -> StripDirective -> Position -> Position
  untransformPosition SDParams (StripDirective affectedLines) (Position l c)
    | l `Set.member` affectedLines = Position l 0
    | otherwise = Position l c
