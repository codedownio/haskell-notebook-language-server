{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module Language.LSP.Notebook.FrontSifter where

import Data.Bits
import Data.Function
import Data.Functor.Identity (Identity(runIdentity))
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector as V hiding (zip)
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.FrontSifter.Util
import Language.LSP.Transformer
import Language.LSP.Types
import System.IO.Unsafe (unsafePerformIO)


newtype FrontSifter = FrontSifter (Vector Int)
  deriving (Show, Eq)


instance Transformer FrontSifter where
  type Params FrontSifter = ()

  project :: Params FrontSifter -> [Text] -> ([Text], FrontSifter)
  project () ls = (chosenLines <> nonChosenLines, FrontSifter (fromList importIndices))
    where
      locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (T.intercalate "\n" ls))

      countNewLines ('\n':xs) = 1 + countNewLines xs
      countNewLines (_:xs) = countNewLines xs
      countNewLines [] = 0

      getLinesStartingAt t startingAt = [startingAt..(startingAt + countNewLines t)]

      importIndices = mconcat [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                              | locatedCodeBlock@(unloc -> Import t) <- locatedCodeBlocks]

      partitionLines :: [Int] -> [(Int, Text)] -> ([Text], [Text])
      partitionLines [] remaining = ([], fmap snd remaining)
      partitionLines _ [] = error "Failed to partition lines"
      partitionLines all@(nextDesired:xs) ((curIndex, curLine):ys)
        | nextDesired == curIndex = let (chosen, notChosen) = partitionLines xs ys in
            (curLine : chosen, notChosen)
        | otherwise = let (chosen, notChosen) = partitionLines all ys in
            (chosen, curLine : notChosen)

      (chosenLines, nonChosenLines) = partitionLines importIndices (zip [0..] ls)

  handleDiff () changes transformer@(FrontSifter indices) = undefined

  transformPosition :: Params FrontSifter -> FrontSifter -> Position -> Maybe Position
  transformPosition () (FrontSifter indices) (Position l c) = case binarySearchVec indices (fromIntegral l) of
    (i, True) -> Just (Position (fromIntegral i) c)
    (i, False) -> Just (Position (l + fromIntegral (V.length indices - i)) c)

  untransformPosition :: Params FrontSifter -> FrontSifter -> Position -> Position
  untransformPosition () (FrontSifter indices) (Position l c)
    | l < fromIntegral (V.length indices) = Position (fromIntegral (indices ! (fromIntegral l))) c
    | otherwise = Position finalL c
        where
          finalL = flip fix (V.length indices - 1, l) $ \loop -> \case
            (-1, currentL) -> fromIntegral currentL
            (chosenLineIndex, currentL) -> let chosenOrig = indices ! chosenLineIndex in
              if | chosenOrig >= fromIntegral currentL -> loop (chosenLineIndex - 1, currentL - 1)
                 | otherwise -> (fromIntegral currentL)
