{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Language.LSP.Transformer
import Language.LSP.Types
import Lib
import System.IO.Unsafe (unsafePerformIO)


newtype FrontSifter = FrontSifter (Vector Int)
  deriving Show


instance Transformer FrontSifter where
  type Params FrontSifter = ()

  project :: Params FrontSifter -> [Text] -> ([Text], FrontSifter)
  project () ls = let
    locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (T.intercalate "\n" ls))

    countNewLines ('\n':xs) = 1 + countNewLines xs
    countNewLines (_:xs) = countNewLines xs
    countNewLines [] = 0

    getImportLinesStartingAt t startingAt = [startingAt..(startingAt + countNewLines t)]

    importIndices = mconcat [getImportLinesStartingAt t (GHC.line locatedCodeBlock - 1)
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

    in (chosenLines <> nonChosenLines, FrontSifter (fromList importIndices))

  -- TODO: efficient implementation
  -- handleDiff :: Params FrontSifter -> [Text] -> [Text] -> [TextDocumentContentChangeEvent] -> FrontSifter -> ([Text], [Text], [TextDocumentContentChangeEvent], FrontSifter)
  -- handleDiff () before after changes x@(FrontSifter indices) = (before', after', fmap transformChange changes, x')
  --   where
  --     (before', _) = project @FrontSifter () before
  --     (after', x') = project @FrontSifter () after

  --     transformChange change@(TextDocumentContentChangeEvent {..}) = case _range of
  --       Just r -> undefined
  --       Nothing -> undefined

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

binarySearchVec = binarySearchVec' @Int

{-# SPECIALISE binarySearchVec' :: Vector Int -> Int -> (Int, Bool) #-}
binarySearchVec' :: forall a. (Num a, Eq a, Bits a, Ord a, Integral a) => Vector a -> a -> (a, Bool)
binarySearchVec' [] _ = (0, False)
binarySearchVec' vec desired = binarySearchVec' 0 (fromIntegral $ V.length vec)
  where
    binarySearchVec' :: a -> a -> (a, Bool)
    binarySearchVec' lb ub | lb == ub = if
      | lb < 0 -> (0, False)
      | ub >= fromIntegral (V.length vec) -> (fromIntegral (V.length vec), False)
      | vec ! fromIntegral lb == desired -> (lb, True)
      | otherwise -> (lb, False)
    binarySearchVec' lb ub = case midValue `compare` desired of
      LT -> if mid > lb then binarySearchVec' mid ub else (lb + 1, False)
      EQ -> (mid, True)
      GT -> if mid < ub then binarySearchVec' lb mid else (lb, False)
      where
        mid = shift (lb + ub) (-1)
        midValue = vec ! fromIntegral mid
