{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module Language.LSP.Notebook.FrontSifter where

import Data.Bifunctor
import Data.Bits
import Data.Function
import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector as V hiding (zip)
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.Util
import Language.LSP.Transformer
import Language.LSP.Types
import System.IO.Unsafe (unsafePerformIO)


newtype ImportSifter = ImportSifter (Vector Int)
  deriving Show
instance Transformer ImportSifter where
  type Params ImportSifter = ()

  project :: Params ImportSifter -> [Text] -> ([Text], ImportSifter)
  project () = second ImportSifter . projectChosenLines isImportCodeBlock

  transformPosition :: Params ImportSifter -> ImportSifter -> Position -> Maybe Position
  transformPosition () (ImportSifter indices) = transformUsingIndices indices

  untransformPosition :: Params ImportSifter -> ImportSifter -> Position -> Position
  untransformPosition () (ImportSifter indices) = untransformUsingIndices indices

newtype PragmaSifter = PragmaSifter (Vector Int)
  deriving Show
instance Transformer PragmaSifter where
  type Params PragmaSifter = ()

  project :: Params PragmaSifter -> [Text] -> ([Text], PragmaSifter)
  project () = second PragmaSifter . projectChosenLines isLanguagePragmaCodeBlock

  transformPosition :: Params PragmaSifter -> PragmaSifter -> Position -> Maybe Position
  transformPosition () (PragmaSifter indices) = transformUsingIndices indices

  untransformPosition :: Params PragmaSifter -> PragmaSifter -> Position -> Position
  untransformPosition () (PragmaSifter indices) = untransformUsingIndices indices

-- * Generic transformer functions

projectChosenLines :: (CodeBlock -> Maybe String) -> [Text] -> ([Text], Vector Int)
projectChosenLines chooseFn ls = (chosenLines <> nonChosenLines, fromList importIndices)
  where
    locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (T.intercalate "\n" ls))

    getLinesStartingAt t startingAt = [startingAt..(startingAt + countNewLines t)]

    importIndices = mconcat [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                            | locatedCodeBlock@((chooseFn . unloc) -> Just t) <- locatedCodeBlocks]

    partitionLines :: [Int] -> [(Int, Text)] -> ([Text], [Text])
    partitionLines [] remaining = ([], fmap snd remaining)
    partitionLines _ [] = error "Failed to partition lines"
    partitionLines all@(nextDesired:xs) ((curIndex, curLine):ys)
      | nextDesired == curIndex = let (chosen, notChosen) = partitionLines xs ys in
          (curLine : chosen, notChosen)
      | otherwise = let (chosen, notChosen) = partitionLines all ys in
          (chosen, curLine : notChosen)

    (chosenLines, nonChosenLines) = partitionLines importIndices (zip [0..] ls)


transformUsingIndices :: Vector Int -> Position -> Maybe Position
transformUsingIndices indices (Position l c) = case binarySearchVec indices (fromIntegral l) of
  (i, True) -> Just (Position (fromIntegral i) c)
  (i, False) -> Just (Position (l + fromIntegral (V.length indices - i)) c)

untransformUsingIndices :: Integral a => Vector a -> Position -> Position
untransformUsingIndices indices (Position l c)
  | l < fromIntegral (V.length indices) = Position (fromIntegral (indices ! (fromIntegral l))) c
  | otherwise = Position finalL c
      where
        finalL = flip fix (V.length indices - 1, l) $ \loop -> \case
          (-1, currentL) -> fromIntegral currentL
          (chosenLineIndex, currentL) -> let chosenOrig = indices ! chosenLineIndex in
            if | chosenOrig >= fromIntegral currentL -> loop (chosenLineIndex - 1, currentL - 1)
               | otherwise -> (fromIntegral currentL)

-- * Choose functions

isImportCodeBlock :: CodeBlock -> Maybe String
isImportCodeBlock (Import t) = Just t
isImportCodeBlock _ = Nothing

isLanguagePragmaCodeBlock :: CodeBlock -> Maybe String
isLanguagePragmaCodeBlock (Pragma PragmaLanguage langs) = Just [__i|{-\# LANGUAGE #{L.unwords langs} \#-}|]
isLanguagePragmaCodeBlock _ = Nothing

-- * Binary search

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



foo = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString [__i|{-\# LANGUAGE TemplateHaskell \#-}
                                                                          import Data.Aeson.TH
                                                                          data Foo = Bar | Baz
                                                                          deriveJSON defaultOptions ''Foo
                                                                         |]
