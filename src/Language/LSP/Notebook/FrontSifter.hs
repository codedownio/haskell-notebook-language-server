{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module Language.LSP.Notebook.FrontSifter where

import Data.Bifunctor
import Data.Bits
import Data.Function
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector as V hiding (zip)
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHCParser
import Language.LSP.Notebook.Util
import Language.LSP.Parse
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


newtype ImportSifter = ImportSifter (Vector Int)
  deriving Show
instance Transformer ImportSifter where
  type Params ImportSifter = FilePath
  project ghcLibDir = second ImportSifter . projectChosenLines ghcLibDir isImportCodeBlock
  transformPosition _ (ImportSifter indices) = transformUsingIndices indices
  untransformPosition _ (ImportSifter indices) = Just . untransformUsingIndices indices

newtype PragmaSifter = PragmaSifter (Vector Int)
  deriving Show
instance Transformer PragmaSifter where
  type Params PragmaSifter = FilePath
  project ghcLibDir = second PragmaSifter . projectChosenLines ghcLibDir isLanguagePragmaCodeBlock
  transformPosition _ (PragmaSifter indices) = transformUsingIndices indices
  untransformPosition _ (PragmaSifter indices) = Just . untransformUsingIndices indices

-- * Generic transformer functions

projectChosenLines :: FilePath -> (CodeBlock -> Maybe String) -> Doc -> (Doc, Vector Int)
projectChosenLines ghcLibDir chooseFn (docToList -> ls) = (listToDoc (chosenLines <> nonChosenLines), fromList importIndices)
  where
    getLinesStartingAt t startingAt = [startingAt..(startingAt + countNewLines t)]

    importIndices = mconcat [getLinesStartingAt t (GHCParser.line locatedCodeBlock - 1)
                            | locatedCodeBlock@((chooseFn . unloc) -> Just t) <- parseCodeString ghcLibDir (T.unpack (T.intercalate "\n" ls))]

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

binarySearchVec :: Vector Int -> Int -> (Int, Bool)
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
