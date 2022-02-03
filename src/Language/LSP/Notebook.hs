{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.LSP.Notebook where

import Data.Sequence hiding (zip)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Transformer
import Language.LSP.Types
import Lib
import System.IO.Unsafe (unsafePerformIO)


newtype FrontSifter = FrontSifter (Seq Int)
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

  handleDiff :: Params FrontSifter -> [Text] -> [Text] -> [TextDocumentContentChangeEvent] -> FrontSifter -> ([Text], [Text], [TextDocumentContentChangeEvent], FrontSifter)
  handleDiff = undefined

  transformPosition :: Params FrontSifter -> FrontSifter -> Position -> Maybe Position
  transformPosition = undefined

  untransformPosition :: Params FrontSifter -> FrontSifter -> Position -> Position
  untransformPosition = undefined
