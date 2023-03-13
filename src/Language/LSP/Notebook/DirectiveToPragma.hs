{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.DirectiveToPragma where

import Control.Monad.IO.Class
import Data.Char (isDigit)
import Data.Either (fromRight)
import qualified Data.List as L
import Data.Set as Set
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Data.Vector as V hiding (all, zip)
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.Util
import Language.LSP.Transformer
import Language.LSP.Types
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Base (defaultExecOpt)
import Text.Regex.PCRE.Text (Regex, compile, compBlank, execute)


newtype DirectiveToPragma = DirectiveToPragma (Set UInt)
  deriving (Show, Eq)

data DTPParams = DTPParams { }

instance Transformer DirectiveToPragma where
  type Params DirectiveToPragma = DTPParams

  project :: Params DirectiveToPragma -> Doc -> (Doc, DirectiveToPragma)
  project DTPParams doc@(docToList -> ls) = (listToDoc $ go 0 (zip ls [0 ..]) directiveIndices, DirectiveToPragma (Set.fromList $ fromIntegral <$> mconcat (fmap fst directiveIndices)))
    where
      locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (Rope.toText doc))

      go :: Int -> [(Text, Int)] -> [([Int], [String])] -> [Text]
      go _ [] _ = []
      go _ xs [] = fmap fst xs
      go counter ((l, ix):xs) (group@(i1:is, languageOptions):remainingGroups)
        | ix == i1 = let
            (extraLinesToProcess, remainingLines) = L.splitAt (L.length is) xs
            in [i|{-\# LANGUAGE #{L.unwords languageOptions} \#-}|] : ["" | _ <- extraLinesToProcess] <> go (counter + 1) remainingLines remainingGroups
        | otherwise = l : go counter xs (group:remainingGroups)

      directiveIndices :: [([Int], [String])]
      directiveIndices = [(getLinesStartingAt t (GHC.line locatedCodeBlock - 1), fmap unflagLanguageOption (L.words t))
                         | locatedCodeBlock@(unloc -> Directive SetDynFlag t) <- locatedCodeBlocks
                         , all isLanguageOption (L.words t)]

      isLanguageOption :: String -> Bool
      isLanguageOption ('-':'X':xs) = True
      isLanguageOption _ = False

      unflagLanguageOption :: String -> String
      unflagLanguageOption = L.drop 2

  -- TODO: do better here
  transformPosition :: Params DirectiveToPragma -> DirectiveToPragma -> Position -> Maybe Position
  transformPosition DTPParams (DirectiveToPragma affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c

  -- TODO: do better here
  untransformPosition :: Params DirectiveToPragma -> DirectiveToPragma -> Position -> Position
  untransformPosition DTPParams (DirectiveToPragma affectedLines) (Position l c)
    | l `Set.member` affectedLines = Position l 0
    | otherwise = Position l c

  handleDiff :: Params DirectiveToPragma -> Doc -> TextDocumentContentChangeEvent -> DirectiveToPragma -> ([TextDocumentContentChangeEvent], DirectiveToPragma)
  handleDiff params before (TextDocumentContentChangeEvent Nothing _ t) (DirectiveToPragma affectedLines) = ([TextDocumentContentChangeEvent Nothing Nothing (Rope.toText doc')], tx)
    where (doc', tx) = project @DirectiveToPragma params (Rope.fromText t)
  handleDiff params before change@(TextDocumentContentChangeEvent (Just range@(Range (Position l1 c1) (Position l2 c2))) _ t) (DirectiveToPragma currentAffectedLines)
    | l1 == l2 && numAddedLines == 1 = undefined
        -- where
        --   newLine = undefined
        --   lineStatusChanged = undefined
    | otherwise = undefined
    where
      after = applyChange change before

      numAddedLines = (T.count "\n" t) + 1

      affectedLineBoundsInclusive = (l1, l2 + fromIntegral numAddedLines - 1)

      -- (beginningLines, rest) = Rope.splitAtLine (fromIntegral l1) before
      -- (modifiedLines, endingLines) = Rope.splitAtLine (fromIntegral (l2 - l1)) rest
