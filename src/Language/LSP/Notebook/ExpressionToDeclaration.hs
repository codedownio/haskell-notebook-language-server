{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.ExpressionToDeclaration where

import Control.Monad.IO.Class
import Data.Char (isDigit)
import qualified Data.List as L
import Data.Set as Set
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC (DynFlags)
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.Util
import Language.LSP.Parse
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Safe
import Text.Regex.Base (defaultExecOpt)
import Text.Regex.PCRE.Text (compile, compBlank, execute)


data ExpressionToDeclaration = ExpressionToDeclaration Int (Set UInt)
  deriving Show

data EDParams = EDParams {
  numberPadding :: Int
  , flags :: DynFlags
  }

isExpressionVariable :: EDParams -> Text -> Bool
isExpressionVariable (EDParams { numberPadding }) t
  | T.length t == 4 + numberPadding = case T.splitAt 4 t of
      ("expr", digits) -> T.all isDigit digits
      _ -> False
  | otherwise = False

containsExpressionVariable :: (MonadIO m, MonadFail m) => EDParams -> Text -> m Bool
containsExpressionVariable (EDParams {..}) t = do
  Right regex <- liftIO $ compile compBlank defaultExecOpt [i|expr\\d{#{numberPadding}}|]
  liftIO (execute regex t) >>= \case
    Right Nothing -> return False
    Left _err -> return False -- TODO: exception?
    _ -> return True

instance Transformer ExpressionToDeclaration where
  type Params ExpressionToDeclaration = EDParams

  transformPosition :: ExpressionToDeclaration -> Position -> Maybe Position
  transformPosition (ExpressionToDeclaration prefixLen affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l (fromIntegral (fromIntegral c + prefixLen))
    | otherwise = Just $ Position l c

  untransformPosition :: ExpressionToDeclaration -> Position -> Maybe Position
  untransformPosition (ExpressionToDeclaration prefixLen affectedLines) (Position l c)
    -- Nothing in the notebook corresponds to the prefix we added, and subtracting would wrap
    | l `Set.member` affectedLines, fromIntegral c < prefixLen = Nothing
    | l `Set.member` affectedLines = Just $ Position l (fromIntegral (fromIntegral c - prefixLen))
    | otherwise = Just $ Position l c


-- | This is a hack to deal with the fact that a top-level TH splice can look like either
-- $(deriveStuff 'f)
-- or
-- deriveStuff 'g
--
-- The latter is indistinguishable from an IHaskell expression, so we require the user to use
-- the former in notebooks.
--
-- See https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/template-haskell.html
looksLikeTemplateHaskell :: String -> Bool
looksLikeTemplateHaskell ('$':'(':rest) = lastMay rest == Just ')'
looksLikeTemplateHaskell _ = False

instance MonadIO m => ProjectIn m ExpressionToDeclaration where
  project :: Params ExpressionToDeclaration -> Doc -> m (Doc, ExpressionToDeclaration, Either Text ())
  project (EDParams {..}) (docToList -> ls) = do
    parsed <- parseCodeString flags (T.unpack (T.intercalate "\n" ls))

    let exprIndices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                      | locatedCodeBlock@(unloc -> Expression t) <- parsed
                      , not (looksLikeTemplateHaskell t)]

    return (listToDoc $ go 0 (zip ls [0 ..]) exprIndices, ExpressionToDeclaration prefixLen (Set.fromList $ fromIntegral <$> mconcat exprIndices), Right ())
    where
      prefixLen = L.length ("expr" :: String) + numberPadding + L.length (" = " :: String)

      go :: Int -> [(Text, Int)] -> [[Int]] -> [Text]
      go _ [] _ = []
      go _ xs [] = fmap fst xs
      go _ _ ([]:_) = error "Empty group"
      go counter ((l, i):xs) (group@(i1:is):remainingGroups)
        | i == i1 = let
            prefix = "expr" <> paddedNumber <> " = "
            paddedNumber = T.replicate numZerosNeeded "0" <> T.pack (show counter)
            numZerosNeeded = numberPadding - L.length (show counter)

            (extraLinesToProcess, remainingLines) = L.splitAt (L.length is) xs
            blankPadding = T.replicate prefixLen " "
            extraLines = fmap ((blankPadding <>) . fst) extraLinesToProcess

            in
              (prefix <> l) : extraLines <> go (counter + 1) remainingLines remainingGroups
        | otherwise = l : go counter xs (group:remainingGroups)
