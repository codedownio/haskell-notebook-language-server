{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.Transformer (
  Doc

  , Transformer(..)
  , (:>)(..)
  , defaultHandleDiff

  , listToDoc
  , docToList

  , applyChange
  , applyChanges
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Diff.Myers
import qualified Data.Diff.Types as DT
import Data.Kind
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Types as J


type Doc = Rope.Rope

class Transformer a where
  type Params a

  project :: MonadIO m => Params a -> Doc -> m (Doc, a)

  handleDiffMulti :: MonadIO m => Params a -> Doc -> [TextDocumentContentChangeEvent] -> a -> m ([TextDocumentContentChangeEvent], a)
  handleDiffMulti params before changes tx = do
    (finalChanges, _finalLines, finalTx) <- foldM f ([], before, tx) changes
    return (finalChanges, finalTx)

    where
      f :: MonadIO m => ([TextDocumentContentChangeEvent], Doc, a) -> TextDocumentContentChangeEvent -> m ([TextDocumentContentChangeEvent], Doc, a)
      f (changesSoFar, curLines, txSoFar) change = do
          (newChanges, tx') <- handleDiff params curLines change txSoFar
          return (changesSoFar <> newChanges, applyChanges [change] curLines, tx')

  handleDiff :: MonadIO m => Params a -> Doc -> TextDocumentContentChangeEvent -> a -> m ([TextDocumentContentChangeEvent], a)
  handleDiff = defaultHandleDiff

  transformPosition :: Params a -> a -> Position -> Maybe Position

  untransformPosition :: Params a -> a -> Position -> Maybe Position

data (a :: Type) :> (b :: Type) = a :> b
  deriving Show
infixr :>

instance (Transformer a, Transformer b) => Transformer (a :> b) where
  type Params (a :> b) = Params a :> Params b
  project (xParams :> yParams) lines = do
    (lines', x) <- project xParams lines
    (lines'', y) <- project yParams lines'
    return (lines'', x :> y)

  handleDiff (xParams :> yParams) before change (x :> y) = do
    (change', x') <- handleDiff xParams before change x
    projected <- project @a xParams before
    (change'', y') <- handleDiffMulti yParams (fst projected) change' y
    return (change'', x' :> y')

  transformPosition (xParams :> yParams) (x :> y) p = transformPosition xParams x p >>= transformPosition yParams y
  untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition yParams y p >>= untransformPosition xParams x
  -- untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition yParams y p >>= untransformPosition xParams x

-- Default implementation uses diff.
defaultHandleDiff :: forall a m. (Transformer a, MonadIO m) => Params a -> Doc -> TextDocumentContentChangeEvent -> a -> m ([TextDocumentContentChangeEvent], a)
defaultHandleDiff params before change _transformer = do
  (before', _ :: a) <- project params before
  let after = applyChanges [change] before
  (after', transformer' :: a) <- project params after
  let change' = fmap repackChangeEvent $ diffTextsToChangeEventsConsolidate (Rope.toText before') (Rope.toText after')
  return (change', transformer')

  where
    repackChangeEvent (DT.ChangeEvent range text) = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial (repackRange range) Nothing text
    repackRange (DT.Range (DT.Position l1 c1) (DT.Position l2 c2)) = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))

-- * Applying changes

listToDoc :: [Text] -> Doc
listToDoc = Rope.fromText . T.intercalate "\n"

docToList :: Doc -> [Text]
docToList = T.splitOn "\n" . Rope.toText

-- * Based on code from haskell-lsp/lsp (https://github.com/haskell/lsp/tree/master/lsp)
-- Under MIT license

applyChanges :: [J.TextDocumentContentChangeEvent] -> Rope -> Rope
applyChanges changes rope = L.foldl' (flip applyChange) rope changes

applyChange :: J.TextDocumentContentChangeEvent -> Rope -> Rope
applyChange (J.TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial {..}))) str =
  changeChars str (Rope.Position (fromIntegral sl) (fromIntegral sc)) (Rope.Position (fromIntegral fl) (fromIntegral fc)) _text
  where
    J.Range (J.Position sl sc) (J.Position fl fc) = _range
applyChange (J.TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument {..}))) _ = Rope.fromText _text

changeChars :: Rope -> Rope.Position -> Rope.Position -> Text -> Rope
changeChars str start finish new = mconcat [before', Rope.fromText new, after]
 where
   (before, after) = Rope.splitAtPosition finish str
   (before', _) = Rope.splitAtPosition start before
