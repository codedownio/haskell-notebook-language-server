{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.Transformer (
  Transformer(..)
  , SomeTransformer
  , (:>)(..)
  , defaultHandleDiff
  ) where

import ApplyChanges (applyChangesTextSilent)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Logger
import Data.Function
import Data.Kind
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Data.Text.Utf16.Rope (Rope)
import qualified Data.Text.Utf16.Rope as Rope
import Language.LSP.Types as J


class Transformer a where
  type Params a

  project :: Params a -> [Text] -> ([Text], a)

  handleDiffMulti ::
    Params a
    -> [Text] -- ^ Before text
    -> [TextDocumentContentChangeEvent]
    -> a
    -> ([TextDocumentContentChangeEvent], a)
  handleDiffMulti params before changes tx = undefined -- (finalChanges, finalTx)
    where
      (finalChanges, finalLines, finalTx) = L.foldl' f ([], before, tx) changes

      f :: ([TextDocumentContentChangeEvent], [Text], a) -> TextDocumentContentChangeEvent -> ([TextDocumentContentChangeEvent], [Text], a)
      f (changesSoFar, curLines, txSoFar) change = let (newChanges, tx') = handleDiff params curLines change txSoFar in
          (changesSoFar <> newChanges, applyChangesTextSilent [change] curLines, tx')

  handleDiff :: Params a -> [Text] -> TextDocumentContentChangeEvent -> a -> ([TextDocumentContentChangeEvent], a)
  handleDiff = defaultHandleDiff

  transformPosition :: Params a -> a -> Position -> Maybe Position

  untransformPosition :: Params a -> a -> Position -> Position
  -- untransformPosition :: Params a -> a -> Position -> Maybe Position

data (a :: Type) :> (b :: Type) = a :> b
  deriving Show
infixr :>

instance (Transformer a, Transformer b) => Transformer (a :> b) where
  type Params (a :> b) = Params a :> Params b
  project (xParams :> yParams) lines = (lines'', x :> y)
    where
      (lines', x) = project xParams lines
      (lines'', y) = project yParams lines'
  handleDiff (xParams :> yParams) before change (x :> y) = (change'', x' :> y')
    where
      (change', x') = handleDiff xParams before change x
      (change'', y') = handleDiffMulti yParams (fst (project @a xParams before)) change' y
  transformPosition (xParams :> yParams) (x :> y) p = transformPosition xParams x p >>= transformPosition yParams y
  untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition xParams x (untransformPosition yParams y p)
  -- untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition yParams y p >>= untransformPosition xParams x

data SomeTransformer where
  SomeTransformer :: forall a. (Transformer a) => a -> Params a -> SomeTransformer

-- Inefficient default implementation; instances should define their own
defaultHandleDiff :: forall a. Transformer a => Params a -> [Text] -> TextDocumentContentChangeEvent -> a -> ([TextDocumentContentChangeEvent], a)
defaultHandleDiff params before change _transformer = (change', transformer')
  where
    (before', _ :: a) = project params before
    after = applyChangesTextSilent [change] before
    (after', transformer' :: a) = project params after
    change' = [TextDocumentContentChangeEvent Nothing Nothing (T.intercalate "\n" after')]
