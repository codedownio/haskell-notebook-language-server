{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.Transformer (
  Doc

  , Transformer(..)
  , SomeTransformer
  , (:>)(..)
  , defaultHandleDiff

  , listToDoc
  , applyChangesText
  , applyChangesTextSilent
  ) where

import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Logger
import Data.Function
import Data.Kind
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Types as J


type Doc = Rope.Rope

class Transformer a where
  type Params a

  project :: Params a -> Doc -> (Doc, a)

  handleDiffMulti :: Params a -> Doc -> [TextDocumentContentChangeEvent] -> a -> ([TextDocumentContentChangeEvent], a)
  handleDiffMulti params before changes tx = (finalChanges, finalTx)
    where
      (finalChanges, finalLines, finalTx) = L.foldl' f ([], before, tx) changes

      f :: ([TextDocumentContentChangeEvent], Doc, a) -> TextDocumentContentChangeEvent -> ([TextDocumentContentChangeEvent], Doc, a)
      f (changesSoFar, curLines, txSoFar) change = let (newChanges, tx') = handleDiff params curLines change txSoFar in
          (changesSoFar <> newChanges, applyChangesTextSilent [change] curLines, tx')

  handleDiff :: Params a -> Doc -> TextDocumentContentChangeEvent -> a -> ([TextDocumentContentChangeEvent], a)
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
defaultHandleDiff :: forall a. Transformer a => Params a -> Doc -> TextDocumentContentChangeEvent -> a -> ([TextDocumentContentChangeEvent], a)
defaultHandleDiff params before change _transformer = (change', transformer')
  where
    (before', _ :: a) = project params before
    after = applyChangesTextSilent [change] before
    (after', transformer' :: a) = project params after
    change' = [TextDocumentContentChangeEvent Nothing Nothing (Rope.toText after')]

-- * Applying changes

applyChangesText :: (MonadLogger m) => [J.TextDocumentContentChangeEvent] -> Doc -> m Doc
applyChangesText changes before = applyChanges before changes

applyChangesTextSilent :: [J.TextDocumentContentChangeEvent] -> Doc -> Doc
applyChangesTextSilent changes before = runIdentity $ runNoLoggingT $ applyChanges before changes

listToDoc :: [Text] -> Doc
listToDoc = Rope.fromText . T.intercalate "\n"

-- * Based on code from haskell-lsp/lsp (https://github.com/haskell/lsp/tree/master/lsp)
-- Under MIT license

applyChanges :: (MonadLogger m) => Rope -> [J.TextDocumentContentChangeEvent] -> m Rope
applyChanges = foldM applyChange

applyChange :: (MonadLogger m) => Rope -> J.TextDocumentContentChangeEvent -> m Rope
applyChange _ (J.TextDocumentContentChangeEvent Nothing _ str)
  = pure $ Rope.fromText str
applyChange str (J.TextDocumentContentChangeEvent (Just (J.Range (J.Position sl sc) (J.Position fl fc))) _ txt)
  = changeChars str (Rope.Position (fromIntegral sl) (fromIntegral sc)) (Rope.Position (fromIntegral fl) (fromIntegral fc)) txt

changeChars :: (MonadLogger m) => Rope -> Rope.Position -> Rope.Position -> Text -> m Rope
changeChars str start finish new = do
 let (before, after) = Rope.splitAtPosition finish str
 let (before', _) = Rope.splitAtPosition start before
 pure $ mconcat [before', Rope.fromText new, after]
