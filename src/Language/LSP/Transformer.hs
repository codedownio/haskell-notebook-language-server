{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.LSP.Transformer (
  Transformer(..)
  , SomeTransformer
  , (:>)(..)
  , defaultHandleDiff

  , applyChangesText
  , applyChangesTextSilent
  ) where

import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Logger
import Data.Function
import Data.Kind
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Data.Text.Utf16.Rope (Rope)
import qualified Data.Text.Utf16.Rope as Rope
import Language.LSP.Types as J


class Transformer a where
  type Params a

  project :: Params a -> [Text] -> ([Text], a)

  handleDiff :: Params a -> [Text] -> [Text] -> [TextDocumentContentChangeEvent] -> a -> ([Text], [Text], [TextDocumentContentChangeEvent], a)
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
  handleDiff (xParams :> yParams) before after change (x :> y) = (before'', after'', change'', x' :> y')
    where
      (before', after', change', x') = handleDiff xParams before after change x
      (before'', after'', change'', y') = handleDiff yParams before' after' change' y
  transformPosition (xParams :> yParams) (x :> y) p = transformPosition xParams x p >>= transformPosition yParams y
  untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition xParams x (untransformPosition yParams y p)
  -- untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition yParams y p >>= untransformPosition xParams x

data SomeTransformer where
  SomeTransformer :: forall a. (Transformer a) => a -> Params a -> SomeTransformer

-- Inefficient default implementation; instances should define their own
defaultHandleDiff :: forall a. Transformer a => Params a -> [Text] -> [Text] -> [TextDocumentContentChangeEvent] -> a -> ([Text], [Text], [TextDocumentContentChangeEvent], a)
defaultHandleDiff params before after _change _transformer = (before', after', change', transformer')
  where
    (before', _ :: a) = project params before
    (after', transformer' :: a) = project params after
    change' = [TextDocumentContentChangeEvent Nothing Nothing (T.intercalate "\n" after')]

-- * Applying changes

-- Careful about using Rope.toTextLines because it behaves similarly to Data.Text.lines, i.e.
-- T.lines ["foo"] == T.lines ["foo\n"] == ["foo"]
-- import qualified Data.Text.Utf16.Lines as Lines


applyChangesText :: (MonadLogger m) => [J.TextDocumentContentChangeEvent] -> [Text] -> m [Text]
applyChangesText changes before = do
  afterRope <- applyChanges (before & T.intercalate "\n" & Rope.fromText) changes
  return $ Rope.toText afterRope
         & T.splitOn "\n"

applyChangesTextSilent :: [J.TextDocumentContentChangeEvent] -> [Text] -> [Text]
applyChangesTextSilent changes before = runIdentity $ do
  afterRope <- runNoLoggingT $ applyChanges (before & T.intercalate "\n" & Rope.fromText) changes
  return $ Rope.toText afterRope
         & T.splitOn "\n"

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
 case Rope.splitAtPosition finish str of
   Nothing -> do
     logWarnN [i|Split inside code point (#{start}, #{finish}): #{str}|]
     pure str
   Just (before, after) ->  case Rope.splitAtPosition start before of
     Nothing -> do
       logWarnN [i|Split inside code point with finish (#{start}, #{finish}): #{str}|]
       pure str
     Just (before', _) -> pure $ mconcat [before', Rope.fromText new, after]
