{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TestLib.Generators (
  arbitrarySingleLineChange
  , testChange
  , testChange'

  , isSingleLineChange
  , mkChange
  , quickCheckSingleProp
  ) where

import ApplyChanges
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.List as L
import Data.Text as T
import GHC.Stack
import Language.LSP.Transformer
import Language.LSP.Types hiding (Reason, line)
import Test.QuickCheck as Q
import Test.Sandwich
import UnliftIO.Exception


arbitrarySingleLineChange :: [Text] -> Gen TextDocumentContentChangeEvent
arbitrarySingleLineChange docLines = do
  lineNo <- chooseInt (0, fromIntegral $ L.length docLines - 1)
  let line = docLines L.!! (fromIntegral lineNo)

  let lineLen = T.length line
  pos1 <- chooseInt (0, max 0 (fromIntegral (lineLen - 1)))
  pos2 <- chooseInt (pos1, (max pos1 (fromIntegral (lineLen - 1))))

  toInsert :: String <- arbitrary

  pure $ TextDocumentContentChangeEvent (Just (Range (p lineNo pos1) (p lineNo pos2))) Nothing (T.pack toInsert)


testChange :: forall a. (
  Transformer a, Eq a, Show a
  ) => Params a -> [Text] -> TextDocumentContentChangeEvent -> Property
testChange = testChange' @a (const [])

testChange' :: forall a. (
  Transformer a, Eq a, Show a
  ) => ([TextDocumentContentChangeEvent] -> [Property]) -> Params a -> [Text] -> TextDocumentContentChangeEvent -> Property
testChange' extraProps params docLines change = conjoin ([
  -- Applying the change' returned from handleDiff to the projected before value gives expected projected value
  afterFromChange' === projectedAfter

  -- The re-projected transformer matches the one we got back from handleDiff
  , reprojectedTransformer === transformer'
  ] <> extraProps changes)

  where
    -- Expected un-projected document after the change
    docLines' = applyChangesTextSilent [change] docLines

    (projectedBefore, transformer :: a) = project params docLines
    (projectedAfter, reprojectedTransformer :: a) = project params docLines'

    (_, _, changes, transformer') = handleDiff params docLines docLines' [change] transformer

    afterFromChange' = applyChangesTextSilent changes projectedBefore

-- * Util

p :: Int -> Int -> Position
p l c = Position (fromIntegral l) (fromIntegral c)

isSingleLineChange :: [TextDocumentContentChangeEvent] -> [Property]
isSingleLineChange [TextDocumentContentChangeEvent (Just (Range (Position l1 _c1) (Position l2 _c2))) _ txt] =
  [l1 === l2 .&&. (L.length (T.splitOn "\n" txt) === 1)]
isSingleLineChange [TextDocumentContentChangeEvent Nothing Nothing _txt] =
  [True === False]
isSingleLineChange [] = []
isSingleLineChange _ = error "Unexpected TextDocumentContentChangeEvent"

mkChange :: (UInt, UInt) -> (UInt, UInt) -> Maybe UInt -> Text -> TextDocumentContentChangeEvent
mkChange (l1, c1) (l2, c2) = TextDocumentContentChangeEvent (Just (Range (Position l1 c1) (Position l2 c2)))

quickCheckSingleProp :: (MonadIO m, Testable prop, MonadLogger m) => prop -> m ()
quickCheckSingleProp prop = do
  liftIO (quickCheckWithResult (stdArgs { Q.chatty = False, Q.maxSuccess = 1 }) prop) >>= \case
    Q.Success {..} -> info (T.pack output)
    x -> throwIO $ Reason (Just callStack) (output x)
