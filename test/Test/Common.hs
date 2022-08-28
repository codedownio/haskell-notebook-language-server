{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Common where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import GHC.Stack
import Language.LSP.Transformer
import Language.LSP.Types hiding (Reason, line)
import Test.QuickCheck as Q
import Test.Sandwich
import UnliftIO.Exception


transformAndUntransform params from to x = do
  transformPosition params x from `shouldBe` (Just to)
  untransformPosition params x to `shouldBe` from


arbitrarySingleLineChange :: [Text] -> Gen TextDocumentContentChangeEvent
arbitrarySingleLineChange docLines = do
  lineNo <- chooseInt (0, fromIntegral $ L.length docLines - 1)
  let line = docLines L.!! (fromIntegral lineNo)

  let lineLen = T.length line
  pos1 <- chooseInt (0, max 0 (fromIntegral (lineLen - 1)))
  pos2 <- chooseInt (pos1, (max pos1 (fromIntegral (lineLen - 1))))

  toInsert :: String <- arbitrary

  pure $ TextDocumentContentChangeEvent (Just (Range (p lineNo pos1) (p lineNo pos2))) Nothing (T.pack toInsert)

arbitraryChange :: [Text] -> Gen TextDocumentContentChangeEvent
arbitraryChange docLines = do
  let totalLen = T.length (T.intercalate "\n" docLines)
  pos1 <- chooseInt (0, max 0 (fromIntegral (totalLen - 1)))
  pos2 <- chooseInt (pos1, (max pos1 (fromIntegral (totalLen - 1))))

  toInsert :: [String] <- arbitrary

  let posToPosition = undefined

  let position1 = posToPosition pos1
  let position2 = posToPosition pos2

  pure $ TextDocumentContentChangeEvent (Just (Range position1 position2)) Nothing (T.intercalate "\n" $ fmap T.pack toInsert)

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

    (changes, transformer') = handleDiff params [change] transformer

    afterFromChange' = applyChangesTextSilent changes projectedBefore

-- * Util

p :: Int -> Int -> Position
p l c = Position (fromIntegral l) (fromIntegral c)

isSingleLineChange :: [TextDocumentContentChangeEvent] -> [Property]
isSingleLineChange [TextDocumentContentChangeEvent (Just (Range (Position l1 c1) (Position l2 c2))) _ txt] =
  [l1 === l2 .&&. (L.length (T.splitOn "\n" txt) === 1)]
isSingleLineChange [TextDocumentContentChangeEvent Nothing Nothing txt] =
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


docLines :: [Text]
docLines = T.splitOn "\n" doc

doc :: Text
doc = [i|

\# Here's a Markdown document!

```{python}
foo = 42
print(foo)
```

and some Haskell code:

```{haskell}
putStrLn "hi there"
bar = 42
```

Now let's do a directive

```{haskell}
:t bar
```

Finally, an import

```{haskell}
import Data.Aeson
```

|]
