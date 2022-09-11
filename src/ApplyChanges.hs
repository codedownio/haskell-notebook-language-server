
module ApplyChanges where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Logger
import Data.Function
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Utf16.Lines as Lines
import Data.Text.Utf16.Rope ( Rope )
import qualified Data.Text.Utf16.Rope as Rope
import qualified Language.LSP.Types as J


applyChangesText :: (MonadLogger m) => [J.TextDocumentContentChangeEvent] -> [Text] -> m [Text]
applyChangesText changes before = do
  afterRope <- applyChanges (before & T.intercalate "\n" & Rope.fromText) changes
  return $ Rope.toTextLines afterRope
         & Lines.lines

applyChangesTextSilent :: [J.TextDocumentContentChangeEvent] -> [Text] -> [Text]
applyChangesTextSilent changes before = runIdentity $ do
  afterRope <- runNoLoggingT $ applyChanges (before & T.intercalate "\n" & Rope.fromText) changes
  return $ Rope.toTextLines afterRope
         & Lines.lines

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
