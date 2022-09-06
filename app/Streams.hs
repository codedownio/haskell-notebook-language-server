
module Streams where

import Control.Monad
import Data.Attoparsec.ByteString as Atto
import Data.Attoparsec.ByteString.Char8 (isDigit_w8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Safe
import System.IO


parseStream :: Handle -> IO BL.ByteString
parseStream h = do
  contentLength <- readHeader 0 h
  case contentLength of
    0 -> return mempty
    n -> BL.hGet h n

readHeader :: Int -> Handle -> IO Int
readHeader contentLengthCandidate h = do
  hWaitForInput h 1_000 >>= \case
    False -> readHeader contentLengthCandidate h
    True -> do
      line <- T.hGetLine h
      let strippedHeader = T.strip line
      if | T.length strippedHeader == 0 -> return contentLengthCandidate
         | "Content-Length: " `T.isPrefixOf` strippedHeader -> do
             let maybeHeader = readMay $ T.unpack $ T.drop (T.length "Content-Length: ") strippedHeader
             case maybeHeader of
               Nothing -> return 0
               Just n -> readHeader n h
         | otherwise -> readHeader contentLengthCandidate h
