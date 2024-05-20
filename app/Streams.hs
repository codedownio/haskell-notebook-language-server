
module Streams where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL


ioLoop :: MonadIO m => IO BS.ByteString -> (BL.ByteString -> m ()) -> m ()
ioLoop clientIn cb = go (parse parser mempty)
  where
    go r = do
      res <- parseOne clientIn r
      case res of
        Nothing -> pure ()
        Just (msg, remainder) -> do
          cb $ BL.fromStrict msg
          go (parse parser remainder)

parser :: Parser BS.ByteString
parser = do
  try contentType <|> return ()
  len <- contentLength
  try contentType <|> return ()
  _ <- string _ONE_CRLF
  Attoparsec.take len

contentLength :: Parser Int
contentLength = do
  _ <- string "Content-Length: "
  len <- decimal
  _ <- string _ONE_CRLF
  return len

contentType :: Parser ()
contentType = do
  _ <- string "Content-Type: "
  skipWhile (/= '\r')
  _ <- string _ONE_CRLF
  return ()

_ONE_CRLF :: BS.ByteString
_ONE_CRLF = "\r\n"
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"

parseOne ::
  MonadIO m
  => IO BS.ByteString
  -> Result BS.ByteString
  -> m (Maybe (BS.ByteString, BS.ByteString))
parseOne clientIn = go
  where
    go (Fail _ _ctxs _err) = do
      -- logger <& HeaderParseFail ctxs err `WithSeverity` Error
      pure Nothing
    go (Partial c) = do
      bs <- liftIO clientIn
      if BS.null bs
        then do
          -- logger <& EOF `WithSeverity` Error
          pure Nothing
        else go (c bs)
    go (Done remainder msg) = do
      pure $ Just (msg,remainder)
