{-# LANGUAGE TypeFamilies #-}

module Streams (ioLoop) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import qualified Data.Text as T
import UnliftIO.Exception (finally)


ioLoop :: (MonadLoggerIO m, MonadUnliftIO m) => T.Text -> IO BS.ByteString -> (BL.ByteString -> m ()) -> m ()
ioLoop loopName clientIn cb = finally (go (parse parser "")) (logDebugN [i|#{loopName}: exited|])
  where
    go r = do
      res <- parseOne loopName clientIn r
      case res of
        Left err -> do
          logErrorN $ [i|#{loopName}: failed to parse: #{err}|]
          pure ()
        Right (msg, remainder) -> do
          cb $ BL.fromStrict msg
          go (parse parser remainder)

parseOne ::
  MonadLoggerIO m
  => T.Text
  -> IO BS.ByteString
  -> Result BS.ByteString
  -> m (Either T.Text (BS.ByteString, BS.ByteString))
parseOne _loopName clientIn = go
  where
    go (Fail _ ctxs err) = do
      pure $ Left [i|Header parse fail. Ctxs: #{ctxs}. Err: #{err}.|]
    go (Partial c) = do
      bs <- liftIO clientIn
      if BS.null bs
        then pure $ Left [i|Got null bytestring.|]
        else go (c bs)
    go (Done remainder msg) = do
      pure $ Right (msg,remainder)

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
