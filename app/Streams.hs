
module Streams where

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

-- | TODO: see if the real HLS uses hWaitForInput with a delay like this?
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



-- ioLoop ::
--   forall config
--   .  LogAction IO (WithSeverity LspServerLog)
--   -> LogAction (LspM config) (WithSeverity LspServerLog)
--   -> IO BS.ByteString
--   -> ServerDefinition config
--   -> VFS
--   -> (FromServerMessage -> IO ())
--   -> IO ()
-- ioLoop ioLogger logger clientIn serverDefinition vfs sendMsg = do
--   minitialize <- parseOne ioLogger clientIn (parse parser "")
--   case minitialize of
--     Nothing -> pure ()
--     Just (msg,remainder) -> do
--       case J.eitherDecode $ BSL.fromStrict msg of
--         Left err -> ioLogger <& DecodeInitializeError err `WithSeverity` Error
--         Right initialize -> do
--           mInitResp <- Processing.initializeRequestHandler serverDefinition vfs sendMsg initialize
--           case mInitResp of
--             Nothing -> pure ()
--             Just env -> runLspT env $ loop (parse parser remainder)
--   where

--     loop :: Result BS.ByteString -> LspM config ()
--     loop = go
--       where
--         pLogger =  L.cmap (fmap LspProcessingLog) logger
--         go r = do
--           res <- parseOne logger clientIn r
--           case res of
--             Nothing -> pure ()
--             Just (msg,remainder) -> do
--               Processing.processMessage pLogger $ BSL.fromStrict msg
--               go (parse parser remainder)

--     parser = do
--       _ <- string "Content-Length: "
--       len <- decimal
--       _ <- string _TWO_CRLF
--       Attoparsec.take len

-- parseOne ::
--   MonadIO m
--   => LogAction m (WithSeverity LspServerLog)
--   -> IO BS.ByteString
--   -> Result BS.ByteString
--   -> m (Maybe (BS.ByteString,BS.ByteString))
-- parseOne logger clientIn = go
--   where
--     go (Fail _ ctxs err) = do
--       logger <& HeaderParseFail ctxs err `WithSeverity` Error
--       pure Nothing
--     go (Partial c) = do
--       bs <- liftIO clientIn
--       if BS.null bs
--         then do
--           logger <& EOF `WithSeverity` Error
--           pure Nothing
--         else go (c bs)
--     go (Done remainder msg) = do
--       logger <& ParsedMsg (T.decodeUtf8 msg) `WithSeverity` Debug
--       pure $ Just (msg,remainder)
