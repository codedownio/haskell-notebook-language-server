{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Aeson as A hiding (Options)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message hiding (LookupFunc, parseClientMessage, parseServerMessage)
import Language.LSP.Protocol.Types
import Options.Applicative
import System.IO
import System.Posix.Signals
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process

import Transform.ClientNot
import Transform.ClientReq
import Transform.ClientRsp
import Transform.ServerNot
import Transform.ServerReq
import Transform.ServerRsp
import Transform.Util

import Streams
import RequestMap
import Parsing
import Process
import Control.Monad.Logger
import Control.Monad.Reader
import System.Exit


data Options = Options {
  optWrappedLanguageServer :: Maybe FilePath
  , optHlsArgs :: Maybe Text
  , optLogLevel :: Maybe Text
  }

options :: Parser Options
options = Options
  <$> optional (strOption (long "wrapped-hls" <> help "Wrapped haskell-language-server binary"))
  <*> optional (strOption (long "hls-args" <> help "Extra arguments to haskell-language-server"))
  <*> optional (strOption (long "log-level" <> help "Log level (debug, info, warn, error)"))

fullOpts :: ParserInfo Options
fullOpts = info (options <**> helper) (
  fullDesc <> progDesc "Run a wrapped haskell-language-server with notebook support"
  )

main :: IO ()
main = do
  Options {..} <- execParser fullOpts

  wrappedLanguageServerPath <- (pure optWrappedLanguageServer <|> findExecutable "haskell-language-server-wrapper") >>= \case
    Nothing -> throwIO $ userError [i|Couldn't find haskell-language-server binary.|]
    Just x -> return x

  (Just hlsIn, Just hlsOut, Just hlsErr, p) <- createProcess (
    (proc wrappedLanguageServerPath (maybe [] (fmap T.unpack . T.words) optHlsArgs)) {
        close_fds = True
        , create_group = True
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        })

  hSetBuffering stdin NoBuffering -- TODO: LineBuffering here and below?
  hSetEncoding  stdin utf8

  hSetBuffering hlsOut NoBuffering
  hSetEncoding  hlsOut utf8

  hSetBuffering stdout LineBuffering
  hSetEncoding  stdout utf8

  hSetBuffering stderr LineBuffering
  hSetEncoding  stderr utf8

  clientReqMap <- newMVar newClientRequestMap
  serverReqMap <- newMVar newServerRequestMap

  -- TODO: switch to using pickFromIxMap or some other way to remove old entries

  transformerState <- newTransformerState

  logLevel <- case optLogLevel of
    Nothing -> return LevelInfo
    Just "debug" -> return LevelDebug
    Just "info" -> return LevelInfo
    Just "warn" -> return LevelWarn
    Just "error" -> return LevelError
    Just x -> throwIO $ userError [i|Unexpected log level: '#{x}' (must be one of debug, info, warn, error)|]

  let logFilterFn :: LogSource -> LogLevel -> Bool
      logFilterFn _src level = level >= logLevel

  let cleanup :: String -> IO ()
      cleanup signal = runStderrLoggingT $ do
        logInfoN [i|Got signal: #{signal}|]
        gracefullyStopProcess p 15_000_000

  void $ liftIO $ installHandler sigINT (Catch (cleanup "sigINT")) Nothing
  void $ liftIO $ installHandler sigTERM (Catch (cleanup "sigTERM")) Nothing

  stdoutLock <- newMVar ()

  let sendToStdout :: (MonadUnliftIO m, ToJSON a) => a -> m ()
      sendToStdout x = do
        withMVar stdoutLock $ \_ -> do
          liftIO $ writeToHandle stdout $ A.encode x

  let logFn :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
      logFn _loc _src level msg = sendToStdout $ TNotificationMessage "2.0" SMethod_WindowLogMessage $ LogMessageParams {
        _type_ = levelToType level
        , _message = T.decodeUtf8 $ fromLogStr msg
        }

  flip runLoggingT logFn $ filterLogger logFilterFn $ flip runReaderT transformerState $
    withAsync (readWrappedOut clientReqMap serverReqMap hlsOut sendToStdout) $ \_hlsOutAsync ->
      withAsync (readWrappedErr hlsErr) $ \_hlsErrAsync ->
        withAsync (forever $ handleStdin hlsIn clientReqMap serverReqMap) $ \_stdinAsync -> do
          waitForProcess p >>= \case
            ExitFailure n -> logErrorN [i|haskell-language-server subprocess exited with code #{n}|]
            ExitSuccess -> logInfoN [i|haskell-language-server subprocess exited successfully|]


handleStdin :: forall m. (
  MonadLoggerIO m, MonadReader TransformerState m, MonadUnliftIO m, MonadFail m
  ) => Handle -> MVar ClientRequestMap -> MVar ServerRequestMap -> m ()
handleStdin wrappedIn clientReqMap serverReqMap = do
  (A.eitherDecode <$> liftIO (parseStream stdin)) >>= \case
    Left err -> logErr [i|Couldn't decode incoming message: #{err}|]
    Right (x :: A.Value) -> do
      m <- readMVar serverReqMap
      case A.parseEither (parseClientMessage (lookupServerId m)) x of
        Left err -> do
          logErr [i|Couldn't decode incoming message: #{err}|]
          liftIO $ writeToHandle wrappedIn (A.encode x)
        Right (ClientToServerRsp meth msg) -> do
          transformClientRsp meth msg >>= liftIO . writeToHandle wrappedIn . A.encode
        Right (ClientToServerReq meth msg) -> do
          let msgId = msg ^. Lens.id
          modifyMVar_ clientReqMap $ \m -> case updateClientRequestMap m msgId (SMethodAndParams meth (msg ^. Lens.params)) of
            Just m' -> return m'
            Nothing -> return m
          transformClientReq meth msg >>= liftIO . writeToHandle wrappedIn . A.encode
        Right (ClientToServerNot meth msg) ->
          transformClientNot sendExtraNotification meth msg >>= (liftIO . writeToHandle wrappedIn . A.encode)
  where
    sendExtraNotification :: SendExtraNotificationFn m
    sendExtraNotification msg = do
      logDebugN [i|Sending extra notification: #{A.encode msg}|]
      liftIO $ writeToHandle wrappedIn $ A.encode msg

readWrappedOut :: (
  MonadUnliftIO m, MonadLoggerIO m, MonadReader TransformerState m, MonadFail m
  ) => MVar ClientRequestMap -> MVar ServerRequestMap -> Handle -> (forall a. ToJSON a => a -> m ()) -> m b
readWrappedOut clientReqMap serverReqMap wrappedOut sendToStdout = forever $ do
  (A.eitherDecode <$> liftIO (parseStream wrappedOut)) >>= \case
    Left err -> logErr [i|Couldn't decode wrapped output: #{err}|]
    Right (x :: A.Value) -> do
      m <- readMVar clientReqMap
      case A.parseEither (parseServerMessage (lookupClientId m)) x of
        Left err -> do
          logErr [i|Couldn't decode server message: #{A.encode x} (#{err})|]
          sendToStdout x
        Right (ServerToClientNot meth msg) ->
          transformServerNot meth msg >>= sendToStdout
        Right (ServerToClientReq meth msg) -> do
          let msgId = msg ^. Lens.id
          modifyMVar_ serverReqMap $ \m -> case updateServerRequestMap m msgId (SMethodAndParams meth (msg ^. Lens.params)) of
            Just m' -> return m'
            Nothing -> return m
          sendToStdout (transformServerReq meth msg)
        Right (ServerToClientRsp meth initialParams msg) ->
          transformServerRsp meth initialParams msg >>= sendToStdout

readWrappedErr :: MonadLoggerIO m => Handle -> m ()
readWrappedErr wrappedErr = forever $ do
  line <- liftIO (hGetLine wrappedErr)
  logErrorN [i|(wrapped stderr) #{line}|]

lookupServerId :: ServerRequestMap -> LookupFunc 'ServerToClient
lookupServerId serverReqMap sid = do
  case lookupServerRequestMap serverReqMap sid of
    Nothing -> Nothing
    Just (SMethodAndParams meth initialParams) -> Just (meth, initialParams)

lookupClientId :: ClientRequestMap -> LookupFunc 'ClientToServer
lookupClientId clientReqMap sid = do
  case lookupClientRequestMap clientReqMap sid of
    Nothing -> Nothing
    Just (SMethodAndParams meth initialParams) -> Just (meth, initialParams)

logErr :: MonadLoggerIO m => Text -> m ()
logErr = logInfoN

writeToHandle :: Handle -> BL8.ByteString -> IO ()
writeToHandle h bytes = do
  BL8.hPutStr h [i|Content-Length: #{BL.length bytes}\r\n\r\n#{bytes}|]
  hFlush h

levelToType :: LogLevel -> MessageType
levelToType LevelDebug = MessageType_Log
levelToType LevelInfo = MessageType_Info
levelToType LevelWarn = MessageType_Warning
levelToType LevelError = MessageType_Error
levelToType (LevelOther _typ) = MessageType_Info
