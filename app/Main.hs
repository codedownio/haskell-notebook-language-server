{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as A hiding (Options)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe
import Data.Sequence hiding (zip)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types hiding (FromServerMessage'(..), FromServerMessage, FromClientMessage'(..), FromClientMessage, parseClientMessage, parseServerMessage, LookupFunc)
import qualified Language.LSP.Types.Lens as Lens
import Options.Applicative
import System.IO
import System.Posix.Signals
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.MVar
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
  }

options :: Parser Options
options = Options
  <$> optional (strOption (long "wrapped-hls" <> help "Wrapped haskell-language-server binary"))
  <*> optional (strOption (long "hls-args" <> help "Extra arguments to haskell-language-server"))

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

  (Just hlsIn, Just hlsOut, hlsErr, p) <- createProcess (
    (proc wrappedLanguageServerPath (maybe [] (fmap T.unpack . T.words) optHlsArgs)) {
        close_fds = True
        , create_group = True
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        })

  hSetBuffering stdin NoBuffering -- TODO: LineBuffering here and below?
  hSetEncoding  stdin utf8

  hSetBuffering hlsOut NoBuffering
  hSetEncoding  hlsOut utf8

  hSetBuffering stdout LineBuffering
  hSetEncoding  stdout utf8

  clientReqMap <- newMVar newClientRequestMap
  serverReqMap <- newMVar newServerRequestMap

  -- TODO: switch to using pickFromIxMap or some other way to remove old entries

  transformerState <- newTransformerState

  processWaiter <- async $ waitForProcess p

  let cleanup :: String -> IO ()
      cleanup signal = runStderrLoggingT $ do
        logInfoN [i|Got signal: #{signal}|]
        gracefullyStopProcess p 15_000_000

  void $ liftIO $ installHandler sigINT (Catch (cleanup "sigINT")) Nothing
  void $ liftIO $ installHandler sigTERM (Catch (cleanup "sigTERM")) Nothing

  runStderrLoggingT $ flip runReaderT transformerState $
    withAsync (readHlsOut clientReqMap serverReqMap hlsOut) $ \_hlsOutAsync ->
      withAsync (forever $ handleStdin hlsIn clientReqMap serverReqMap transformerState) $ \_stdinAsync -> do
        waitForProcess p >>= \case
          ExitFailure n -> logErrorN [i|haskell-language-server subprocess exited with code #{n}|]
          ExitSuccess -> logInfoN [i|haskell-language-server subprocess exited successfully|]


handleStdin hlsIn clientReqMap serverReqMap transformerState = do
  (A.eitherDecode <$> liftIO (parseStream stdin)) >>= \case
    Left err -> logErr [i|Couldn't decode incoming message: #{err}|]
    Right (x :: A.Value) -> do
      m <- readMVar serverReqMap
      case A.parseEither (parseClientMessage (lookupServerId m)) x of
        Left err -> do
          logErr [i|Couldn't decode incoming message: #{err}|]
          writeToHandle hlsIn (A.encode x)
        Right (FromClientRsp meth msg) -> do
          transformClientRsp meth msg >>= writeToHandle hlsIn . A.encode
        Right (FromClientReq meth msg) -> do
          let msgId = msg ^. Lens.id
          modifyMVar_ clientReqMap $ \m -> case updateClientRequestMap m msgId (SMethodAndParams meth (msg ^. Lens.params)) of
            Just m' -> return m'
            Nothing -> return m
          transformClientReq meth msg >>= writeToHandle hlsIn . A.encode
        Right (FromClientNot meth msg) ->
          transformClientNot meth msg >>= writeToHandle hlsIn . A.encode

readHlsOut clientReqMap serverReqMap hlsOut = forever $ do
  (A.eitherDecode <$> liftIO (parseStream hlsOut)) >>= \case
    Left err -> logErr [i|Couldn't decode HLS output: #{err}|]
    Right (x :: A.Value) -> do
      m <- readMVar clientReqMap
      case A.parseEither (parseServerMessage (lookupClientId m)) x of
        Left err -> do
          logErr [i|Couldn't decode server message: #{A.encode x} (#{err})|]
          writeToHandle stdout (A.encode x)
        Right (FromServerNot meth msg) ->
          writeToHandle stdout (A.encode (transformServerNot meth msg))
        Right (FromServerReq meth msg) -> do
          let msgId = msg ^. Lens.id
          modifyMVar_ serverReqMap $ \m -> case updateServerRequestMap m msgId (SMethodAndParams meth (msg ^. Lens.params)) of
            Just m' -> return m'
            Nothing -> return m
          writeToHandle stdout (A.encode (transformServerReq meth msg))
        Right (FromServerRsp meth initialParams msg) ->
          transformServerRsp meth initialParams msg >>= writeToHandle stdout . A.encode

lookupServerId :: ServerRequestMap -> LookupFunc FromServer
lookupServerId serverReqMap sid = do
  case lookupServerRequestMap serverReqMap sid of
    Nothing -> Nothing
    Just (SMethodAndParams meth initialParams) -> Just (meth, initialParams)

lookupClientId :: ClientRequestMap -> LookupFunc FromClient
lookupClientId clientReqMap sid = do
  case lookupClientRequestMap clientReqMap sid of
    Nothing -> Nothing
    Just (SMethodAndParams meth initialParams) -> Just (meth, initialParams)

logErr :: MonadLoggerIO m => Text -> m ()
logErr = logInfoN

writeToHandle :: MonadIO m => Handle -> BL8.ByteString -> m ()
writeToHandle h bytes = liftIO $ do
  BL8.hPutStr h [i|Content-Length: #{BL.length bytes}\r\n\r\n#{bytes}|]
  hFlush h
