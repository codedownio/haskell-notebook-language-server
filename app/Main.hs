{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Control.Monad
import Data.Aeson as A hiding (Options)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe
import Data.Sequence hiding (zip)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Options.Applicative
import Streams
import System.IO
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process


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

  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering hlsOut NoBuffering
  hSetEncoding  hlsOut utf8

  withAsync (readHlsOut hlsOut) $ \_ ->
    forever $ do
      (A.eitherDecode <$> parseStream stdin) >>= \case
        Left err -> ioError $ userError [i|Couldn't decode incoming message: #{err}|]
        Right (x :: A.Value) -> writeToHandle hlsIn (A.encode x)

readHlsOut hlsOut = forever $ do
  (A.eitherDecode <$> parseStream hlsOut) >>= \case
    Left err -> ioError $ userError [i|Couldn't decode HLS output: #{err}|]
    Right (x :: A.Value) -> writeToHandle stdout (A.encode x)


writeToHandle :: Handle -> BL8.ByteString -> IO ()
writeToHandle h bytes =
  BL8.hPutStrLn h [i|Content-Length: #{BL.length bytes}\r\n\r\n#{bytes}|]
