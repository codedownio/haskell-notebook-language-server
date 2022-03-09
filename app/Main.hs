{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Control.Monad
import Data.Aeson as A hiding (Options)
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
  <$> optional (strOption (long "--wrapped-hls" <> help "Wrapped haskell-language-server binary"))
  <*> optional (strOption (long "--hls-args" <> help "Extra arguments to haskell-language-server"))

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

  (p, hlsIn, Just hlsOut, hlsErr) <- createProcess (
    (proc wrappedLanguageServerPath (maybe [] (fmap T.unpack . T.words) optHlsArgs)) {
        close_fds = True
        , create_group = True
        , std_out = CreatePipe
        , std_err = Inherit
        })

  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering hlsOut NoBuffering
  hSetEncoding  hlsOut utf8

  inputReader <- async $ forever $ do
    (A.eitherDecode <$> parseStream stdin) >>= \case
      Left err -> undefined
      Right (x :: ()) -> undefined

  hlsReader <- async $ forever $ do
    (A.eitherDecode <$> parseStream hlsOut) >>= \case
      Left err -> undefined
      Right (x :: ()) -> undefined

  (asyncWhichStopped, result :: Either SomeException ()) <- waitAnyCatchCancel [inputReader, hlsReader]

  putStrLn [i|Final result: #{result}|]
