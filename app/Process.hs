{-# LANGUAGE QuasiQuotes #-}

module Process where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import Data.Maybe
import Data.String.Interpolate
import System.Process


gracefullyStopProcess :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m ()
gracefullyStopProcess p gracePeriodUs = do
  liftIO $ interruptProcessGroupOf p
  gracefullyWaitForProcess p gracePeriodUs

gracefullyWaitForProcess :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m ()
gracefullyWaitForProcess p gracePeriodUs = do
  let waitForExit = do
        let policy = limitRetriesByCumulativeDelay gracePeriodUs $ capDelay 200_000 $ exponentialBackoff 1_000
        retrying policy (\_ x -> return $ isNothing x) $ \_ -> do
          liftIO $ getProcessExitCode p

  waitForExit >>= \case
    Just _ -> return ()
    Nothing -> do
      pid <- liftIO $ getPid p
      logWarnN [i|(#{pid}) Process didn't stop after #{gracePeriodUs}us; trying to interrupt|]

      liftIO $ interruptProcessGroupOf p
      waitForExit >>= \case
        Just _ -> return ()
        Nothing -> void $ do
          logWarnN [i|(#{pid}) Process didn't stop after a further #{gracePeriodUs}us; going to kill|]
          liftIO $ terminateProcess p
          liftIO $ waitForProcess p
