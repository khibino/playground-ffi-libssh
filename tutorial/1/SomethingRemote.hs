{-# OPTIONS -Wno-name-shadowing #-}

module SomethingRemote (
  showRemoteProcesses,
  )where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Monoid (Last (..))

import Foreign (nullPtr, allocaBytes)
import Foreign.C (CInt, withCString, peekCStringLen)

import Libssh (SshSession, sshOk, sshError,
               c'ssh_channel_new, c'ssh_channel_close,  c'ssh_channel_free,
               c'ssh_channel_open_session, c'ssh_channel_request_exec,
               c'ssh_channel_read, c'ssh_channel_send_eof, )

showRemoteProcesses :: SshSession -> IO CInt
showRemoteProcesses session = (either (maybe sshError id . getLast) (const sshOk) <$>) .
                              runExceptT $ do
  let throwRC = throwE . Last . Just
  channel <- liftIO $ c'ssh_channel_new session
  when (channel == nullPtr) $ throwRC sshError

  rc <- liftIO $ c'ssh_channel_open_session channel
  when (rc /= sshOk) $ do
    liftIO $ c'ssh_channel_free channel
    throwRC rc

  rc <- liftIO $ withCString "ps aux" (c'ssh_channel_request_exec channel)
  when (rc /= sshOk) $ do
    liftIO $ do
      void $ c'ssh_channel_close channel
      c'ssh_channel_free channel
    throwRC rc

  let size = 256
  nbytes <- liftIO $ allocaBytes (fromIntegral size) $ \buffer -> do
    let loop = do
          nbytes <- c'ssh_channel_read channel buffer size 0
          if nbytes > 0
            then do putStr =<< peekCStringLen (buffer, fromIntegral nbytes)
                    loop
            else    return nbytes
    loop

  when (nbytes < 0) $ do
    liftIO $ do
      void $ c'ssh_channel_close channel
      c'ssh_channel_free channel
    throwRC sshError

  liftIO $ do
    void $ c'ssh_channel_send_eof channel
    void $ c'ssh_channel_close channel
    c'ssh_channel_free channel
