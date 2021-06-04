{-# OPTIONS -Wno-name-shadowing #-}

import Control.Monad (when, void)

import Foreign (nullPtr)
import Foreign.C (withCString)

import AuthServer (verifyKnownhost)
import Libssh
  (c'ssh_new, c'ssh_free, c'ssh_options_set, c'ssh_connect, c'ssh_get_fd, c'ssh_disconnect,
   sshOptionsHost, sshOk, sshSessionGetError, )

main :: IO ()
main = do
  mySshSession <- c'ssh_new
  when (mySshSession ==  nullPtr) $ fail "session is nullptr!"
  void $ withCString "localhost" (c'ssh_options_set mySshSession sshOptionsHost)
  rc <- c'ssh_connect mySshSession
  when (rc /= sshOk) $ do
    em <- sshSessionGetError mySshSession
    fail $ "Error connecting to localhost: " ++ em
  print =<< c'ssh_get_fd mySshSession

  rc <- verifyKnownhost mySshSession
  when (rc < 0) $ fail "known_hosts verification error!"

  c'ssh_disconnect mySshSession
  c'ssh_free mySshSession
