{-# OPTIONS -Wno-name-shadowing #-}

import Control.Monad (when, void)

import Foreign (nullPtr)
import Foreign.C (withCString)

import System.IO (hPutStrLn, stderr)
import System.Exit (ExitCode (ExitFailure), exitWith)

import AuthServer (verifyKnownhost)
import Unistd (c'getpass)
import Libssh
  (c'ssh_new, c'ssh_free, c'ssh_options_set, c'ssh_connect, c'ssh_disconnect,
   sshOptionsHost, sshOk, sshSessionGetError,
   c'ssh_userauth_password,
   sshAuthSuccess,)


main :: IO ()
main = do
  -- Open session and set options
  mySshSession <- c'ssh_new
  when (mySshSession ==  nullPtr) $
    exitWith $ ExitFailure (-1)
  void $ withCString "localhost" (c'ssh_options_set mySshSession sshOptionsHost)

  -- Connect to server
  rc <- c'ssh_connect mySshSession
  when (rc /= sshOk) $ do
    hPutStrLn stderr . ("Error connecting to localhost: " ++)
      =<< sshSessionGetError mySshSession
    c'ssh_free mySshSession
    exitWith $ ExitFailure (-1)

  -- Verify the server's identity
  -- For the source code of verify_knownhost(), check previous example
  rc <- verifyKnownhost mySshSession
  when (rc < 0) $ do
    c'ssh_disconnect mySshSession
    c'ssh_free mySshSession
    exitWith $ ExitFailure (-1)

  -- Authenticate ourselves
  password <- withCString "Password: " c'getpass
  rc <- c'ssh_userauth_password mySshSession nullPtr password
  when (rc /= sshAuthSuccess) $ do
    hPutStrLn stderr . ("Error authenticating with password: " ++)
      =<< sshSessionGetError mySshSession
    c'ssh_disconnect mySshSession
    c'ssh_free mySshSession
    exitWith $ ExitFailure (-1)

  -- ...

  c'ssh_disconnect mySshSession
  c'ssh_free mySshSession
