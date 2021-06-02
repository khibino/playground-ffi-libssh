{-# OPTIONS -Wno-name-shadowing #-}

import Control.Applicative (empty)
import Control.Monad (when, void)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)

import Foreign (nullPtr, peek)
import Foreign.C (CInt, withCString)

import Util (ialloca)
import Libssh
  (SshSession,
   c'ssh_new, c'ssh_free, c'ssh_options_set, c'ssh_connect, c'ssh_get_fd, c'ssh_disconnect,
   sshOptionsHost, sshRcOK, sshSessionGetError,
   c'ssh_get_server_publickey, c'ssh_get_publickey_hash, c'ssh_key_free,
   sshPublickeyHashSha1,
   c'ssh_session_is_known_server, c'ssh_clean_pubkey_hash, c'ssh_print_hexa,
   sshKnownHostsError, sshKnownHostsNotFound, sshKnownHostsUnknown,
   sshKnownHostsOk, sshKnownHostsChanged, sshKnownHostsOther,
   sshGetHexA, sshSessionUpdateKnownHosts, sshSessionGetError, )


verifyKnownhost :: SshSession -> IO CInt
verifyKnownhost session = do
  ialloca nullPtr $ \keyp ->
    ialloca nullPtr $ \hashp ->
      ialloca  0      $ \lenp -> (maybe (-1) (const 0) <$>) . runMaybeT $ do
    rc <- liftIO $ c'ssh_get_server_publickey session keyp  -- fill keyp
    when (rc < 0) empty

    rc <- liftIO $ do
      key <- peek keyp
      rc <- c'ssh_get_publickey_hash key sshPublickeyHashSha1 hashp lenp  -- fill hashp and lenp
      c'ssh_key_free key
      return rc
    when (rc < 0) empty

    state <- liftIO $ c'ssh_session_is_known_server session
    let dispatch
          | state == sshKnownHostsOk       =  return () :: MaybeT IO ()
          | state == sshKnownHostsChanged  =  do
              liftIO $ do
                hPutStrLn stderr "Host key for server changed: it is now:"
                hash <- peek hashp
                len  <- peek lenp
                withCString "Public key hash" (\s -> c'ssh_print_hexa s hash len)
                hPutStrLn stderr "For security reasons, connection will be stopped"
                c'ssh_clean_pubkey_hash hashp
              empty

          | state == sshKnownHostsOther    =  do
              liftIO $ do
                hPutStrLn stderr $ unlines
                  ["The host key for this server was not found but an other \
                   \type of key exists.",
                   "An attacker might change the default server key to \
                   \confuse your client into thinking the key does not exist"]
                c'ssh_clean_pubkey_hash hashp
              empty

          | state == sshKnownHostsNotFound ||
            state == sshKnownHostsUnknown  =  do
              liftIO $ do
                when (state == sshKnownHostsNotFound) $
                  hPutStrLn stderr $ unlines
                  ["Could not find known host file.",
                   "If you accept the host key here, the file will be \
                   \automatically created."]

                hash <- peek hashp
                len  <- peek lenp
                hexa <- sshGetHexA hash len
                hPutStrLn stderr $ unlines
                  ["The server is unknown. Do you trust the host key?",
                   "Public key hash: " ++ hexa]
                c'ssh_clean_pubkey_hash hashp

              buf <- liftIO $ take 3 <$> getLine
              when (buf /= "yes") empty

              maybe
                (return ())
                (\em -> do
                   liftIO $ hPutStrLn stderr $ "Error " ++ em
                   empty)
                =<< liftIO (sshSessionUpdateKnownHosts session)

          | state == sshKnownHostsError   =  do
              liftIO $ do
                em <- sshSessionGetError session
                hPutStrLn stderr $ "Error " ++ em
              empty

          | otherwise                     =  do
              liftIO $ hPutStrLn stderr $ "Error: Unknown state: " ++ show state
              empty

    dispatch
    return ()

main :: IO ()
main = do
  ses <- c'ssh_new
  when (ses ==  nullPtr) $ fail "session is nullptr!"
  void $ withCString "localhost" (c'ssh_options_set ses sshOptionsHost)
  rc <- c'ssh_connect ses
  when (rc /= sshRcOK) $ do
    em <- sshSessionGetError ses
    fail $ "Error connecting to localhost: " ++ em
  print =<< c'ssh_get_fd ses

  rc <- verifyKnownhost ses
  when (rc < 0) $ fail "known_hosts verification error!"

  c'ssh_disconnect ses
  c'ssh_free ses
