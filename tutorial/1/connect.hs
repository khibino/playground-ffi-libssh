import Control.Monad (when, void)

import Foreign (nullPtr)
import Foreign.C (withCString)

import Libssh
  (c'ssh_new, c'ssh_free, c'ssh_options_set, c'ssh_connect, c'ssh_get_fd, c'ssh_disconnect,
   sshOptionsHost, sshRcOK, sshSessionGetError)


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
  c'ssh_disconnect ses
  c'ssh_free ses
