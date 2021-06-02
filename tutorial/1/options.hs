import Control.Monad (when)

import Foreign (nullPtr)
import Foreign.C (CInt, withCString)

import Util (ialloca)
import Libssh
  (c'ssh_new, c'ssh_free, c'ssh_options_set,
   sshOptionsHost, sshOptionsLogVerbosity, sshOptionsPort, sshLogProtocol,)


main :: IO ()
main = do
  ses <- c'ssh_new
  when (ses ==  nullPtr) $ fail "session is nullptr!"
  print =<< withCString "localhost" (c'ssh_options_set ses sshOptionsHost)
  print =<< ialloca sshLogProtocol (c'ssh_options_set ses sshOptionsLogVerbosity)
  print =<< ialloca (22 :: CInt) (c'ssh_options_set ses sshOptionsPort)
  c'ssh_free ses
