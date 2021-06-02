import GHC.Ptr (nullPtr)
import Control.Monad (when)

import Libssh (c'ssh_new, c'ssh_free)


main :: IO ()
main = do
  ses <- c'ssh_new
  print ses
  when (ses ==  nullPtr) $ fail "session is nullptr!"
  c'ssh_free ses
