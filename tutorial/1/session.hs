import GHC.Ptr (nullPtr)
import Control.Monad (when)

import Libssh (c'ssh_new, c'ssh_free)


main :: IO ()
main = do
  mySshSession <- c'ssh_new
  print mySshSession
  when (mySshSession ==  nullPtr) $ fail "session is nullptr!"
  c'ssh_free mySshSession
