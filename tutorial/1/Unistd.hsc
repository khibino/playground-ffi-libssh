module Unistd where

#include <unistd.h>

import Foreign.C (CString)

foreign import ccall unsafe "unistd.h getpass"
  c'getpass :: CString -> IO CString
