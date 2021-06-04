module Unistd where

#include <unistd.h>

import Foreign.C (CString)

-- char *getpass (const char *__prompt) __nonnull ((1));
-- return pointer is pointing to static buffer
foreign import ccall unsafe "unistd.h getpass"
  c'getpass :: CString -> IO CString
