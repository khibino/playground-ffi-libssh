module String where

#include <string.h>

import Foreign.C (CInt (..), CString)

foreign import ccall unsafe "string.h strerror"
  c'strerror :: CInt -> IO CString
