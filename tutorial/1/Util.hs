module Util (ialloca) where

import Foreign (Ptr, Storable (poke), alloca)


-- action with pointer to specified immediate-value
ialloca :: Storable a => a -> (Ptr a -> IO b) -> IO b
ialloca x act = alloca $ \p -> poke p x >> act p
