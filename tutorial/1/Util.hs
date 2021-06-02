module Util (ialloca) where

import Foreign (Ptr, Storable (poke), alloca)


ialloca :: Storable a => a -> (Ptr a -> IO b) -> IO b
ialloca x act = alloca $ \p -> poke p x >> act p
