module Temp where

import UNO
import Foreign

data Sequence a

instance Service (ForeignPtr a) where
  getInterface = castForeignPtr
