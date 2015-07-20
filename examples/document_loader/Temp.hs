module Temp where

import UNO
import Foreign

data Sequence a

instance Service (Ptr a) where
  getInterface = castPtr
