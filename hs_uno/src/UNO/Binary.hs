module UNO.Binary where

import Foreign

data UnoInterface

foreign import ccall "bootstrap" unoBootstrap
  :: IO ()
