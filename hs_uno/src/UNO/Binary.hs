module UNO.Binary where

import Foreign

data UnoInterface

data Context

type ContextPtr = Ptr Context

data CssUnoType

foreign import ccall "bootstrap" unoBootstrap
  :: IO ContextPtr

-- *Any

data AnyRef

anyToInterface :: Ptr AnyRef -> IO (Ptr a)
anyToInterface aptr = cAnyToInterface aptr >>= return . castPtr

foreign import ccall "anyToInterface" cAnyToInterface
  :: Ptr AnyRef -> IO (Ptr ())
