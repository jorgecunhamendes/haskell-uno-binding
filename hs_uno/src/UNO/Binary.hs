module UNO.Binary where

import Foreign
import Foreign.C
import System.Environment

data UnoInterface

data Context

type ContextPtr = Ptr Context

data CssUnoType

unoBootstrap :: IO ContextPtr
unoBootstrap = do
  progname <- getExecutablePath
  args <- getArgs
  withStringsArray (progname : args) $ \ argsPtr -> do
    cUnoBootstrap (1 + length args) argsPtr

foreign import ccall "bootstrap" cUnoBootstrap
  :: Int -> Ptr CString -> IO ContextPtr

-- *Any

data AnyRef

anyToInterface :: Ptr AnyRef -> IO (Ptr a)
anyToInterface aptr = cAnyToInterface aptr >>= return . castPtr

foreign import ccall "anyToInterface" cAnyToInterface
  :: Ptr AnyRef -> IO (Ptr ())

-- *Auxiliary Functions

withStringsArray :: [String] -> (Ptr CString -> IO a) -> IO a
withStringsArray l f = aux l []
  where aux [] cs = withArray (reverse cs) f
        aux (h:t) cs = withCString h (\ c -> aux t (c:cs))
