module Com.Sun.Star.Bridge where

import UNO

import Control.Applicative ((<$>))
import qualified Control.Exception as E
import Control.Monad
import Data.Text (Text)
import Foreign

import Com.Sun.Star.Uno

-- *UnoUrlResolver

data UnoUrlResolver = UnoUrlResolver (Ptr UnoInterface)

instance Service UnoUrlResolver where
    getInterface (UnoUrlResolver ptr) = ptr

instance XUnoUrlResolver UnoUrlResolver where

unoUrlResolverCreate :: Ptr Context -> IO UnoUrlResolver
unoUrlResolverCreate ptr = UnoUrlResolver <$> cUnoUrlResolver_create ptr

foreign import ccall "hsuno_com_sun_star_bridge_UnoUrlResolver_create" cUnoUrlResolver_create
    :: Ptr Context -> IO (Ptr UnoInterface)

-- *XUnoUrlResolver

foreign import ccall "hsuno_com_sun_star_bridge_XUnoUrlResolver_resolve" cresolve
     :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> IO (Ptr XInterfaceRef)

class Service a => XUnoUrlResolver a where
    resolve :: (Com.Sun.Star.Uno.XInterface b) => a -> Text -> IO (Ptr b)
    resolve a sUnoUrl = do
        let iface = getInterface a
        putStrLn "XUnoUrlResolver.resolve 0"
        ssUnoUrl <- hs_text_to_oustring sUnoUrl
        putStrLn "XUnoUrlResolver.resolve 1"
        with nullPtr $ \ exceptionPtr -> do
            putStrLn "XUnoUrlResolver.resolve 2"
            result <- cresolve iface exceptionPtr ssUnoUrl
            putStrLn "XUnoUrlResolver.resolve 3"
            aException <- peek exceptionPtr
            putStrLn "XUnoUrlResolver.resolve 4"
            when (aException /= nullPtr) (error "exceptions not yet implemented")
            putStrLn "XUnoUrlResolver.resolve 5"
            return (castPtr result)
