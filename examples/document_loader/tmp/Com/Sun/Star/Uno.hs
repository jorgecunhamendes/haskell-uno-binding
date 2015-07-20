module Com.Sun.Star.Uno where

import UNO

import qualified Control.Exception as E
import Control.Monad
import Data.Text (Text)
import Foreign

-- *XInterface

foreign import ccall "hsuno_com_sun_star_uno_XInterface_queryInterface" cqueryInterface
     :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr any -> IO (Ptr AnyRef)
foreign import ccall "hsuno_com_sun_star_uno_XInterface_acquire" cacquire
     :: Ptr UnoInterface -> Ptr (Ptr Any) -> IO ()
foreign import ccall "hsuno_com_sun_star_uno_XInterface_release" crelease
     :: Ptr UnoInterface -> Ptr (Ptr Any) -> IO ()

data XInterfaceRef

instance Service XInterfaceRef

instance XInterface XInterfaceRef

class Service a => XInterface a where
    queryInterface :: a -> TypeDescriptionPtr -> IO Any
    queryInterface a aType = do
        let iface = getInterface a
        with nullPtr $ \ exceptionPtr -> do
            withForeignPtr aType $ \ aTypePtr -> do
                result <- cqueryInterface iface exceptionPtr aTypePtr
                aException <- peek exceptionPtr
                when (aException /= nullPtr) (error "exceptions not yet implemented")
                -- error "non-primitive types are not yet supported"
                return undefined
    acquire :: a -> IO ()
    acquire a = do
        let iface = getInterface a
        with nullPtr $ \ exceptionPtr -> do
            result <- cacquire iface exceptionPtr
            aException <- peek exceptionPtr
            when (aException /= nullPtr) (error "exceptions not yet implemented")
            return ()
    release :: a -> IO ()
    release a = do
        let iface = getInterface a
        with nullPtr $ \ exceptionPtr -> do
            result <- crelease iface exceptionPtr
            aException <- peek exceptionPtr
            when (aException /= nullPtr) (error "exceptions not yet implemented")
            return ()
