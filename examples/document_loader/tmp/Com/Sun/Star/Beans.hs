module Com.Sun.Star.Beans where

import UNO

import qualified Control.Exception as E
import Control.Monad
import Data.Text (Text)
import Foreign

import Com.Sun.Star.Uno

-- ### BEGIN TEMP ###
import Temp -- FIXME remove this import when no more temporary code is used

data XPropertyChangeListenerRef
data XVetoableChangeListenerRef
-- ### END TEMP ###

-- *Property

data Property

data PropertyRef

foreign import ccall "hsuno_com_sun_star_beans_Property_get_Name" cProperty_get_Name
    :: Ptr Property -> IO (Ptr OUString)

foreign import ccall "hsuno_com_sun_star_beans_Property_set_Name" cProperty_set_Name
    :: Ptr Property -> Ptr OUString -> IO ()

foreign import ccall "hsuno_com_sun_star_beans_Property_get_Handle" cProperty_get_Handle
    :: Ptr Property -> IO Int32

foreign import ccall "hsuno_com_sun_star_beans_Property_set_Handle" cProperty_set_Handle
    :: Ptr Property -> Int32 -> IO ()

foreign import ccall "hsuno_com_sun_star_beans_Property_get_Type" cProperty_get_Type
    :: Ptr Property -> IO (Ptr CssUnoType)

foreign import ccall "hsuno_com_sun_star_beans_Property_set_Type" cProperty_set_Type
    :: Ptr Property -> Ptr CssUnoType -> IO ()

foreign import ccall "hsuno_com_sun_star_beans_Property_get_Attributes" cProperty_get_Attributes
    :: Ptr Property -> IO Int16

foreign import ccall "hsuno_com_sun_star_beans_Property_set_Attributes" cProperty_set_Attributes
    :: Ptr Property -> Int16 -> IO ()

-- *XPropertySet

instance XPropertySet (Ptr a)

class Service a => XPropertySet a where
    getPropertySetInfo :: a -> IO (Ptr Com.Sun.Star.Beans.XPropertySetInfoRef)
    getPropertySetInfo a = do
      let iface = getInterface a
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySet_getPropertySetInfo iface exceptionPtr
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    setPropertyValue :: a -> Text -> (Ptr AnyRef) -> IO ()
    setPropertyValue a aPropertyName aValue = do
      let iface = getInterface a
      saPropertyName <- hs_text_to_oustring aPropertyName
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySet_setPropertyValue iface exceptionPtr saPropertyName aValue
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    getPropertyValue :: a -> Text -> IO (Ptr AnyRef)
    getPropertyValue a propertyName = do
      let iface = getInterface a
      sPropertyName <- hs_text_to_oustring propertyName
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySet_getPropertyValue iface exceptionPtr sPropertyName
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    addPropertyChangeListener :: a -> Text -> (Ptr Com.Sun.Star.Beans.XPropertyChangeListenerRef) -> IO ()
    addPropertyChangeListener a aPropertyName xListener = do
      let iface = getInterface a
      saPropertyName <- hs_text_to_oustring aPropertyName
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySet_addPropertyChangeListener iface exceptionPtr saPropertyName xListener
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    removePropertyChangeListener :: a -> Text -> (Ptr Com.Sun.Star.Beans.XPropertyChangeListenerRef) -> IO ()
    removePropertyChangeListener a aPropertyName aListener = do
      let iface = getInterface a
      saPropertyName <- hs_text_to_oustring aPropertyName
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySet_removePropertyChangeListener iface exceptionPtr saPropertyName aListener
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    addVetoableChangeListener :: a -> Text -> (Ptr Com.Sun.Star.Beans.XVetoableChangeListenerRef) -> IO ()
    addVetoableChangeListener a propertyName aListener = do
      let iface = getInterface a
      sPropertyName <- hs_text_to_oustring propertyName
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySet_addVetoableChangeListener iface exceptionPtr sPropertyName aListener
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    removeVetoableChangeListener :: a -> Text -> (Ptr Com.Sun.Star.Beans.XVetoableChangeListenerRef) -> IO ()
    removeVetoableChangeListener a propertyName aListener = do
      let iface = getInterface a
      sPropertyName <- hs_text_to_oustring propertyName
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySet_removeVetoableChangeListener iface exceptionPtr sPropertyName aListener
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

foreign import ccall "hsuno_com_sun_star_beans_XPropertySet_getPropertySetInfo" cXPropertySet_getPropertySetInfo
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> IO (Ptr Com.Sun.Star.Beans.XPropertySetInfoRef)

foreign import ccall "hsuno_com_sun_star_beans_XPropertySet_setPropertyValue" cXPropertySet_setPropertyValue
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> Ptr AnyRef -> IO ()

foreign import ccall "hsuno_com_sun_star_beans_XPropertySet_getPropertyValue" cXPropertySet_getPropertyValue
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> IO (Ptr AnyRef)

foreign import ccall "hsuno_com_sun_star_beans_XPropertySet_addPropertyChangeListener" cXPropertySet_addPropertyChangeListener
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> Ptr Com.Sun.Star.Beans.XPropertyChangeListenerRef -> IO ()

foreign import ccall "hsuno_com_sun_star_beans_XPropertySet_removePropertyChangeListener" cXPropertySet_removePropertyChangeListener
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> Ptr Com.Sun.Star.Beans.XPropertyChangeListenerRef -> IO ()

foreign import ccall "hsuno_com_sun_star_beans_XPropertySet_addVetoableChangeListener" cXPropertySet_addVetoableChangeListener
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> Ptr Com.Sun.Star.Beans.XVetoableChangeListenerRef -> IO ()

foreign import ccall "hsuno_com_sun_star_beans_XPropertySet_removeVetoableChangeListener" cXPropertySet_removeVetoableChangeListener
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> Ptr Com.Sun.Star.Beans.XVetoableChangeListenerRef -> IO ()

-- *XPropertySetInfo

data XPropertySetInfoRef

instance Service XPropertySetInfoRef

instance XInterface XPropertySetInfoRef

class Service a => XPropertySetInfo a where
    getProperties :: a -> IO (Ptr (Sequence Com.Sun.Star.Beans.Property))
    getProperties a = do
      let iface = getInterface a
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySetInfo_getProperties iface exceptionPtr
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    getPropertyByName :: a -> Text -> IO (Ptr Com.Sun.Star.Beans.Property)
    getPropertyByName a aName = do
      let iface = getInterface a
      saName <- hs_text_to_oustring aName
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySetInfo_getPropertyByName iface exceptionPtr saName
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

    hasPropertyByName :: a -> Text -> IO Bool
    hasPropertyByName a name = do
      let iface = getInterface a
      sName <- hs_text_to_oustring name
      with nullPtr $ \ exceptionPtr -> do
        result <- cXPropertySetInfo_hasPropertyByName iface exceptionPtr sName
        aException <- peek exceptionPtr
        when (aException /= nullPtr) (error "exceptions not yet implemented")
        return result

foreign import ccall "hsuno_com_sun_star_beans_XPropertySetInfo_getProperties" cXPropertySetInfo_getProperties
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> IO (Ptr (Sequence Com.Sun.Star.Beans.Property))

foreign import ccall "hsuno_com_sun_star_beans_XPropertySetInfo_getPropertyByName" cXPropertySetInfo_getPropertyByName
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> IO (Ptr Com.Sun.Star.Beans.Property)

foreign import ccall "hsuno_com_sun_star_beans_XPropertySetInfo_hasPropertyByName" cXPropertySetInfo_hasPropertyByName
    :: Ptr UnoInterface -> Ptr (Ptr Any) -> Ptr OUString -> IO Bool
