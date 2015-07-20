module UNO.Types where

import Data.Text (Text, unpack)
import Foreign
import Foreign.C

data Any

data TypeDescription

type TypeDescriptionPtr = ForeignPtr TypeDescription

getTypeDescription :: TypeClass -> Text -> IO TypeDescriptionPtr
getTypeDescription tc t = do
  ptr <- withCString (unpack t) $ \ ct -> do
    css_uno_Type_getDescription (fromEnum tc) ct
  newForeignPtr typelib_typedescription_release ptr

foreign import ccall "css_uno_Type_getDescription"
  css_uno_Type_getDescription :: Int -> CString -> IO (Ptr TypeDescription)

foreign import ccall "&typelib_typedescription_release"
  typelib_typedescription_release :: FunPtr (Ptr TypeDescription -> IO ())

-- |This type class enum is binary compatible with the IDL enum com.sun.star.uno.TypeClass.
--
-- (from 'include/typelib/typeclass.h': typedef enum _typelib_TypeClass)
data TypeClass
  -- | type class of void
  = Typelib_TypeClass_VOID
  -- | type class of char
  | Typelib_TypeClass_CHAR
  -- | type class of boolean
  | Typelib_TypeClass_BOOLEAN
  -- | type class of byte
  | Typelib_TypeClass_BYTE
  -- | type class of short
  | Typelib_TypeClass_SHORT
  -- | type class of unsigned short
  | Typelib_TypeClass_UNSIGNED_SHORT
  -- | type class of long
  | Typelib_TypeClass_LONG
  -- | type class of unsigned long
  | Typelib_TypeClass_UNSIGNED_LONG
  -- | type class of hyper
  | Typelib_TypeClass_HYPER
  -- | type class of unsigned hyper
  | Typelib_TypeClass_UNSIGNED_HYPER
  -- | type class of float
  | Typelib_TypeClass_FLOAT
  -- | type class of double
  | Typelib_TypeClass_DOUBLE
  -- | type class of string
  | Typelib_TypeClass_STRING
  -- | type class of type
  | Typelib_TypeClass_TYPE
  -- | type class of any
  | Typelib_TypeClass_ANY
  -- | type class of enum
  | Typelib_TypeClass_ENUM
  -- | type class of typedef
  | Typelib_TypeClass_TYPEDEF
  -- | type class of struct
  | Typelib_TypeClass_STRUCT
  -- | Deprecated, UNOIDL does not have a union concept.
  | Typelib_TypeClass_UNION
  -- | type class of exception
  | Typelib_TypeClass_EXCEPTION
  -- | type class of sequence
  | Typelib_TypeClass_SEQUENCE
  -- | Deprecated, UNOIDL does not have an array concept.
  | Typelib_TypeClass_ARRAY
  -- | type class of interface
  | Typelib_TypeClass_INTERFACE
  -- | type class of service (not implemented)
  | Typelib_TypeClass_SERVICE
  -- | type class of module (not implemented)
  | Typelib_TypeClass_MODULE
  -- | type class of interface method
  | Typelib_TypeClass_INTERFACE_METHOD
  -- | type class of interface attribute
  | Typelib_TypeClass_INTERFACE_ATTRIBUTE
  -- | type class of unknown type
  | Typelib_TypeClass_UNKNOWN
  -- | type class of properties
  | Typelib_TypeClass_PROPERTY
  -- | type class of constants
  | Typelib_TypeClass_CONSTANT
  -- | type class of constants groups
  | Typelib_TypeClass_CONSTANTS
  -- | type class of singletons
  | Typelib_TypeClass_SINGLETON
  deriving (Eq, Ord, Enum, Show)
