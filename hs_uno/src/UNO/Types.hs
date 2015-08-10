{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module UNO.Types where

import UNO.Text

import Data.Int
import Data.Text (Text, unpack)
import qualified Data.Text as T (append)
import Foreign
import Foreign.C

data CSequence a

-- *Is UNO Type

class IsUnoType a where
  getUnoTypeClass :: a -> TypeClass
  getUnoTypeName  :: a -> Text
  getUnoType      :: a -> IO TypeDescriptionPtr
  getUnoType v = getTypeDescription (getUnoTypeName v)

instance IsUnoType Int16 where
  getUnoTypeClass _ = Typelib_TypeClass_SHORT
  getUnoTypeName  _ = "short"

instance IsUnoType Word16 where
  getUnoTypeClass _ = Typelib_TypeClass_UNSIGNED_SHORT
  getUnoTypeName  _ = "unsigned short"

instance IsUnoType Int32 where
  getUnoTypeClass _ = Typelib_TypeClass_LONG
  getUnoTypeName  _ = "long"

instance IsUnoType Word32 where
  getUnoTypeClass _ = Typelib_TypeClass_UNSIGNED_LONG
  getUnoTypeName  _ = "unsigned long"

instance IsUnoType Int64 where
  getUnoTypeClass _ = Typelib_TypeClass_HYPER
  getUnoTypeName  _ = "hyper"

instance IsUnoType Word64 where
  getUnoTypeClass _ = Typelib_TypeClass_UNSIGNED_HYPER
  getUnoTypeName  _ = "unsigned hyper"

instance IsUnoType OUString where
  getUnoTypeClass _ = Typelib_TypeClass_STRING
  getUnoTypeName  _ = "string"

instance IsUnoType a => IsUnoType (CSequence a) where
  getUnoTypeClass _ = Typelib_TypeClass_SEQUENCE
  getUnoTypeName _ = "[]" `T.append` (getUnoTypeName (undefined :: a))

instance IsUnoType a => IsUnoType [a] where
  getUnoTypeClass _ = Typelib_TypeClass_SEQUENCE
  getUnoTypeName _ = "[]" `T.append` (getUnoTypeName (undefined :: a))

sequenceInnerType :: forall a . IsUnoType a => CSequence a -> IO TypeDescriptionPtr
sequenceInnerType _ = getUnoType (undefined :: a)

pSequenceInnerType :: forall a . IsUnoType a => Ptr (CSequence a) -> IO TypeDescriptionPtr
pSequenceInnerType _ = getUnoType (undefined :: a)

-- *Marshallable UNO Type

class MarshallableUnoType a b | a -> b, b -> a where
  fromUno :: a -> IO b
  toUno :: b -> IO a

instance MarshallableUnoType Int16 Int16 where
  fromUno = return
  toUno = return

instance MarshallableUnoType Word16 Word16 where
  fromUno = return
  toUno = return

instance MarshallableUnoType Int32 Int32 where
  fromUno = return
  toUno = return

instance MarshallableUnoType Word32 Word32 where
  fromUno = return
  toUno = return

instance MarshallableUnoType Int64 Int64 where
  fromUno = return
  toUno = return

instance MarshallableUnoType Word64 Word64 where
  fromUno = return
  toUno = return

instance MarshallableUnoType (Ptr OUString) Text where
  fromUno = hs_oustring_to_text
  toUno = hs_text_to_oustring

-- *TypeDescription

data TypeDescription

type TypeDescriptionPtr = ForeignPtr TypeDescription

getTypeDescription :: Text -> IO TypeDescriptionPtr
getTypeDescription name = do
  psName <- hs_text_to_oustring name
  ptr <- cGetTypeDescriptionByName psName
  c_delete_oustring psName
  newForeignPtr typelib_typedescription_release ptr
{-
getTypeDescription :: TypeClass -> Text -> IO TypeDescriptionPtr
getTypeDescription tc t = do
  ptr <- withCString (unpack t) $ \ ct -> do
    css_uno_Type_getDescription (fromEnum tc) ct
  newForeignPtr typelib_typedescription_release ptr
-}

getTypeSize :: TypeDescriptionPtr -> IO Int32
getTypeSize fpTD = withForeignPtr fpTD typelib_typedescription_getSize

{-
foreign import ccall "css_uno_Type_getDescription"
  css_uno_Type_getDescription :: Int -> CString -> IO (Ptr TypeDescription)
-}

foreign import ccall "&typelib_typedescription_release"
  typelib_typedescription_release :: FunPtr (Ptr TypeDescription -> IO ())

foreign import ccall "typelib_typedescription_getSize"
  typelib_typedescription_getSize :: Ptr TypeDescription -> IO Int32

foreign import ccall "hsuno_getTypeDescriptionByName"
  cGetTypeDescriptionByName :: Ptr OUString -> IO (Ptr TypeDescription)

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

-- *Type Description Reference

data TypeDescriptionReference
