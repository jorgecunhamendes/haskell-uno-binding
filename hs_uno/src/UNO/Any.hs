{-# LANGUAGE ScopedTypeVariables #-}
module UNO.Any where

import qualified UNO.Binary as B
import UNO.Reference
import UNO.Text
import UNO.Types

import Control.Applicative ((<$>))
import Data.Text (Text)
import Foreign
import Prelude hiding (any)

data Any
  = AVoid
  | AChar
  | ABool Bool
  | AByte
  | AShort  Int16
  | AUShort Word16
  | ALong   Int32
  | AULong  Word32
  | AHyper  Int64
  | AUHyper Word64
  | AFloat  Float
  | ADouble Double
  | AString Text
  | AType
  | AEnum   Int
  | AStruct
  | AInterface Text (ForeignPtr ())

anyValue :: Storable a => Ptr B.Any -> IO a
anyValue pAny = B.anyGetValue pAny >>= peek

anyFromUno :: Ptr B.Any -> IO Any
anyFromUno pAny = do
  t <- toEnum <$> B.anyGetTypeClass pAny
  case t of
    Typelib_TypeClass_VOID           -> return AVoid
    Typelib_TypeClass_CHAR           -> error "not implemented" -- TODO
    Typelib_TypeClass_BOOLEAN        -> error "not implemented" -- TODO
    Typelib_TypeClass_BYTE           -> error "not implemented" -- TODO
    Typelib_TypeClass_SHORT          -> AShort  <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_SHORT -> AUShort <$> anyValue pAny
    Typelib_TypeClass_LONG           -> ALong   <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_LONG  -> AULong  <$> anyValue pAny
    Typelib_TypeClass_HYPER          -> AHyper  <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_HYPER -> AUHyper <$> anyValue pAny
    Typelib_TypeClass_FLOAT          -> AFloat  <$> anyValue pAny
    Typelib_TypeClass_DOUBLE         -> ADouble <$> anyValue pAny
    Typelib_TypeClass_STRING         -> AString <$> (hs_oustring_to_text =<< anyValue pAny)
    Typelib_TypeClass_TYPE           -> error "not implemented" -- TODO
    Typelib_TypeClass_ENUM           -> AEnum   <$> anyValue pAny
    Typelib_TypeClass_STRUCT         -> error "not implemented" -- TODO
    Typelib_TypeClass_EXCEPTION      -> error "not implemented" -- TODO
    Typelib_TypeClass_SEQUENCE       -> error "not implemented" -- TODO
    Typelib_TypeClass_INTERFACE      -> do
      v <- anyValue pAny :: IO (Ptr a)
      tn <- uStringToText =<< B.anyGetTypeName pAny
      -- B.cInterfaceAcquire v
      AInterface tn <$> newForeignPtr B.cInterfaceReleasePtr v

class Anyable a where
  toAny :: a -> Any
  fromAny :: Any ->  a
  toAnyIO :: a -> IO Any
  toAnyIO = return . toAny
  fromAnyIO :: Any -> IO a
  fromAnyIO = return . fromAny

instance IsUnoType a => Anyable (Reference a) where
    toAny (Ref i) = AInterface (getUnoTypeName (undefined :: a))
                               (castForeignPtr i)
    fromAny (AInterface tn0 i) = getCorrectInterface $ Ref (castForeignPtr i)
      where tn1 = getUnoTypeName (undefined :: a)
            getCorrectInterface = if tn0 == tn1
                                  then id
                                  else error "need to query for interface"
    fromAny _ = error "invalid type"
    fromAnyIO (AInterface tn0 i) = getCorrectInterface $ Ref (castForeignPtr i)
      where tn1 = getUnoTypeName (undefined :: a)
            getCorrectInterface = if tn0 == tn1 then return else queryInterface
    fromAnyIO _ = error "invalid type"

{-
anyToUno :: Any -> IO (Ptr UNOAny)
anyToUno pAny = do
  t <- toEnum <$> cAnyGetTypeClass pAny
  case t of
    Typelib_TypeClass_VOID           -> return AVoid
    Typelib_TypeClass_CHAR           -> error "not implemented" -- TODO
    Typelib_TypeClass_BOOLEAN        -> error "not implemented" -- TODO
    Typelib_TypeClass_BYTE           -> error "not implemented" -- TODO
    Typelib_TypeClass_SHORT          -> AShort  <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_SHORT -> AUShort <$> anyValue pAny
    Typelib_TypeClass_LONG           -> ALong   <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_LONG  -> AULong  <$> anyValue pAny
    Typelib_TypeClass_HYPER          -> AHyper  <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_HYPER -> AUHyper <$> anyValue pAny
    Typelib_TypeClass_FLOAT          -> AFloat  <$> anyValue pAny
    Typelib_TypeClass_DOUBLE         -> ADouble <$> anyValue pAny
    Typelib_TypeClass_STRING         -> AString <$> (hs_oustring_to_text =<< anyValue pAny)
    Typelib_TypeClass_TYPE           -> error "not implemented" -- TODO
    Typelib_TypeClass_ENUM           -> error "not implemented" -- TODO
    Typelib_TypeClass_STRUCT         -> error "not implemented" -- TODO
    Typelib_TypeClass_EXCEPTION      -> error "not implemented" -- TODO
    Typelib_TypeClass_SEQUENCE       -> error "not implemented" -- TODO
    Typelib_TypeClass_INTERFACE      -> do
      v <- anyValue pAny :: IO (Ptr a)
      tn <- hs_oustring_to_text =<< cAnyGetTypeName pAny
      cInterfaceAcquire v
      AInterface tn <$> newForeignPtr cInterfaceReleasePtr v
    _ -> error "type not supported"
-}

anyToUno' :: Any -> Ptr B.Any -> IO ()
anyToUno' any = error "not yet implemented" -- TODO

withAny :: Any -> (Ptr B.Any -> IO a) -> IO a
withAny any f = allocaBytes B.anyStructSize $ \ pAny -> do
  anyToUno' any pAny
  f pAny
