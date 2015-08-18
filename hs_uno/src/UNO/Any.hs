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
  | AChar   Char
  | ABool   Bool
  | AByte   Word8
  | AShort  Int16
  | AUShort Word16
  | ALong   Int32
  | AULong  Word32
  | AHyper  Int64
  | AUHyper Word64
  | AFloat  Float
  | ADouble Double
  | AString Text
  | AType   () -- TODO
  | AEnum   Int
  | AStruct () -- TODO
  | AInterface Text (ForeignPtr ())

-- *Convertion from and to UNO

anyFromUno :: Ptr B.Any -> IO Any
anyFromUno pAny = do
  t <- toEnum <$> B.anyGetTypeClass pAny
  case t of
    Typelib_TypeClass_VOID           -> return AVoid
    Typelib_TypeClass_CHAR           -> AChar   <$> anyValue pAny
    Typelib_TypeClass_BOOLEAN        -> ABool   <$> anyValue pAny
    Typelib_TypeClass_BYTE           -> AByte   <$> anyValue pAny
    Typelib_TypeClass_SHORT          -> AShort  <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_SHORT -> AUShort <$> anyValue pAny
    Typelib_TypeClass_LONG           -> ALong   <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_LONG  -> AULong  <$> anyValue pAny
    Typelib_TypeClass_HYPER          -> AHyper  <$> anyValue pAny
    Typelib_TypeClass_UNSIGNED_HYPER -> AUHyper <$> anyValue pAny
    Typelib_TypeClass_FLOAT          -> AFloat  <$> anyValue pAny
    Typelib_TypeClass_DOUBLE         -> ADouble <$> anyValue pAny
    Typelib_TypeClass_STRING         -> AString <$> (hs_oustring_to_text =<< anyValue pAny)
    Typelib_TypeClass_TYPE           -> error "[anyFromUno] not implemented (type)" -- TODO
    Typelib_TypeClass_ENUM           -> AEnum   <$> anyValue pAny
    Typelib_TypeClass_STRUCT         -> error "[anyFromUno] not implemented (struct)" -- TODO
    Typelib_TypeClass_EXCEPTION      -> error "[anyFromUno] not implemented (exception)" -- TODO
    Typelib_TypeClass_SEQUENCE       -> error "[anyFromUno] not implemented (sequence)" -- TODO
    Typelib_TypeClass_INTERFACE      -> do
      v <- anyValue pAny :: IO (Ptr a)
      tn <- uStringToText =<< B.anyGetTypeName pAny
      -- B.cInterfaceAcquire v
      AInterface tn <$> newForeignPtr B.cInterfaceReleasePtr v

anyToUno' :: Any -> Ptr B.Any -> IO ()
anyToUno' (AVoid) =
  \ pAny -> B.anyConstruct pAny nullPtr nullPtr B.cInterfaceAcquirePtr
anyToUno' (AChar   v) = createUNOAny v
anyToUno' (ABool   v) = createUNOAny v
anyToUno' (AByte   v) = createUNOAny v
anyToUno' (AShort  v) = createUNOAny v
anyToUno' (AUShort v) = createUNOAny v
anyToUno' (ALong   v) = createUNOAny v
anyToUno' (AULong  v) = createUNOAny v
anyToUno' (AHyper  v) = createUNOAny v
anyToUno' (AUHyper v) = createUNOAny v
anyToUno' (AFloat  v) = createUNOAny v
anyToUno' (ADouble v) = createUNOAny v
anyToUno' (AString v) = \ pAny -> withUString v
  (\ pV -> createUNOAnyWithPtr pV pAny)
-- TODO
--  - Type
--  - Struct
anyToUno' (AInterface t fp) = \ pAny -> do
  rType <- getTypeDescription t
  withForeignPtr rType $ \ pType ->
    withForeignPtr fp $ \ p ->
      B.anyConstruct pAny p pType B.cInterfaceAcquirePtr
anyToUno' _ = error "[anyToUno'] not yet implemented" -- TODO

createUNOAny :: (IsUnoType a, Storable a) => a -> Ptr B.Any -> IO ()
createUNOAny a pAny = with a $ \ pA -> createUNOAnyWithPtr pA pAny

createUNOAnyWithPtr :: forall a . IsUnoType a => Ptr a -> Ptr B.Any -> IO ()
createUNOAnyWithPtr pA pAny = do
  rType <- getUnoType (undefined :: a)
  withForeignPtr rType $ \ pType -> do
    B.anyConstruct pAny pA pType B.cInterfaceAcquirePtr

anyValue :: Storable a => Ptr B.Any -> IO a
anyValue pAny = B.anyGetValue pAny >>= peek

withAny :: Any -> (Ptr B.Any -> IO a) -> IO a
withAny any f = allocaBytes B.anyStructSize $ \ pAny -> do
  anyToUno' any pAny
  f pAny

-- *Insertion and extraction of values

class Anyable a where
  toAny :: a -> Any
  fromAny :: Any ->  a
  toAnyIO :: a -> IO Any
  toAnyIO = return . toAny
  fromAnyIO :: Any -> IO a
  fromAnyIO = return . fromAny

instance Anyable () where
  toAny () = AVoid
  fromAny AVoid = ()
  fromAny _ = error "cannot extract to VOID"

instance Anyable Char where
  toAny = AChar
  fromAny (AChar v) = v
  fromAny _ = error "cannot extract to CHAR"

instance Anyable Bool where
  toAny = ABool
  fromAny (ABool v) = v
  fromAny _ = error "cannot extract to BOOLEAN"

instance Anyable Word8 where
  toAny = AByte
  fromAny (AByte v) = v
  fromAny _ = error "cannot extract to BYTE"

instance Anyable Int16 where
  toAny = AShort
  fromAny (AShort v) = v
  fromAny _ = error "cannot extract to SHORT"

instance Anyable Word16 where
  toAny = AUShort
  fromAny (AUShort v) = v
  fromAny _ = error "cannot extract to UNSIGNED SHORT"

instance Anyable Int32 where
  toAny = ALong
  fromAny (ALong v) = v
  fromAny _ = error "cannot extract to LONG"

instance Anyable Word32 where
  toAny = AULong
  fromAny (AULong v) = v
  fromAny _ = error "cannot extract to UNSIGNED LONG"

instance Anyable Int64 where
  toAny = AHyper
  fromAny (AHyper v) = v
  fromAny _ = error "cannot extract to HYPER"

instance Anyable Word64 where
  toAny = AUHyper
  fromAny (AUHyper v) = v
  fromAny _ = error "cannot extract to UNSIGNED HYPER"

instance Anyable Float where
  toAny = AFloat
  fromAny (AFloat v) = v
  fromAny _ = error "cannot extract to FLOAT"

instance Anyable Double where
  toAny = ADouble
  fromAny (ADouble v) = v
  fromAny _ = error "cannot extract to DOUBLE"

-- TODO
--  - Text / String
--  - Type
--  - Struct

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
