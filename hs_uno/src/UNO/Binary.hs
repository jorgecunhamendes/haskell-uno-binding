{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module UNO.Binary where

import UNO.Types
import UNO.Text

import Control.Applicative ((<$>))
import Foreign
import Foreign.C
import System.Environment

data UnoInterface

data Context

type ContextPtr = Ptr Context

data CssUnoType

type CssUnoTypePtr = Ptr CssUnoType

unoBootstrap :: IO ContextPtr
unoBootstrap = do
  progname <- getExecutablePath
  args <- getArgs
  withStringsArray (progname : args) $ \ argsPtr -> do
    cUnoBootstrap (1 + length args) argsPtr

foreign import ccall "bootstrap" cUnoBootstrap
  :: Int -> Ptr CString -> IO ContextPtr

-- *Any

data UNOAny

-- type AnyPtr = (Ptr ())
newtype AnyRef = AnyRef { unAnyRef :: ForeignPtr () }

anyToInterface :: AnyRef -> Bool -> IO (ForeignPtr a)
anyToInterface fpAny b = withForeignPtr (unAnyRef fpAny) $ \ pAny -> do
  cAnyToInterface (castPtr pAny) b >>= newForeignPtr cInterfaceReleasePtr

anyRelease :: AnyPtr -> IO ()
anyRelease ptr = anyDestruct ptr cInterfaceReleasePtr

foreign import ccall "anyToInterface" cAnyToInterface
  :: AnyPtr -> Bool -> IO (Ptr a)

--foreign import ccall "anyToInterface" cAnyToInterface
--  :: AnyPtr -> IO (Ptr a)

-- TODO check the functions above

data Any

type AnyPtr = Ptr Any

foreign import ccall unsafe "hsuno_any_structSize" anyStructSize
  :: Int

foreign import ccall "hsuno_any_getTypeClass" anyGetTypeClass
  :: Ptr Any -> IO Int

foreign import ccall "hsuno_any_getTypeName" anyGetTypeName
  :: Ptr Any -> IO (Ptr UString)

foreign import ccall "hsuno_any_getValue" anyGetValue
  :: Ptr Any -> IO (Ptr a)

foreign import ccall "hsuno_any_destruct" anyDestruct
  :: Ptr Any -> FunPtr (Ptr a -> IO ()) -> IO ()

-- *Sequence

fromSequence
    :: (IsUnoType a, MarshallableUnoType (Ptr a) b)
    => ForeignPtr (CSequence a) -> IO [b]
fromSequence fpSequence = withForeignPtr fpSequence $ \ pSequence -> do
  len <- fromIntegral <$> cUnoSequenceGetLength pSequence
  arr <- cUnoSequenceGetArray pSequence
  elemType <- pSequenceInnerType pSequence
  elemSize <- fromIntegral <$> getTypeSize elemType
  let seqElem = \ i -> arr `plusPtr` (i * elemSize)
  mapM (fromUno . seqElem) [0 .. len-1]

toSequence :: IsUnoType a => [a] -> IO (ForeignPtr (CSequence a))
toSequence = error "toSequence not implemented" -- TODO

sequenceRelease :: forall a . IsUnoType a => Ptr (CSequence a) -> IO ()
sequenceRelease pSequence = do
  fpType <- getUnoType (undefined :: CSequence a)
  withForeignPtr fpType $ \ pType -> do
    cUnoSequenceRelease pType pSequence

type SequenceRelease a = Ptr (CSequence a) -> IO ()
foreign import ccall "wrapper"
  mkSequenceRelease :: SequenceRelease a -> IO (FunPtr (SequenceRelease a))

foreign import ccall "unoSequenceGetLength" cUnoSequenceGetLength
  :: Ptr (CSequence a) -> IO Int32

foreign import ccall "unoSequenceGetArray" cUnoSequenceGetArray
  :: Ptr (CSequence a) -> IO (Ptr a)

foreign import ccall "unoSequenceRelease" cUnoSequenceRelease
  :: Ptr TypeDescription -> Ptr (CSequence a) -> IO ()

foreign import ccall "&unoSequenceRelease" cUnoSequenceReleaseFinalizer
  :: FunPtr (Ptr TypeDescription -> Ptr (CSequence a) -> IO ())

-- *Interface

queryInterface :: forall a b . IsUnoType b => ForeignPtr a -> IO (ForeignPtr b)
queryInterface fpInterface = withForeignPtr fpInterface $ \ pInterface -> do
  fpType <- getUnoType (undefined :: b)
  withForeignPtr fpType $ \ pType -> do
    pIface <- cHsunoQueryInterface pInterface pType
    newForeignPtr cInterfaceReleasePtr pIface

foreign import ccall "hsunoQueryInterface" cHsunoQueryInterface
  :: Ptr a -> Ptr b -> IO (Ptr c)

foreign import ccall "cpp_acquire" cInterfaceAcquire
  :: Ptr a -> IO ()

foreign import ccall "&cpp_acquire" cInterfaceAcquirePtr
  :: FunPtr (Ptr a -> IO ())

foreign import ccall "cpp_release" cInterfaceRelease
  :: Ptr a -> IO ()

foreign import ccall "&cpp_release" cInterfaceReleasePtr
  :: FunPtr (Ptr a -> IO ())

-- *Auxiliary Functions

withStringsArray :: [String] -> (Ptr CString -> IO a) -> IO a
withStringsArray l f = aux l []
  where aux [] cs = withArray (reverse cs) f
        aux (h:t) cs = withCString h (\ c -> aux t (c:cs))
