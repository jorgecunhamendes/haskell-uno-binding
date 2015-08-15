module UNO.Text where

import           Data.Int
import           Data.Text (Text)
import qualified Data.Text.Foreign as T (fromPtr, useAsPtr)
import           Data.Word
import           Foreign.Ptr

data OUString

type OUStringPtr = Ptr OUString

hs_text_to_oustring :: Text -> IO (Ptr OUString)
hs_text_to_oustring text = T.useAsPtr text
  $ \ buf len -> c_oustring_new buf (fromIntegral len)

hs_oustring_to_text :: Ptr OUString -> IO Text
hs_oustring_to_text oustrPtr = do
  buf <- c_oustring_buffer oustrPtr
  len <- c_oustring_length oustrPtr
  T.fromPtr buf (fromIntegral len)

peekOUString :: Ptr OUString -> IO Text
peekOUString = hs_oustring_to_text

oustringToUString :: Ptr OUString -> IO (Ptr UString)
oustringToUString = c_oustringGetUString

ustringToOUString :: Ptr UString -> IO (Ptr OUString)
ustringToOUString = c_oustringFromUString

foreign import ccall "create_oustring" c_oustring_new
  :: Ptr Word16 -> Int32 -> IO (Ptr OUString)

-- void delete_oustring (OUString * str);
foreign import ccall "delete_oustring" c_delete_oustring
  :: Ptr OUString -> IO ()

foreign import ccall "oustring_buffer" c_oustring_buffer
  :: Ptr OUString -> IO (Ptr Word16)

foreign import ccall "oustring_length" c_oustring_length
  :: Ptr OUString -> IO Int32

-- rtl_uString * oustringGetUString (OUString * str);
foreign import ccall "oustringGetUString" c_oustringGetUString
  :: Ptr OUString -> IO (Ptr UString)

-- OUString * oustringFromUString (rtl_uString * ustr);
foreign import ccall "oustringFromUString" c_oustringFromUString
  :: Ptr UString -> IO (Ptr OUString)

-- *UString

data UString

uStringNew :: Text -> IO (Ptr UString)
uStringNew text = T.useAsPtr text
  $ \ buf len -> hsuno_uString_new buf (fromIntegral len)

uStringToText :: Ptr UString -> IO Text
uStringToText ustrPtr = do
  buf <- uStringGetStr ustrPtr
  len <- uStringGetLength ustrPtr
  T.fromPtr buf (fromIntegral len)

foreign import ccall unsafe "hsuno_uString_new" hsuno_uString_new
  :: Ptr Word16 -> Int32 -> IO (Ptr UString)

foreign import ccall unsafe "rtl_uString_getLength" uStringGetLength
  :: Ptr UString -> IO Int32

foreign import ccall unsafe "rtl_uString_getStr" uStringGetStr
  :: Ptr UString -> IO (Ptr Word16)
