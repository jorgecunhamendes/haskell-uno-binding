{-# LANGUAGE OverloadedStrings #-}
module Com.Sun.Star.Frame where

import Foreign
import UNO

type    XDesktop2Ptr = Ptr ()
newtype XDesktop2Ref = XDesktop2Ref { unXDesktop2Ref :: ForeignPtr () }

type    XComponentLoaderPtr = Ptr ()
newtype XComponentLoaderRef = XComponentLoaderRef { unXComponentLoaderRef :: ForeignPtr () }

instance IsUnoType XComponentLoaderRef where
  getUnoTypeClass _ = Typelib_TypeClass_INTERFACE
  getUnoTypeName _ = "com.sun.star.frame.XComponentLoader"
