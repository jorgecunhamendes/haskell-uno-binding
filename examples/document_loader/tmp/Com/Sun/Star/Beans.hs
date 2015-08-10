{-# LANGUAGE OverloadedStrings #-}
module Com.Sun.Star.Beans where

import UNO

import qualified Control.Exception as E
import Control.Monad
import Data.Text (Text)
import Foreign

import Com.Sun.Star.Uno

import Temp -- FIXME remove this import when no more temporary code is used

type    XPropertyChangeListenerPtr = (Ptr ())
newtype XPropertyChangeListenerRef = XPropertyChangeListenerRef { unXPropertyChangeListenerRef :: ForeignPtr () }
type    XVetoableChangeListenerPtr = (Ptr ())
newtype XVetoableChangeListenerRef = XVetoableChangeListenerRef { unXVetoableChangeListenerRef :: ForeignPtr () }
type    PropertyChangeEventPtr     = (Ptr ())
newtype PropertyChangeEventRef     = PropertyChangeEventRef     { unPropertyChangeEventRef     :: ForeignPtr () }
type    XPropertySetPtr            = (Ptr ())
newtype XPropertySetRef            = XPropertySetRef            { unXPropertySetRef            :: ForeignPtr () }
instance IsUnoType XPropertySetRef where
  getUnoTypeClass _ = Typelib_TypeClass_INTERFACE
  getUnoTypeName _ = "com.sun.star.beans.XPropertySet"
type    XPropertySetInfoPtr        = (Ptr ())
newtype XPropertySetInfoRef        = XPropertySetInfoRef        { unXPropertySetInfoRef        :: ForeignPtr () }
type    PropertyValuePtr           = (Ptr ())
newtype PropertyValueRef           = PropertyValueRef           { unPropertyValueRef           :: ForeignPtr () }

-- *Property

type    PropertyPtr = (Ptr ())
newtype PropertyRef = PropertyRef { unPropertyRef :: ForeignPtr () }
