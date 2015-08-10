module Com.Sun.Star.Uno where

import UNO

import qualified Control.Exception as E
import Control.Monad
import Data.Text (Text)
import Foreign

import Com.Sun.Star.Lang

type    XInterfacePtr = Ptr ()
newtype XInterfaceRef = XInterfaceRef { unXInterfaceRef :: ForeignPtr () }

type    XComponentContextPtr = Ptr ()
newtype XComponentContextRef = XComponentContextRef { unXComponentContextRef :: ForeignPtr () }
