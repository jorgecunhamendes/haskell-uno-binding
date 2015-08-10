module Com.Sun.Star.Lang where

import UNO

import Control.Applicative ((<$>))
import Data.Int
import Data.Word
import Foreign


type    XComponentPtr = Ptr ()
newtype XComponentRef = XComponentRef { unXComponentRef :: ForeignPtr () }

type    XMultiComponentFactoryPtr = Ptr ()
newtype XMultiComponentFactoryRef = XMultiComponentFactoryRef { unXMultiComponentFactoryRef :: ForeignPtr () }
