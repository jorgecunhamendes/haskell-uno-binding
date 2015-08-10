module Com.Sun.Star.Bridge where

import UNO

import Control.Applicative ((<$>))
import qualified Control.Exception as E
import Control.Monad
import Data.Text (Text)
import Foreign

import Com.Sun.Star.Uno

type    XUnoUrlResolverPtr = Ptr ()
newtype XUnoUrlResolverRef = XUnoUrlResolverRef { unXUnoUrlResolverRef :: ForeignPtr () }
