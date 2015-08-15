module UNO.Reference where

import Control.Applicative ((<$>))
import Foreign

import UNO.Types
import qualified UNO.Binary as UNO (queryInterface, cInterfaceReleasePtr)

data Reference a = Ref { unRef :: ForeignPtr a }

-- |Make a reference to a UNO interface.
mkReference :: IsUnoType a => Ptr a -> IO (Reference a)
mkReference ptr = Ref <$> newForeignPtr UNO.cInterfaceReleasePtr ptr

-- |Use the pointer of the UNO interface referenced.
withReference :: Reference a -> (Ptr a -> IO b) -> IO b
withReference rA = withForeignPtr (unRef rA)

-- |Make a reference to a new interface after querying the referenced interface.
queryInterface :: IsUnoType b => Reference a -> IO (Reference b)
queryInterface rA = Ref <$> UNO.queryInterface (unRef rA)
