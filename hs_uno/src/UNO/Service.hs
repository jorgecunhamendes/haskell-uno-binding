module UNO.Service where

import UNO.Binary

import Foreign

class Service a where
    getInterface :: a -> Ptr UnoInterface
