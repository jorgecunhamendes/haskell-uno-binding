{-# LANGUAGE OverloadedStrings #-}
module UNO.Singleton where

import UNO.Any
import UNO.Binary
import UNO.Reference
import UNO.Text
import UNO.Types

import Data.Text (Text, append)
import Foreign

-- |Get a singleton from an XComponentContext.
--
-- 'a' should implement 'com.sun.star.uno.XComponentContext'.
unoGetSingletonFromContext :: IsUnoType b => Text -> Reference a -> IO (Reference b)
unoGetSingletonFromContext t rContext =
  withUString ("/singletons/" `append` t) $ \ sSingletonSpecifier ->
    withReference rContext $ \ pContext -> do
      withAny AVoid $ \ pAny -> do
        hsunoGetSingletonFromContext sSingletonSpecifier (castPtr pContext) pAny
        fromAnyIO =<< anyFromUno pAny
