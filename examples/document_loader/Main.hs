--
-- Running the example:
-- $ cabal run -- "file://$LO_INSTDIR/sdk/examples/cpp/DocumentLoader/test.odt" "-env:URE_MORE_TYPES=file://$LO_INSTDIR/program/types/offapi.rdb"
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import UNO

import Control.Applicative ((<$>))
import Foreign

import Com.Sun.Star.Uno
import Com.Sun.Star.Beans
import Com.Sun.Star.Bridge
import Com.Sun.Star.Frame
import Com.Sun.Star.Uno.XInterface
import Com.Sun.Star.Bridge.XUnoUrlResolver
import Com.Sun.Star.Bridge.UnoUrlResolver
import Com.Sun.Star.Beans.Property
import Com.Sun.Star.Beans.XPropertySet
import Com.Sun.Star.Beans.XPropertySetInfo
import Com.Sun.Star.Uno.XComponentContext
import Com.Sun.Star.Uno.DeploymentException
import Com.Sun.Star.Frame.Desktop
import Com.Sun.Star.Frame.XDesktop2
import Com.Sun.Star.Frame.XComponentLoader
import Com.Sun.Star.Lang.XMultiComponentFactory

import Data.Text (Text, unpack, pack)
import System.Environment

main :: IO ()
main = do
  oContext <- unoBootstrap
  oUnoUrlResolver <- unoUrlResolverCreate oContext
  xUnoUrlResolver <- queryXUnoUrlResolver oUnoUrlResolver
  xInterface <- resolve xUnoUrlResolver sConnectionString
  fpType <- getUnoType (undefined :: XPropertySetRef)
  xPropertySet <- withForeignPtr fpType $ \ pType -> do
    aXPropertySet <- queryInterface xInterface (castPtr pType)
    iface <- anyToInterface aXPropertySet False
    return $ XPropertySetRef iface
  aXComponentContext <- getPropertyValue xPropertySet "DefaultContext"
  xComponentContext <- XComponentContextRef <$> anyToInterface aXComponentContext True
  --Reference < XDesktop2 > xComponentLoader = Desktop::create(xComponentContext);
  withForeignPtr (unXComponentContextRef xComponentContext) $ \ oContext2 -> do
    oDesktop <- Com.Sun.Star.Frame.Desktop.desktopCreate (castPtr oContext2)
    -- xComponentLoader <- queryXComponentLoader oDesktop
    xDesktop2 <- queryXDesktop2 oDesktop
    fpType2 <- getUnoType (undefined :: XComponentLoaderRef)
    xComponentLoader <- withForeignPtr fpType2 $ \ pType -> do
      aXComponentLoader <- queryInterface (XInterfaceRef (unXDesktop2Ref xDesktop2)) (castPtr pType)
      iface <- anyToInterface aXComponentLoader False
      return $ XComponentLoaderRef iface
  -- Loads a component specified by an URL into the specified new or existing frame.
  -- OUString sAbsoluteDocUrl, sWorkingDir, sDocPathUrl, sArgDocUrl;
  --rtl_getAppCommandArg(0, &sArgDocUrl.pData);
  --osl_getProcessWorkingDir(&sWorkingDir.pData);
  --osl::FileBase::getFileURLFromSystemPath( sArgDocUrl, sDocPathUrl);
  --osl::FileBase::getAbsoluteFileURL( sWorkingDir, sDocPathUrl, sAbsoluteDocUrl);
  --
  --Reference< XComponent > xComponent = xComponentLoader->loadComponentFromURL( sAbsoluteDocUrl, OUString( "_blank" ), 0, Sequence < ::com::sun::star::beans::PropertyValue >() );
    args <- getArgs
    xComponent <- loadComponentFromURL xComponentLoader (pack $ args !! 0) "_blank" 0 nullPtr
  -- dispose the local service manager
  --Reference< XComponent >::query( xMultiComponentFactoryClient )->dispose();
    return ()

sConnectionString :: Text
sConnectionString = "uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager"
