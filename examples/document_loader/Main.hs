{-# LANGUAGE OverloadedStrings #-}
module Main where

import UNO.Binary

import Control.Applicative ((<$>))
import Foreign

import Com.Sun.Star.Uno
import Com.Sun.Star.Beans
import Com.Sun.Star.Bridge
import Com.Sun.Star.Uno.XInterface
import Com.Sun.Star.Bridge.XUnoUrlResolver
import Com.Sun.Star.Bridge.UnoUrlResolver
import Com.Sun.Star.Beans.Property
import Com.Sun.Star.Beans.XPropertySet
import Com.Sun.Star.Beans.XPropertySetInfo
import Com.Sun.Star.Uno.XComponentContext
import Com.Sun.Star.Uno.DeploymentException
import Com.Sun.Star.Frame.Desktop
import Com.Sun.Star.Frame.XComponentLoader
import Com.Sun.Star.Lang.XMultiComponentFactory

import Data.Text (Text, unpack, pack)
import System.Environment

main :: IO ()
main = do
  oContext <- unoBootstrap
  oUnoUrlResolver <- unoUrlResolverCreate oContext
  xInterface <- resolve oUnoUrlResolver sConnectionString :: IO (Ptr XInterfaceRef)
  let xPropertySet = castPtr xInterface :: Ptr XPropertySetRef
  aXComponentContext <- getPropertyValue xPropertySet "DefaultContext"
  xComponentContext <- castPtr <$> cAnyToInterface aXComponentContext :: IO (Ptr XComponentContextRef)
  -- xMultiComponentFactoryServer <- getServiceManager xComponentContext
  --Reference < XDesktop2 > xComponentLoader = Desktop::create(xComponentContext);
  xComponentLoader <- Com.Sun.Star.Frame.Desktop.desktopCreate (castPtr xComponentContext)
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
