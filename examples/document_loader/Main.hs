{-# LANGUAGE OverloadedStrings #-}
module Main where

import UNO.Binary

import Foreign

import Com.Sun.Star.Beans
import Com.Sun.Star.Bridge
import Com.Sun.Star.Uno

import Data.Text (Text, unpack)

main :: IO ()
main = do
  oContext <- unoBootstrap
  oUnoUrlResolver <- unoUrlResolverCreate oContext
  xInterface <- resolve oUnoUrlResolver sConnectionString :: IO (Ptr XInterfaceRef)
  let xPropertySet = castPtr xInterface
  aXComponentContext <- getPropertyValue xPropertySet "DefaultContext"
  xComponentContext <- cAnyToInterface aXComponentContext
  --Reference< XMultiComponentFactory > xMultiComponentFactoryServer(xComponentContext->getServiceManager() );
  --Reference < XDesktop2 > xComponentLoader = Desktop::create(xComponentContext);
  -- Loads a component specified by an URL into the specified new or existing frame.
  -- OUString sAbsoluteDocUrl, sWorkingDir, sDocPathUrl, sArgDocUrl;
  --rtl_getAppCommandArg(0, &sArgDocUrl.pData);
  --osl_getProcessWorkingDir(&sWorkingDir.pData);
  --osl::FileBase::getFileURLFromSystemPath( sArgDocUrl, sDocPathUrl);
  --osl::FileBase::getAbsoluteFileURL( sWorkingDir, sDocPathUrl, sAbsoluteDocUrl);
  --
  --Reference< XComponent > xComponent = xComponentLoader->loadComponentFromURL( sAbsoluteDocUrl, OUString( "_blank" ), 0, Sequence < ::com::sun::star::beans::PropertyValue >() );
  -- dispose the local service manager
  --Reference< XComponent >::query( xMultiComponentFactoryClient )->dispose();
  return ()

sConnectionString :: Text
sConnectionString = "uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager"
