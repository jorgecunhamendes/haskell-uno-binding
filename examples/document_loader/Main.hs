--
-- Running the example:
-- $ ../startlo.sh
-- $ cabal run -- "file://$LO_INSTDIR/sdk/examples/cpp/DocumentLoader/test.odt" "-env:URE_MORE_TYPES=file://$LO_INSTDIR/program/types/offapi.rdb"
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import UNO

import Foreign

import Com.Sun.Star.Bridge.XUnoUrlResolver
import Com.Sun.Star.Bridge.UnoUrlResolver
import Com.Sun.Star.Beans.XPropertySet
import Com.Sun.Star.Frame.Desktop
import Com.Sun.Star.Frame.XComponentLoader

import Data.Text (Text, pack)
import System.Environment

main :: IO ()
main = do
  xContext <- mkReference . castPtr =<< unoBootstrap
  oUnoUrlResolver <- unoUrlResolverCreate xContext
  xUnoUrlResolver <- queryInterface oUnoUrlResolver
  xInterface <- resolve xUnoUrlResolver sConnectionString
  xPropertySet <- queryInterface xInterface
  aXComponentContext <- getPropertyValue xPropertySet "DefaultContext"
  xComponentContext <- fromAnyIO aXComponentContext
  --Reference < XDesktop2 > xComponentLoader = Desktop::create(xComponentContext);
  xDesktop2 <- Com.Sun.Star.Frame.Desktop.desktopCreate xComponentContext
  xComponentLoader <- queryInterface xDesktop2
  -- Loads a component specified by an URL into the specified new or existing frame.
  -- OUString sAbsoluteDocUrl, sWorkingDir, sDocPathUrl, sArgDocUrl;
  --rtl_getAppCommandArg(0, &sArgDocUrl.pData);
  --osl_getProcessWorkingDir(&sWorkingDir.pData);
  --osl::FileBase::getFileURLFromSystemPath( sArgDocUrl, sDocPathUrl);
  --osl::FileBase::getAbsoluteFileURL( sWorkingDir, sDocPathUrl, sAbsoluteDocUrl);
  --
  args <- getArgs
  xComponent <- loadComponentFromURL xComponentLoader (pack $ args !! 0) "_blank" 0 nullPtr
  -- dispose the local service manager
  --Reference< XComponent >::query( xMultiComponentFactoryClient )->dispose();
  return ()

sConnectionString :: Text
sConnectionString = "uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager"
