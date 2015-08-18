{-# LANGUAGE OverloadedStrings #-}
module Main where

import Com.Sun.Star.Uno
import Com.Sun.Star.Util.TheMacroExpander
import Com.Sun.Star.Util.XMacroExpander
import UNO

import Data.Text (Text, unpack)
import Foreign

main :: IO ()
main = do
  xContext <- mkReference . castPtr =<< unoBootstrap :: IO (Reference XComponentContext)
  tME <- theMacroExpanderGet xContext
  outText <- expandMacros tME inText
  putStrLn (unpack outText)

inText :: Text
inText = "test: $UNO_TYPES :test"
