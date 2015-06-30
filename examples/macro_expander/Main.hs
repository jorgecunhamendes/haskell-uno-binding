{-# LANGUAGE OverloadedStrings #-}
module Main where

import Com.Sun.Star.Util.TheMacroExpander
import Com.Sun.Star.Util.XMacroExpander
import UNO.Binary

import Data.Text (Text, unpack)

main :: IO ()
main = do
  unoBootstrap
  tME <- theMacroExpanderNew
  outText <- expandMacros tME inText
  putStrLn (unpack outText)

inText :: Text
inText = "test: $UNO_TYPES :test"
