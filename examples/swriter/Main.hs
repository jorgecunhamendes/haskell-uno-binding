--
-- Running the example:
-- $ ../startlo.sh
-- $ cabal run -- "-env:URE_MORE_TYPES=file://$LO_INSTDIR/program/types/offapi.rdb"
--
module Main where

import UNO

import Data.Text (Text)
import Foreign

import Com.Sun.Star.Uno
import Com.Sun.Star.Uno.XComponentContext
import Com.Sun.Star.Bridge.XUnoUrlResolver
import Com.Sun.Star.Bridge.UnoUrlResolver
import Com.Sun.Star.Beans.XPropertySet
import Com.Sun.Star.Container.XIndexAccess
import Com.Sun.Star.Frame.Desktop
import Com.Sun.Star.Frame.XComponentLoader
import Com.Sun.Star.Lang.XMultiServiceFactory
-- import Com.Sun.Star.Table.XCell
import Com.Sun.Star.Text
import Com.Sun.Star.Text.XSimpleText
import Com.Sun.Star.Text.XText
import Com.Sun.Star.Text.XTextCursor
import Com.Sun.Star.Text.XTextDocument
import Com.Sun.Star.Text.XTextRange (getEnd, setString)
import Com.Sun.Star.Text.XTextTable

main :: IO ()
main = do
  putStrLn "main 0"
  xLocalContext <- mkReference . castPtr =<< unoBootstrap
  putStrLn "main 1"
  xContext <- connect xLocalContext
  putStrLn "main 2"
  xTextDocument <- openWriter xContext
  putStrLn "main 3"
  insertText xTextDocument
  putStrLn "main 4"
  insertTable xTextDocument
  putStrLn "the end"
  return ()

sConnectionString :: Text
sConnectionString = "uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager"

connect :: Reference XComponentContext -> IO (Reference XComponentContext)
connect xLocalContext = do
  oUnoUrlResolver    <- unoUrlResolverCreate xLocalContext
  xUnoUrlResolver    <- queryInterface oUnoUrlResolver
  xInterface         <- resolve xUnoUrlResolver sConnectionString
  xPropertySet       <- queryInterface xInterface
  aXComponentContext <- getPropertyValue xPropertySet "DefaultContext"
  xComponentContext  <- fromAnyIO aXComponentContext
  return xComponentContext

openWriter :: Reference XComponentContext -> IO (Reference XTextDocument)
openWriter xContext = do
  xMultiComponentFactory <- getServiceManager xContext
  xComponentLoader <- queryInterface =<< desktopCreate xContext
  xComponent <- loadComponentFromURL xComponentLoader "private:factory/swriter"
                                     "_blank" 0 nullPtr
  queryInterface xComponent

insertText :: Reference XTextDocument -> IO ()
insertText xTextDocument = do
  xText <- queryInterface =<< getText xTextDocument
  xTextCursor <- queryInterface =<< createTextCursor xText
  insertString xText xTextCursor "First line of the newly created text document.\n" False
  insertString xText xTextCursor "Second line of the newly created text document.\n" False

insertTable :: Reference XTextDocument -> IO ()
insertTable xTextDocument = do
  xText <- getText xTextDocument
  xSimpleText <- queryInterface xText
  xTextCursor <- queryInterface =<< createTextCursor xSimpleText
  xMultiServiceFactory <- queryInterface xTextDocument
  xTextTable <- queryInterface =<<
      createInstance xMultiServiceFactory "com.sun.star.text.TextTable"
  initialize xTextTable 4 4
  xTextContent <- queryInterface xTextTable
  xTextRange <- getEnd xTextCursor
  insertTextContent xText xTextRange xTextContent False
  -- get XPropertySet interfaces for the first row and the table
  rRowProperties <- fromAnyIO =<< (`getByIndex` 0) =<< queryInterface =<< getRows xTextTable
  rTableProperties <- queryInterface xTextTable
  -- set the back color
  setPropertyValue rTableProperties "BackTransparent" (toAny False)
  setPropertyValue rTableProperties "BackColor" (toAny (13421823 :: Int32))
  setPropertyValue rRowProperties "BackTransparent" (toAny False)
  setPropertyValue rRowProperties "BackColor" (toAny (6710932 :: Int32))
  -- insert table header
  insertIntoCell xTextTable "A1" "First Column"
  insertIntoCell xTextTable "B1" "Second Column"
  insertIntoCell xTextTable "C1" "Third Column"
  insertIntoCell xTextTable "D1" "SUM"
  -- insert first row
  --(`setValue` 12.3)            =<< getCellByName "A2" xTextTable
  --(`setValue` 4567.8)          =<< getCellByName "B2" xTextTable
  --(`setValue` (negate 910.11)) =<< getCellByName "C2" xTextTable
  --(`setFormula` "sum <A2:C2>") =<< getCellByName "D2" xTextTable
  -- insert second row
  -- TODO
  -- insert third row
  -- TODO

--
-- ****************************************************************************
--

insertIntoCell xTextTable sCellName sText = do
  xText <- queryInterface =<< getCellByName xTextTable sCellName
  xPropertySet <- queryInterface =<< createTextCursor xText
  setPropertyValue xPropertySet "CharColor" (toAny (16777215 :: Int32))
  xTextRange <- queryInterface xText
  setString xTextRange sText
