module Test.FileTests (fileTests) where

import Language.Drasil.Code (PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), StateTypeSym(..), StatementSym(..), 
  ControlStatementSym(..), VariableSym(..), ValueSym(..), Selector(..),
  MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

fileTests :: (PackageSym repr) => repr (Package repr)
fileTests = packMods "FileTests" [fileDoc (buildModule "FileTests" [] [fileTestMethod] [])]

fileTestMethod :: (RenderSym repr) => repr (Method repr)
fileTestMethod = mainMethod "FileTests" (body [writeStory, block [readStory], 
  goodBye])

writeStory :: (RenderSym repr) => repr (Block repr)
writeStory = block [
  varDecDef (var "e" int) (litInt 5),
  varDec $ var "f" float,
  var "f" float &= cast float (varVal $ var "e" int),
  varDec $ var "fileToWrite" outfile,

  openFileW (var "fileToWrite" outfile) (litString "testText.txt"),
  printFile (varVal $ var "fileToWrite" outfile) (litInt 0),
  printFileLn (varVal $ var "fileToWrite" outfile) (litFloat 0.89),
  printFileStr (varVal $ var "fileToWrite" outfile) "ello",
  printFileStrLn (varVal $ var "fileToWrite" outfile) "bye",
  printFileStrLn (varVal $ var "fileToWrite" outfile) "!!",
  closeFile (varVal $ var "fileToWrite" outfile),

  varDec $ var "fileToRead" infile,
  openFileR (var "fileToRead" infile) (litString "testText.txt"),
  varDec $ var "fileLine" string,
  getFileInputLine (varVal $ var "fileToRead" infile) (var "fileLine" string),
  discardFileLine (varVal $ var "fileToRead" infile),
  listDec 0 (var "fileContents" (listType dynamic_ string))]

readStory :: (RenderSym repr) => repr (Statement repr)
readStory = getFileInputAll (varVal $ var "fileToRead" infile) 
  (var "fileContents" (listType dynamic_ string))

goodBye :: (RenderSym repr) => repr (Block repr)
goodBye = block [
  printLn (varVal $ var "fileContents" (listType dynamic_ string)), 
  closeFile (varVal$ var "fileToRead" infile)]