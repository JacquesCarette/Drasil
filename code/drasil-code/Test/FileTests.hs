module Test.FileTests (fileTests) where

import Language.Drasil.Code (PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), StateTypeSym(..), 
  StatementSym(..), ControlStatementSym(..), ValueSym(..), Selector(..),
  FunctionSym(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

fileTests :: (PackageSym repr) => repr (Package repr)
fileTests = packMods "FileTests" [fileDoc (buildModule "FileTests" [] [fileTestMethod] [])]

fileTestMethod :: (RenderSym repr) => repr (Method repr)
fileTestMethod = mainMethod "FileTests" (body [writeStory, block [readStory], 
  goodBye])

writeStory :: (RenderSym repr) => repr (Block repr)
writeStory = block [
  varDecDef "e" int (litInt 5),
  varDec $ var "f" float,
  var "f" float &= castObj (cast float) (var "e" int),
  varDec $ var "fileToWrite" outfile,

  openFileW (var "fileToWrite" outfile) (litString "testText.txt"),
  printFile (var "fileToWrite" outfile) (litInt 0),
  printFileLn (var "fileToWrite" outfile) (litFloat 0.89),
  printFileStr (var "fileToWrite" outfile) "ello",
  printFileStrLn (var "fileToWrite" outfile) "bye",
  printFileStrLn (var "fileToWrite" outfile) "!!",
  closeFile (var "fileToWrite" outfile),

  varDec $ var "fileToRead" infile,
  openFileR (var "fileToRead" infile) (litString "testText.txt"),
  varDec $ var "fileLine" string,
  getFileInputLine (var "fileToRead" infile) (var "fileLine" string),
  discardFileLine (var "fileToRead" infile),
  listDec "fileContents" 0 (listType dynamic_ string)]

readStory :: (RenderSym repr) => repr (Statement repr)
readStory = getFileInputAll (var "fileToRead" infile) 
  (var "fileContents" (listType dynamic_ string))

goodBye :: (RenderSym repr) => repr (Block repr)
goodBye = block [printLn (var "fileContents" (listType dynamic_ string)), 
  closeFile (var "fileToRead" infile)]