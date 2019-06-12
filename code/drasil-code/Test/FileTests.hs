module Test.FileTests (fileTests) where

import Language.Drasil.Code (PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), StateTypeSym(..), 
  StatementSym(..), ControlStatementSym(..), ValueSym(..), Selector(..),
  FunctionSym(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

fileTests :: (PackageSym repr) => repr (Package repr)
fileTests = packMods "FileTests" [fileDoc (buildModule "FileTests" [] [] [fileTestMethod] [])]

fileTestMethod :: (RenderSym repr) => repr (Method repr)
fileTestMethod = mainMethod "FileTests" (body [writeStory, block [readStory], 
  goodBye])

writeStory :: (RenderSym repr) => repr (Block repr)
writeStory = block [
  varDecDef "e" int (litInt 5),
  varDec "f" float,
  "f" &.= castObj (cast float int) (var "e"),
  varDec "fileToWrite" outfile,

  openFileW (var "fileToWrite") (litString "testText.txt"),
  printFile (var "fileToWrite") int (litInt 0),
  printFileLn (var "fileToWrite") int (litFloat 0.89),
  printFileStr (var "fileToWrite") "ello",
  printFileStrLn (var "fileToWrite") "bye",
  printFileStrLn (var "fileToWrite") "!!",
  closeFile (var "fileToWrite"),

  varDec "fileToRead" infile,
  openFileR (var "fileToRead") (litString "testText.txt"),
  varDec "fileLine" string,
  getFileInputLine (var "fileToRead") (var "fileLine"),
  discardFileLine (var "fileToRead"),
  listDec "fileContents" 0 (listType dynamic_ string)]

readStory :: (RenderSym repr) => repr (Statement repr)
readStory = getFileInputAll (var "fileToRead") (var "fileContents")

goodBye :: (RenderSym repr) => repr (Block repr)
goodBye = block [printLnList string (var "fileContents"), closeFile (var "fileToRead")]