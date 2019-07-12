module Test.FileTests (fileTests) where

import Language.Drasil.Code (PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), StateTypeSym(..), 
  StatementSym(..), ControlStatementSym(..), ValueSym(..), Selector(..),
  MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

fileTests :: (PackageSym repr) => repr (Package repr)
fileTests = packMods "FileTests" [fileDoc (buildModule "FileTests" [] [fileTestMethod] [])]

fileTestMethod :: (RenderSym repr) => repr (Method repr)
fileTestMethod = mainMethod "FileTests" (body [writeStory, block [readStory], 
  goodBye])

writeStory :: (RenderSym repr) => repr (Block repr)
writeStory = block [
  varDecDef (varVal "e" int) (litInt 5),
  varDec $ varVal "f" float,
  varVal "f" float &= cast float (varVal "e" int),
  varDec $ varVal "fileToWrite" outfile,

  openFileW (varVal "fileToWrite" outfile) (litString "testText.txt"),
  printFile (varVal "fileToWrite" outfile) (litInt 0),
  printFileLn (varVal "fileToWrite" outfile) (litFloat 0.89),
  printFileStr (varVal "fileToWrite" outfile) "ello",
  printFileStrLn (varVal "fileToWrite" outfile) "bye",
  printFileStrLn (varVal "fileToWrite" outfile) "!!",
  closeFile (varVal "fileToWrite" outfile),

  varDec $ varVal "fileToRead" infile,
  openFileR (varVal "fileToRead" infile) (litString "testText.txt"),
  varDec $ varVal "fileLine" string,
  getFileInputLine (varVal "fileToRead" infile) (varVal "fileLine" string),
  discardFileLine (varVal "fileToRead" infile),
  listDec 0 (varVal "fileContents" (listType dynamic_ string))]

readStory :: (RenderSym repr) => repr (Statement repr)
readStory = getFileInputAll (varVal "fileToRead" infile) 
  (varVal "fileContents" (listType dynamic_ string))

goodBye :: (RenderSym repr) => repr (Block repr)
goodBye = block [printLn (varVal "fileContents" (listType dynamic_ string)), 
  closeFile (varVal "fileToRead" infile)]