module Test.FileTests (fileTests) where

import GOOL.Drasil (ProgramSym(..), FileSym(..),
  PermanenceSym(..), BodySym(..), BlockSym(..), TypeSym(..), 
  StatementSym(..), ControlStatementSym(..), VariableSym(..), ValueSym(..), 
MethodSym(..), ModuleSym(..), GS, MS)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

fileTests :: (ProgramSym repr) => GS (repr (Program repr))
fileTests = prog "FileTests" [fileDoc (buildModule "FileTests" [fileTestMethod] 
  [])]

fileTestMethod :: (ProgramSym repr) => MS (repr (Method repr))
fileTestMethod = mainFunction (body [writeStory, block [readStory], goodBye])

writeStory :: (ProgramSym repr) => MS (repr (Block repr))
writeStory = block [
  varDec $ var "fileToWrite" outfile,

  openFileW (var "fileToWrite" outfile) (litString "testText.txt"),
  printFile (valueOf $ var "fileToWrite" outfile) (litInt 0),
  printFileLn (valueOf $ var "fileToWrite" outfile) (litFloat 0.89),
  printFileStr (valueOf $ var "fileToWrite" outfile) "ello",
  printFileStrLn (valueOf $ var "fileToWrite" outfile) "bye",
  printFileStrLn (valueOf $ var "fileToWrite" outfile) "!!",
  closeFile (valueOf $ var "fileToWrite" outfile),

  varDec $ var "fileToRead" infile,
  openFileR (var "fileToRead" infile) (litString "testText.txt"),
  varDec $ var "fileLine" string,
  getFileInputLine (valueOf $ var "fileToRead" infile) (var "fileLine" string),
  discardFileLine (valueOf $ var "fileToRead" infile),
  listDec 0 (var "fileContents" (listType dynamic_ string))]

readStory :: (ProgramSym repr) => MS (repr (Statement repr))
readStory = getFileInputAll (valueOf $ var "fileToRead" infile) 
  (var "fileContents" (listType dynamic_ string))

goodBye :: (ProgramSym repr) => MS (repr (Block repr))
goodBye = block [
  printLn (valueOf $ var "fileContents" (listType dynamic_ string)), 
  closeFile (valueOf $ var "fileToRead" infile)]