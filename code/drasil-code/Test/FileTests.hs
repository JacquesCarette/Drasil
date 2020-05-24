module Test.FileTests (fileTests) where

import GOOL.Drasil (GSProgram, MSBlock, MSStatement, SMethod, OOProg, 
  ProgramSym(..), FileSym(..), BodySym(..), BlockSym(..), TypeSym(..), 
  DeclStatement(..), IOStatement(..), VariableSym(..), Literal(..), 
  VariableValue(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

fileTests :: (OOProg r) => GSProgram r
fileTests = prog "FileTests" [fileDoc (buildModule "FileTests" [] 
  [fileTestMethod] [])]

fileTestMethod :: (OOProg r) => SMethod r
fileTestMethod = mainFunction (body [writeStory, block [readStory], goodBye])

writeStory :: (OOProg r) => MSBlock r
writeStory = block [
  varDec $ var "fileToWrite" outfile,

  openFileW (var "fileToWrite" outfile) (litString "testText.txt"),
  printFile (valueOf $ var "fileToWrite" outfile) (litInt 0),
  printFileLn (valueOf $ var "fileToWrite" outfile) (litDouble 0.89),
  printFileStr (valueOf $ var "fileToWrite" outfile) "ello",
  printFileStrLn (valueOf $ var "fileToWrite" outfile) "bye",
  printFileStrLn (valueOf $ var "fileToWrite" outfile) "!!",
  closeFile (valueOf $ var "fileToWrite" outfile),

  varDec $ var "fileToRead" infile,
  openFileR (var "fileToRead" infile) (litString "testText.txt"),
  varDec $ var "fileLine" string,
  getFileInputLine (valueOf $ var "fileToRead" infile) (var "fileLine" string),
  discardFileLine (valueOf $ var "fileToRead" infile),
  listDec 0 (var "fileContents" (listType string))]

readStory :: (OOProg r) => MSStatement r
readStory = getFileInputAll (valueOf $ var "fileToRead" infile) 
  (var "fileContents" (listType string))

goodBye :: (OOProg r) => MSBlock r
goodBye = block [
  printLn (valueOf $ var "fileContents" (listType string)), 
  closeFile (valueOf $ var "fileToRead" infile)]