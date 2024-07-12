-- | Simple GOOL test program for making sure generated programs can read
-- and write to files. See stable/gooltest for more details on what is generated through this.
module FileTests (fileTests) where

import GOOL.Drasil (GSProgram, MSBlock, MSStatement, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), BlockSym(..), TypeSym(..),
  DeclStatement(..), IOStatement(..), var, ScopeSym(..),
  Literal(..), VariableValue(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return, print, log, exp, sin, cos, tan)

-- | Creates a program in GOOL to test reading and writing to files.
fileTests :: (OOProg r) => GSProgram r
fileTests = prog "FileTests" "" [fileDoc (buildModule "FileTests" []
  [fileTestMethod] [])]

-- | File test method starts with 'writeStory' and ends with 'goodBye'.
fileTestMethod :: (OOProg r) => SMethod r
fileTestMethod = mainFunction (body [writeStory, block [readStory], goodBye])

-- | Generates functions that write to the file.
writeStory :: (OOProg r) => MSBlock r
writeStory = block [
  varDec $ var "fileToWrite" outfile mainFn,

  openFileW (var "fileToWrite" outfile mainFn) (litString "testText.txt"),
  printFile (valueOf $ var "fileToWrite" outfile mainFn) (litInt 0),
  printFileLn (valueOf $ var "fileToWrite" outfile mainFn) (litDouble 0.89),
  printFileStr (valueOf $ var "fileToWrite" outfile mainFn) "ello",
  printFileStrLn (valueOf $ var "fileToWrite" outfile mainFn) "bye",
  printFileStrLn (valueOf $ var "fileToWrite" outfile mainFn) "!!",
  closeFile (valueOf $ var "fileToWrite" outfile mainFn),

  varDec $ var "fileToRead" infile mainFn,
  openFileR (var "fileToRead" infile mainFn) (litString "testText.txt"),
  varDec $ var "fileLine" string mainFn,
  getFileInputLine (valueOf $ var "fileToRead" infile mainFn)
                    (var "fileLine" string mainFn),
  discardFileLine (valueOf $ var "fileToRead" infile mainFn),
  listDec 0 (var "fileContents" (listType string) mainFn)]

-- | Generates functions to read from a file.
readStory :: (OOProg r) => MSStatement r
readStory = getFileInputAll (valueOf $ var "fileToRead" infile mainFn)
  (var "fileContents" (listType string) mainFn)

-- | Prints the result of the 'readStory' function. Should be the same as
-- what was given in 'writeStory'.
goodBye :: (OOProg r) => MSBlock r
goodBye = block [
  printLn (valueOf $ var "fileContents" (listType string) mainFn),
  closeFile (valueOf $ var "fileToRead" infile mainFn)]
