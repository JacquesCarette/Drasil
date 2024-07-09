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
  varDec $ var "fileToWrite" outfile local, -- TODO: get scope from state.  This shows a problem - the main function may or may not be global scope

  openFileW (var "fileToWrite" outfile local) (litString "testText.txt"),
  printFile (valueOf $ var "fileToWrite" outfile local) (litInt 0),
  printFileLn (valueOf $ var "fileToWrite" outfile local) (litDouble 0.89),
  printFileStr (valueOf $ var "fileToWrite" outfile local) "ello",
  printFileStrLn (valueOf $ var "fileToWrite" outfile local) "bye",
  printFileStrLn (valueOf $ var "fileToWrite" outfile local) "!!",
  closeFile (valueOf $ var "fileToWrite" outfile local),

  varDec $ var "fileToRead" infile local,
  openFileR (var "fileToRead" infile local) (litString "testText.txt"),
  varDec $ var "fileLine" string local,
  getFileInputLine (valueOf $ var "fileToRead" infile local)
                    (var "fileLine" string local),
  discardFileLine (valueOf $ var "fileToRead" infile local),
  listDec 0 (var "fileContents" (listType string) local)]

-- | Generates functions to read from a file.
readStory :: (OOProg r) => MSStatement r
readStory = getFileInputAll (valueOf $ var "fileToRead" infile local)
  (var "fileContents" (listType string) local)

-- | Prints the result of the 'readStory' function. Should be the same as
-- what was given in 'writeStory'.
goodBye :: (OOProg r) => MSBlock r
goodBye = block [
  printLn (valueOf $ var "fileContents" (listType string) local),
  closeFile (valueOf $ var "fileToRead" infile local)]
