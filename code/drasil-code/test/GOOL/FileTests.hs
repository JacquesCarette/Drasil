-- | Simple GOOL test program for making sure generated programs can read
-- and write to files. See stable/gooltest for more details on what is generated through this.
module GOOL.FileTests (fileTests) where

import Drasil.GOOL (GSProgram, MSBlock, MSStatement, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), BlockSym(..), TypeSym(..),
  DeclStatement(..), IOStatement(..), mainVar, Literal(..), VariableValue(..),
  MethodSym(..), ModuleSym(..))
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
  varDec $ mainVar "fileToWrite" outfile,

  openFileW (mainVar "fileToWrite" outfile) (litString "testText.txt"),
  printFile (valueOf $ mainVar "fileToWrite" outfile) (litInt 0),
  printFileLn (valueOf $ mainVar "fileToWrite" outfile) (litDouble 0.89),
  printFileStr (valueOf $ mainVar "fileToWrite" outfile) "ello",
  printFileStrLn (valueOf $ mainVar "fileToWrite" outfile) "bye",
  printFileStrLn (valueOf $ mainVar "fileToWrite" outfile) "!!",
  closeFile (valueOf $ mainVar "fileToWrite" outfile),

  varDec $ mainVar "fileToRead" infile,
  openFileR (mainVar "fileToRead" infile) (litString "testText.txt"),
  varDec $ mainVar "fileLine" string,
  getFileInput (valueOf $ mainVar "fileToRead" infile)
                    (mainVar "fileLine" string),
  discardFileLine (valueOf $ mainVar "fileToRead" infile),
  listDec 0 (mainVar "fileContents" (listType string))]

-- | Generates functions to read from a file.
readStory :: (OOProg r) => MSStatement r
readStory = getFileInputAll (valueOf $ mainVar "fileToRead" infile)
  (mainVar "fileContents" (listType string))

-- | Prints the result of the 'readStory' function. Should be the same as
-- what was given in 'writeStory'.
goodBye :: (OOProg r) => MSBlock r
goodBye = block [
  printLn (valueOf $ mainVar "fileContents" (listType string)),
  closeFile (valueOf $ mainVar "fileToRead" infile)]
