-- | Simple GOOL test program for making sure generated programs can read
-- and write to files. See stable/gooltest for more details on what is generated through this.
module FileTests (fileTestsOO, fileTestsProc) where

import Drasil.GOOL (MSBlock, MSStatement, SMethod, SharedProg, OOProg,
  BodySym(..), BlockSym(..), TypeSym(..), DeclStatement(..), IOStatement(..),
  mainVar,Literal(..), VariableValue(..), MethodSym(..))
import qualified Drasil.GOOL as OO (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Prelude hiding (return, print, log, exp, sin, cos, tan)

-- | Creates a program in GOOL to test reading and writing to files.
fileTestsOO :: (OOProg r) => OO.GSProgram r
fileTestsOO = OO.prog "FileTests" "" [OO.fileDoc (OO.buildModule "FileTests" []
  [fileTestMethod] [])]

-- | Creates a program in GProc to test reading and writing to files.
fileTestsProc :: (ProcProg r) => GProc.GSProgram r
fileTestsProc = GProc.prog "FileTests" "" [GProc.fileDoc (GProc.buildModule
  "FileTests" [] [fileTestMethod])]

-- | File test method starts with 'writeStory' and ends with 'goodBye'.
fileTestMethod :: (SharedProg r) => SMethod r
fileTestMethod = mainFunction (body [writeStory, block [readStory], goodBye])

-- | Generates functions that write to the file.
writeStory :: (SharedProg r) => MSBlock r
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
readStory :: (SharedProg r) => MSStatement r
readStory = getFileInputAll (valueOf $ mainVar "fileToRead" infile)
  (mainVar "fileContents" (listType string))

-- | Prints the result of the 'readStory' function. Should be the same as
-- what was given in 'writeStory'.
goodBye :: (SharedProg r) => MSBlock r
goodBye = block [
  printLn (valueOf $ mainVar "fileContents" (listType string)),
  closeFile (valueOf $ mainVar "fileToRead" infile)]
