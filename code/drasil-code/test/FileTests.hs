-- | Simple GOOL test program for making sure generated programs can read
-- and write to files. See stable/gooltest for more details on what is generated through this.
module FileTests (fileTestsOO, fileTestsProc) where

import Drasil.GOOL (MS, OOProg, BodySym(..), BlockSym(..), TypeSym(..),
  DeclStatement(..), PrintConsole(..), FileHandling(..), PrintFile(..),
  ReadFile(..), ControlStatement(..), VariableSym(var), Literal(..),
  VariableValue(..), Comparison(..), List(..), MethodSym(..), ScopeSym(..))
import qualified Drasil.GOOL as OO (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))

-- | Creates a program in GOOL to test reading and writing to files.
fileTestsOO
  :: (OOProg r vis scope typ param val stmt mthd stvr attch prg file mod bod block)
  => OO.GSProgram r prg
fileTestsOO = OO.prog "FileTests" "" [OO.fileDoc (OO.buildModule "FileTests" []
  [fileTestMethod] [])]

-- | Creates a program in GProc to test reading and writing to files.
fileTestsProc
  :: (ProcProg r vis scope typ param val stmt mthd prg file mod bod block)
  => GProc.GSProgram r prg
fileTestsProc = GProc.prog "FileTests" "" [GProc.fileDoc (GProc.buildModule
  "FileTests" [] [fileTestMethod])]

-- | File test method starts with 'writeStory' and ends with 'goodBye'.
fileTestMethod
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , TypeSym r typ
    , Literal r typ val
    , ScopeSym r scope
    , VariableSym r typ
    , VariableValue r val
    , Comparison r val
    , List r val
    , DeclStatement r scope val stmt bod
    , ControlStatement r val stmt bod
    , PrintConsole r val stmt
    , FileHandling r val stmt
    , PrintFile r val stmt
    , ReadFile r val stmt
    , MethodSym r vis typ param mthd bod
    )
  => MS (r mthd)
fileTestMethod = mainFunction (body [writeStory, block [readStory], goodBye])

-- | Generates functions that write to the file.
writeStory
  ::
    ( BlockSym r block stmt
    , TypeSym r typ
    , Literal r typ val
    , ScopeSym r scope
    , VariableSym r typ
    , VariableValue r val
    , Comparison r val
    , DeclStatement r scope val stmt bod
    , ControlStatement r val stmt bod
    , FileHandling r val stmt
    , PrintFile r val stmt
    , ReadFile r val stmt
    )
  => MS (r block)
writeStory = block [
  varDec (var "fileToWrite" outfile) mainFn,

  openFileW (var "fileToWrite" outfile) (litString "testText.txt"),
  printFile (valueOf $ var "fileToWrite" outfile) (litInt 0),
  printFileLn (valueOf $ var "fileToWrite" outfile) (litDouble 0.89),
  printFileStr (valueOf $ var "fileToWrite" outfile) "ello",
  printFileStrLn (valueOf $ var "fileToWrite" outfile) "bye",
  printFileStrLn (valueOf $ var "fileToWrite" outfile) "!!",
  closeFile (valueOf $ var "fileToWrite" outfile),

  varDec (var "fileToRead" infile) mainFn,
  openFileR (var "fileToRead" infile) (litString "testText.txt"),
  varDec (var "fileLine" string) mainFn,
  getFileInput (valueOf $ var "fileToRead" infile)
                    (var "fileLine" string),
  discardFileLine (valueOf $ var "fileToRead" infile),
  assert (valueOf (var "fileLine" string) ?!= litString "") (litString "First line should not be empty."),
  listDec 0 (var "fileContents" (listType string)) mainFn]

-- | Generates functions to read from a file.
readStory
  :: (TypeSym r typ, VariableSym r typ, VariableValue r val, ReadFile r val stmt)
  => MS (r stmt)
readStory = getFileInputAll (valueOf $ var "fileToRead" infile)
  (var "fileContents" (listType string))

-- | Prints the result of the 'readStory' function. Should be the same as
-- what was given in 'writeStory'.
goodBye
  ::
    ( BlockSym r block stmt
    , TypeSym r typ
    , Comparison r val
    , Literal r typ val
    , VariableSym r typ
    , VariableValue r val
    , List r val
    , ControlStatement r val stmt bod
    , PrintConsole r val stmt
    , FileHandling r val stmt
    )
  => MS (r block)
goodBye = block [
  printLn (valueOf $ var "fileContents" (listType string)),
  assert (listSize (valueOf (var "fileContents" (listType string))) ?> litInt 0)
  (litString "fileContents should not be empty."),
  closeFile (valueOf $ var "fileToRead" infile)]
