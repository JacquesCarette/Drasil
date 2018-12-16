module Example.FileTests (fileTests) where

import New (Label, Library,
  RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..),
  FunctionSym(..), SelectorFunction(..), ScopeSym(..), MethodTypeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

fileTests :: (RenderSym repr) => repr (RenderFile repr)
fileTests = fileDoc (buildModule "" [] [] [] [fileTestClass])

fileTestClass :: (RenderSym repr) => repr (Class repr)
fileTestClass = pubClass "FileTests" Nothing [privMVar 1 "dummy" int] 
  [mainMethod (body [writeStory, readStory, goodBye])]

writeStory :: (RenderSym repr) => repr (Block repr)
writeStory = block [
  (varDecDef "e" int (litInt 5)),
  (varDec "f" float),
  ("f" &.= (castObj (cast float int) (var "e"))),
  (varDec "fileToWrite" outfile),
  (openFileW (var "fileToWrite") (litString "../filetowrite.txt")),
  (printFile (var "fileToWrite") int (litInt 0)),
  (printFileLn (var "fileToWrite") int (litFloat 0.89)),
  (printFileStr (var "fileToWrite") "ello"),
  (printFileStrLn (var "fileToWrite") "byebye"),
  (varDec "fileToRead" infile),
  (openFileR (var "fileToRead") (litString "../filename.txt")),
  (varDec "fileLine" string),
  (getFileInputLine (var "fileToRead") (var "fileLine")),
  (discardFileLine (var "fileToRead")),
  (listDec "fileContents" 1 (listType dynamic string))]

readStory :: (RenderSym repr) => repr (Block repr)
readStory = (getFileInputAll (var "fileToRead") (var "fileContents"))

goodBye :: (RenderSym repr) => repr (Block repr)
goodBye = block [(closeFile (var "fileToWrite")), (closeFile (var "fileToRead"))]