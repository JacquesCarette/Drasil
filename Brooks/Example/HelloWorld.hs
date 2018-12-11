module Example.HelloWorld (helloWorld) where

import New (Class, Method, Body, Block, Statement, Declaration, Value, StateType,
  Function, StateVar, IOType, IOSt, Scope, Keyword, Label, Library, VarDecl, 
  FunctionDecl,
  RenderSym(..), KeywordSym(..), ClassSym(..), MethodSym(..), 
  BodySym(..), Symantics(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
  IOStSym(..), ValueSym(..), Selector(..), FunctionSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode [unJC helloWorld] ["HelloWorld"] [".java"]
  setCurrentDirectory workingDir
    
genCode :: [Doc] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts

helloWorld :: (RenderSym repr) => repr Doc
helloWorld = fileDoc (
  block [
    printStrLn "Hello, world",
    printLn (string) (litString " too"),
    printStr "boo",
    print (bool) litTrue])