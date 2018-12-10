module Example.HelloWorld (helloWorld) where

import LanguageRenderer.NewJavaRenderer (JavaCodeMonad(..))
import Prelude hiding (return)

helloWorld :: repr Doc
helloWorld = fileDoc (printStrLn "Hello, world")

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode [unJC helloWorld] ["HelloWorld"] [".java"]
  setCurrentDirectory workingDir
    
genCode :: [a] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts