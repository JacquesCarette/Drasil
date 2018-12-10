module Example.HelloWorld (helloWorld) where

import Language.Drasil.Code
import Prelude hiding (return)

helloWorld :: repr Statement
helloWorld = printStrLn "Hello, world"

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode [unJC helloWorld] ["HelloWorld"] [".java"]
  setCurrentDirectory workingDir
    
genCode :: [a] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts