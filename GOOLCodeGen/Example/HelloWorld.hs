module Example.HelloWorld (helloWorld) where

import Language.Drasil.Code
import Prelude hiding (return)

helloWorld :: Module repr
helloWorld = buildModule "Control" [] [] [main_func] []

main_func :: repr FunctionDecl
main_func = mainMethod body 
  [
    printStrLn "Hello, world"
  ]

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode javaLabel
  setCurrentDirectory workingDir
    
genCode :: String -> IO()
genCode lang = createCodeFiles $ makeCode 
  lang
  [helloWorld] ["HelloWorld"]