module Test.Main (main) where

import Language.Drasil.Code (Label, PackageSym(..), JavaCode(..), 
  PythonCode(..), CSharpCode(..), CppSrcCode(..), CppHdrCode(..), ModData(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Test.HelloWorld (helloWorld)
import Test.PatternTest (patternTest)
import Test.FileTests (fileTests)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode (classes unJC) [".java"]
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "python"
  setCurrentDirectory "python"
  genCode (classes unPC) [".py"]
  setCurrentDirectory workingDir 
  createDirectoryIfMissing False "csharp"
  setCurrentDirectory "csharp"
  genCode (classes unCSC) [".cs"]
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "cpp"
  setCurrentDirectory "cpp"
  genCode (classes unCPPSC) [".cpp"]
  genCode (classes unCPPHC) [".hpp"]
  setCurrentDirectory workingDir
    
genCode :: [([ModData], Label)] -> [Label] -> IO()
genCode files exts = createCodeFiles (concatMap (\(ms, l) -> replicate (length ms) l) files) $ makeCode (concatMap fst files) exts

classes :: (PackageSym repr) => (repr (Package repr) -> ([ModData], Label)) -> [([ModData], Label)]
classes unRepr = map unRepr [helloWorld, patternTest, fileTests]

-- | Takes code and extensions
makeCode :: [ModData] -> [Label] -> [(FilePath, Doc)]
makeCode files exts =
    [(name ++ ext, file) | (name, (file, ext)) <- zip (repeatListElems (length exts) (map name files)) (zip (map modDoc files) (cycle exts))]

repeatListElems :: Int -> [a] -> [a]
repeatListElems _ [] = []
repeatListElems 1 xs = xs
repeatListElems n (x:xs) = (take n (repeat x)) ++ repeatListElems n xs

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files
createCodeFiles :: [Label] -> [(FilePath, Doc)] -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles ns cs = mapM_ createCodeFile (zip ns cs)

createCodeFile :: (Label, (FilePath, Doc)) -> IO ()
createCodeFile (n, (path, code)) = do
    createDirectoryIfMissing False n
    setCurrentDirectory n
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h
    setCurrentDirectory ".."
