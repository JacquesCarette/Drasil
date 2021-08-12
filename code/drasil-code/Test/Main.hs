module Test.Main (main) where

import GOOL.Drasil (Label, OOProg, ProgramSym(..), unCI, unJC, unPC, unCSC, 
  unCPPC, unSC, FileData(..), ModData(..), ProgData(..), initialState)

import Language.Drasil.Code (PackageSym(..), AuxiliarySym(..), AuxData(..), 
  PackData(..), unPP, unJP, unCSP, unCPPP, unSP, ImplementationType(..))

import Text.PrettyPrint.HughesPJ (Doc, render)
import Control.Monad.State (evalState, runState)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath.Posix (takeDirectory)
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Test.HelloWorld (helloWorld)
import Test.PatternTest (patternTest)
import Test.FileTests (fileTests)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode (classes unJC unJP)
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "python"
  setCurrentDirectory "python"
  genCode (classes unPC unPP)
  setCurrentDirectory workingDir 
  createDirectoryIfMissing False "csharp"
  setCurrentDirectory "csharp"
  genCode (classes unCSC unCSP)
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "cpp"
  setCurrentDirectory "cpp"
  genCode (classes unCPPC unCPPP)
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "swift"
  setCurrentDirectory "swift"
  genCode (classes unSC unSP)
  setCurrentDirectory workingDir
    
genCode :: [PackData] -> IO()
genCode files = createCodeFiles (concatMap (\p -> replicate (length (progMods 
  (packProg p)) + length (packAux p)) (progName $ packProg p)) files) $ makeCode (map (progMods . packProg) files) (map packAux files)

-- Cannot assign the list of tests in a where clause and re-use it because the 
-- "r" type variable needs to be instantiated to two different types 
-- (CodeInfo and a renderer) each time this function is called
classes :: (OOProg r, PackageSym r') => (r (Program r) -> ProgData) -> 
  (r' (Package r') -> PackData) -> [PackData]
classes unRepr unRepr' = zipWith 
  (\p gs -> let (p',gs') = runState p gs 
                pd = unRepr p'
  in unRepr' $ package pd [makefile [] Program [] gs' pd]) 
  [helloWorld, patternTest, fileTests]
  (map (unCI . (`evalState` initialState)) [helloWorld, patternTest, fileTests])

-- | Takes code
makeCode :: [[FileData]] -> [[AuxData]] -> [(FilePath, Doc)]
makeCode files auxs = concat $ zipWith (++) 
  (map (map (\fd -> (filePath fd, modDoc $ fileMod fd))) files) 
  (map (map (\ad -> (auxFilePath ad, auxDoc ad))) auxs)
  
  -- zip (map filePath files) (map (modDoc . fileMod) files)
  -- ++ zip (map auxFilePath auxs) (map auxDoc auxs) 

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
    createDirectoryIfMissing True (takeDirectory path)
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h
    setCurrentDirectory ".."
