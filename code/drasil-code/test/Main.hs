-- | Main module to gather all the GOOL tests and generate them.
module Main (main) where

import Drasil.GOOL (Label, OOProg, unJC, unPC, unCSC,
  unCPPC, unSC, initialState, FileData(..), ModData(..))
import qualified Drasil.GOOL as OO (unCI, ProgramSym(..), ProgData(..))
import Drasil.GProc (ProcProg, unJLC)
import qualified Drasil.GProc as Proc (unCI, ProgramSym(..), ProgData(..))

import Language.Drasil.Code (PackageSym(..), AuxiliarySym(..), AuxData(..),
  PackData(..), unPP, unJP, unCSP, unCPPP, unSP, unJLP, ImplementationType(..))

import Text.PrettyPrint.HughesPJ (Doc, render)
import Control.Monad.State (evalState, runState)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath.Posix (takeDirectory)
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

import qualified GOOL.HelloWorld as OO (helloWorld)
import GOOL.PatternTest (patternTest)
import qualified GOOL.FileTests as OO (fileTests)
import qualified GOOL.VectorTest as OO (vectorTest)
import qualified GOOL.NameGenTest as OO (nameGenTest)

import qualified GProc.HelloWorld as Proc (helloWorld)
import qualified GProc.FileTests as Proc (fileTests)
import qualified GProc.VectorTest as Proc (vectorTest)
import qualified GProc.NameGenTest as Proc (nameGenTest)

-- | Renders five GOOL tests (FileTests, HelloWorld, PatternTest, VectorTest, and NameGenTest)
-- in Java, Python, C#, C++, and Swift.
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
  createDirectoryIfMissing False "julia"
  setCurrentDirectory "julia"
  genCode (jlClasses unJLC unJLP)
  setCurrentDirectory workingDir

-- | Gathers all information needed to generate code, sorts it, and calls the renderers.
genCode :: [PackData] -> IO()
genCode files = createCodeFiles (concatMap (\p -> replicate (length (OO.progMods
  (packProg p)) + length (packAux p)) (OO.progName $ packProg p)) files) $
    makeCode (map (OO.progMods . packProg) files) (map packAux files)

-- Cannot assign the list of tests in a where clause and re-use it because the
-- "r" type variable needs to be instantiated to two different types
-- (CodeInfo and a renderer) each time this function is called
-- | Gathers the GOOL file tests and prepares them for rendering
classes :: (OOProg r, PackageSym r') => (r (OO.Program r) -> OO.ProgData) ->
  (r' (Package r') -> PackData) -> [PackData]
classes unRepr unRepr' = zipWith
  (\p gs -> let (p',gs') = runState p gs
                pd = unRepr p'
  in unRepr' $ package pd [makefile [] Program [] gs' pd])
  [OO.helloWorld, patternTest, OO.fileTests, OO.vectorTest, OO.nameGenTest]
  (map (OO.unCI . (`evalState` initialState)) [OO.helloWorld, patternTest,
    OO.fileTests, OO.vectorTest, OO.nameGenTest])

-- Classes that Julia is currently able to render
jlClasses :: (ProcProg r, PackageSym r') => (r (Proc.Program r) ->
  Proc.ProgData) -> (r' (Package r') -> PackData) -> [PackData]
jlClasses unRepr unRepr' = zipWith
  (\p gs -> let (p',gs') = runState p gs
                pd = unRepr p'
  in unRepr' $ package pd [makefile [] Program [] gs' pd])
  [Proc.helloWorld, Proc.fileTests, Proc.vectorTest, Proc.nameGenTest]
  (map (Proc.unCI . (`evalState` initialState)) [Proc.helloWorld,
    Proc.fileTests, Proc.vectorTest, Proc.nameGenTest])

-- | Formats code to be rendered.
makeCode :: [[FileData]] -> [[AuxData]] -> [(FilePath, Doc)]
makeCode files auxs = concat $ zipWith (++)
  (map (map (\fd -> (filePath fd, modDoc $ fileMod fd))) files)
  (map (map (\ad -> (auxFilePath ad, auxDoc ad))) auxs)

  -- zip (map filePath files) (map (modDoc . fileMod) files)
  -- ++ zip (map auxFilePath auxs) (map auxDoc auxs)

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files.
createCodeFiles :: [Label] -> [(FilePath, Doc)] -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles ns cs = mapM_ createCodeFile (zip ns cs)

-- | Helper that creates the file and renders code.
createCodeFile :: (Label, (FilePath, Doc)) -> IO ()
createCodeFile (n, (path, code)) = do
    createDirectoryIfMissing False n
    setCurrentDirectory n
    createDirectoryIfMissing True (takeDirectory path)
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h
    setCurrentDirectory ".."
