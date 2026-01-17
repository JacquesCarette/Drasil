-- | Main module to gather all the GOOL tests and generate them.
module Main (main) where

import Drasil.GOOL (Label, OOProg, unJC, unPC, unCSC,
  unCPPC, unSC, initialState, FileData(..), ProgData(..), ModData(..))
import qualified Drasil.GOOL as OO (unCI, ProgramSym(..))
import Drasil.GProc (ProcProg, unJLC)
import qualified Drasil.GProc as Proc (unCI, ProgramSym(..))

import Language.Drasil.Code (ImplementationType(..))
import Language.Drasil.GOOL (PackageSym(..), AuxiliarySym(..),
  FileAndContents(fileDoc), PackData(..), unPP, unJP, unCSP, unCPPP, unSP,
  unJLP)
import qualified Language.Drasil.GOOL as D (filePath)

import Utils.Drasil (createDirIfMissing)

import Text.PrettyPrint.HughesPJ (Doc, render)
import Control.Monad.State (evalState, runState)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.FilePath.Posix (takeDirectory)
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

import HelloWorld (helloWorldOO, helloWorldProc)
import GOOL.PatternTest (patternTest)
import FileTests (fileTestsOO, fileTestsProc)
import VectorTest (vectorTestOO, vectorTestProc)
import NameGenTest (nameGenTestOO, nameGenTestProc)

-- | Renders five GOOL tests (FileTests, HelloWorld, PatternTest, VectorTest, and NameGenTest)
-- in Java, Python, C#, C++, Swift, and Julia.
main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirIfMissing False "java"
  setCurrentDirectory "java"
  genCode (classes unJC unJP)
  setCurrentDirectory workingDir
  createDirIfMissing False "python"
  setCurrentDirectory "python"
  genCode (classes unPC unPP)
  setCurrentDirectory workingDir
  createDirIfMissing False "csharp"
  setCurrentDirectory "csharp"
  genCode (classes unCSC unCSP)
  setCurrentDirectory workingDir
  createDirIfMissing False "cpp"
  setCurrentDirectory "cpp"
  genCode (classes unCPPC unCPPP)
  setCurrentDirectory workingDir
  createDirIfMissing False "swift"
  setCurrentDirectory "swift"
  genCode (classes unSC unSP)
  setCurrentDirectory workingDir
  createDirIfMissing False "julia"
  setCurrentDirectory "julia"
  genCode (jlClasses unJLC unJLP)
  setCurrentDirectory workingDir

-- | Gathers all information needed to generate code, sorts it, and calls the renderers.
genCode :: [PackData ProgData] -> IO()
genCode files = createCodeFiles (concatMap (\p -> replicate (length (progMods
  (packProg p)) + length (packAux p)) (progName $ packProg p)) files) $
    makeCode (map (progMods . packProg) files) (map packAux files)

-- Cannot assign the list of tests in a where clause and re-use it because the
-- "r" type variable needs to be instantiated to two different types
-- (CodeInfo and a renderer) each time this function is called
-- | Gathers the GOOL file tests and prepares them for rendering
classes :: (OOProg r, PackageSym r') => (r (OO.Program r) -> ProgData) ->
  (r' (Package r') -> PackData ProgData) -> [PackData ProgData]
classes unRepr unRepr' = zipWith
  (\p gs -> let (p',gs') = runState p gs
                pd = unRepr p'
  in unRepr' $ package pd [makefile [] Program [] gs' pd])
  [helloWorldOO, patternTest, fileTestsOO, vectorTestOO, nameGenTestOO]
  (map (OO.unCI . (`evalState` initialState)) [helloWorldOO, patternTest,
    fileTestsOO, vectorTestOO, nameGenTestOO])

-- Classes that Julia is currently able to render
jlClasses :: (ProcProg r, PackageSym r') => (r (Proc.Program r) ->
  ProgData) -> (r' (Package r') -> PackData ProgData) -> [PackData ProgData]
jlClasses unRepr unRepr' = zipWith
  (\p gs -> let (p',gs') = runState p gs
                pd = unRepr p'
  in unRepr' $ package pd [makefile [] Program [] gs' pd])
  [helloWorldProc, fileTestsProc, vectorTestProc, nameGenTestProc]
  (map (Proc.unCI . (`evalState` initialState)) [helloWorldProc,
    fileTestsProc, vectorTestProc, nameGenTestProc])

-- | Formats code to be rendered.
makeCode :: [[FileData]] -> [[FileAndContents]] -> [(FilePath, Doc)]
makeCode files auxs = concat $ zipWith (++)
  (map (map (\fd -> (filePath fd, modDoc $ fileMod fd))) files)
  (map (map (\fileAndContents ->
      (D.filePath fileAndContents, fileDoc fileAndContents))) auxs)

  -- zip (map filePath files) (map (modDoc . fileMod) files)
  -- ++ zip (map D.filePath auxs) (map fileDoc auxs)

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files.
createCodeFiles :: [Label] -> [(FilePath, Doc)] -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles ns cs = mapM_ createCodeFile (zip ns cs)

-- | Helper that creates the file and renders code.
createCodeFile :: (Label, (FilePath, Doc)) -> IO ()
createCodeFile (n, (path, code)) = do
    createDirIfMissing False n
    setCurrentDirectory n
    createDirIfMissing True (takeDirectory path)
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h
    setCurrentDirectory ".."
