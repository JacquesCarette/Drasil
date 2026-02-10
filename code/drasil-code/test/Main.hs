{-# LANGUAGE PatternSynonyms, TupleSections #-}

-- | Main module to gather all the GOOL tests and generate them.
module Main (main) where

import Drasil.GOOL (Label, OOProg, unJC, unPC, unCSC, unCPPC, unSC,
  initialState, ProgData(..), headers, sources, mainMod)
import qualified Drasil.GOOL as OO (unCI, ProgramSym(..))
import Drasil.GProc (ProcProg, unJLC)
import qualified Drasil.GProc as Proc (unCI, ProgramSym(..))

import Language.Drasil.Code (ImplementationType(..), makeSds)
import Language.Drasil.GOOL (AuxiliarySym(..), package,
  hasPathAndDocToFileAndContents, PackageData(..), pattern PackageData,
  unPP, unJP, unCSP, unCPPP, unSP, unJLP)
import qualified Language.Drasil.GOOL as D (filePath, FileAndContents(..))

import Utils.Drasil (createDirIfMissing, createFile)

import Text.PrettyPrint.HughesPJ (render)
import Control.Monad.State (evalState, runState)
import Control.Lens ((^.))
import Data.Functor ((<&>))
import Data.Foldable (traverse_)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
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
genCode :: [PackageData ProgData] -> IO()
genCode files =
  createCodeFiles $ files >>= \(PackageData prog aux) ->
    let label = progName prog
        modCode = progMods prog <&> \modFileData ->
          (label, hasPathAndDocToFileAndContents modFileData)
        auxCode = aux <&> (label,)
    in modCode ++ auxCode

classes :: (OOProg r, AuxiliarySym r', Monad r') => (r (OO.Program r) -> ProgData) ->
  (r' (PackageData ProgData) -> PackageData ProgData) -> [PackageData ProgData]
classes unRepr unRepr' = zipWith
  (\p gs -> let (p',gs') = runState p gs
                pd = unRepr p'
                fileInfoState = makeSds (gs' ^. headers) (gs' ^. sources)
                                        (gs' ^. mainMod)
  in unRepr' $ package pd [makefile [] Program [] fileInfoState pd])
  [helloWorldOO, patternTest, fileTestsOO, vectorTestOO, nameGenTestOO]
  (map (OO.unCI . (`evalState` initialState)) [helloWorldOO, patternTest,
    fileTestsOO, vectorTestOO, nameGenTestOO])

-- Classes that Julia is currently able to render
jlClasses :: (ProcProg r, AuxiliarySym r', Monad r') => (r (Proc.Program r) -> ProgData) ->
  (r' (PackageData ProgData) -> PackageData ProgData) -> [PackageData ProgData]
jlClasses unRepr unRepr' = zipWith
  (\p gs -> let (p',gs') = runState p gs
                pd = unRepr p'
                fileInfoState = makeSds (gs' ^. headers) (gs' ^. sources)
                                        (gs' ^. mainMod)
  in unRepr' $ package pd [makefile [] Program [] fileInfoState pd])
  [helloWorldProc, fileTestsProc, vectorTestProc, nameGenTestProc]
  (map (Proc.unCI . (`evalState` initialState)) [helloWorldProc,
    fileTestsProc, vectorTestProc, nameGenTestProc])

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files.
createCodeFiles :: [(Label, D.FileAndContents)] -> IO ()
createCodeFiles = traverse_ $ \(name, file) -> do
  let path = name ++ "/" ++ D.filePath file
  createFile path (render $ D.fileDoc file)
