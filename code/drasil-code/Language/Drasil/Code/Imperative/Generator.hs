module Language.Drasil.Code.Imperative.Generator (
  generator, generateCode
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.ConceptMatch (chooseConcept)
import Language.Drasil.Code.Imperative.Descriptions (unmodularDesc)
import Language.Drasil.Code.Imperative.SpaceMatch (chooseSpace)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..), 
  genDoxConfig, genModule, genModuleWithImports)
import Language.Drasil.Code.Imperative.GenODE (chooseODELib)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (genModDef, genModFuncs,
  genModClasses)
import Language.Drasil.Code.Imperative.Modules (chooseInModule, genConstClass, 
  genConstMod, genInputClass, genInputConstraints, genInputDerived, 
  genInputFormat, genMain, genMainFunc, genCalcMod, genCalcFunc, 
  genOutputFormat, genOutputMod, genSampleInput)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..), inMod,
  modExportMap, clsDefMap)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..))
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Code.ExtLibImport (auxMods, imports, modExports)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.CodeSpec (CodeSpec(..), Choices(..), Modularity(..), 
  Visibility(..))

import GOOL.Drasil (GSProgram, SFile, OOProg, ProgramSym(..), ScopeTag(..), 
  ProgData(..), initialState, unCI)

import System.Directory (setCurrentDirectory, createDirectoryIfMissing, 
  getCurrentDirectory)
import Control.Lens ((^.))
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (evalState, runState)
import Data.List (nub)
import Data.Map (fromList, member, keys, elems)
import Data.Maybe (maybeToList)

generator :: Lang -> String -> [Expr] -> Choices -> CodeSpec -> DrasilState
generator l dt sd chs spec = DrasilState {
  -- constants
  codeSpec = spec,
  date = showDate $ dates chs,
  modular = modularity chs,
  implType = impType chs,
  inStruct = inputStructure chs,
  conStruct = constStructure chs,
  conRepr = constRepr chs,
  logKind  = logging chs,
  commented = comments chs,
  doxOutput = doxVerbosity chs,
  concMatches = chooseConcept chs,
  spaceMatches = chooseSpace l chs,
  auxiliaries = auxFiles chs,
  sampleData = sd,
  modules = modules',
  extLibMap = fromList elmap,
  libPaths = maybeToList pth,
  eMap = mem,
  libEMap = lem,
  clsMap = cdm,
  defList = nub $ keys mem ++ keys cdm,
  
  -- state
  currentModule = "",
  currentClass = "",

  -- next depend on chs
  logName = logFile chs,
  onSfwrC = onSfwrConstraint chs,
  onPhysC = onPhysConstraint chs
}
  where showDate Show = dt
        showDate Hide = ""
        (pth, elmap) = chooseODELib l (odeLib chs) (odes chs)
        els = map snd elmap
        mem = modExportMap spec chs modules' 
        lem = fromList (concatMap (^. modExports) els)
        cdm = clsDefMap spec chs modules'
        modules' = mods spec ++ concatMap (^. auxMods) els

generateCode :: (OOProg progRepr, PackageSym packRepr) => Lang -> 
  (progRepr (Program progRepr) -> ProgData) -> (packRepr (Package packRepr) -> 
  PackData) -> DrasilState -> IO ()
generateCode l unReprProg unReprPack g = do 
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False (getDir l)
  setCurrentDirectory (getDir l)
  let pckg = runReader (genPackage unReprProg) g 
      code = makeCode (progMods $ packProg $ unReprPack pckg) (packAux $ 
          unReprPack pckg)
  createCodeFiles code
  setCurrentDirectory workingDir

genPackage :: (OOProg progRepr, PackageSym packRepr) => 
  (progRepr (Program progRepr) -> ProgData) -> 
  Reader DrasilState (packRepr (Package packRepr))
genPackage unRepr = do
  g <- ask
  ci <- genProgram
  p <- genProgram
  let info = unCI $ evalState ci initialState
      (reprPD, s) = runState p info
      pd = unRepr reprPD
      n = pName $ codeSpec g
      m = makefile (libPaths g) (implType g) (commented g) s pd
  i <- genSampleInput
  d <- genDoxConfig n s
  return $ package pd (m:i++d)

genProgram :: (OOProg r) => Reader DrasilState (GSProgram r)
genProgram = do
  g <- ask
  ms <- chooseModules $ modular g
  let n = pName $ codeSpec g
  return $ prog n ms

chooseModules :: (OOProg r) => Modularity -> Reader DrasilState [SFile r]
chooseModules Unmodular = liftS genUnmodular
chooseModules (Modular _) = genModules

genUnmodular :: (OOProg r) => Reader DrasilState (SFile r)
genUnmodular = do
  g <- ask
  umDesc <- unmodularDesc
  let n = pName $ codeSpec g
      cls = any (`member` clsMap g) 
        ["get_input", "derived_values", "input_constraints"]
  genModuleWithImports n umDesc (concatMap (^. imports) (elems $ extLibMap g))
    (genMainFunc 
      : map (fmap Just) (map genCalcFunc (execOrder $ codeSpec g) 
        ++ concatMap genModFuncs (modules g)) 
      ++ ((if cls then [] else [genInputFormat Pub, genInputDerived Pub, 
        genInputConstraints Pub]) ++ [genOutputFormat])) 
    ([genInputClass Auxiliary, genConstClass Auxiliary] 
      ++ map (fmap Just) (concatMap genModClasses $ modules g))
          
genModules :: (OOProg r) => Reader DrasilState [SFile r]
genModules = do
  g <- ask
  mn     <- genMain
  inp    <- chooseInModule $ inMod g
  con    <- genConstMod 
  cal    <- genCalcMod
  out    <- genOutputMod
  moddef <- traverse genModDef (modules g) -- hack ?
  return $ mn : inp ++ con ++ cal : out ++ moddef

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"