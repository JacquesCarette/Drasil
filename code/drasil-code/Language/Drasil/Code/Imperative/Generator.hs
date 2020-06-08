module Language.Drasil.Code.Imperative.Generator (
  generator, generateCode
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.ConceptMatch (chooseConcept)
import Language.Drasil.Code.Imperative.Descriptions (unmodularDesc)
import Language.Drasil.Code.Imperative.SpaceMatch (chooseSpace)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..), 
  genDoxConfig, genModuleWithImports)
import Language.Drasil.Code.Imperative.GenODE (chooseODELib)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (genModDef, genModFuncs,
  genModClasses)
import Language.Drasil.Code.Imperative.Modules (chooseInModule, genConstClass, 
  genConstMod, genInputClass, genInputConstraints, genInputDerived, 
  genInputFormat, genMain, genMainFunc, genCalcMod, genCalcFunc, 
  genOutputFormat, genOutputMod, genSampleInput)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..), 
  designLog, inMod, modExportMap, clsDefMap)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..), ad)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Code.ExtLibImport (auxMods, imports, modExports)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Choices (Choices(..), Modularity(..), Visibility(..))
import Language.Drasil.CodeSpec (CodeSpec(..))

import GOOL.Drasil (GSProgram, SFile, OOProg, ProgramSym(..), ScopeTag(..), 
  ProgData(..), initialState, unCI)

import System.Directory (setCurrentDirectory, createDirectoryIfMissing, 
  getCurrentDirectory)
import Control.Lens ((^.))
import Control.Monad.State (get, evalState, runState)
import Data.List (nub)
import Data.Map (fromList, member, keys, elems)
import Data.Maybe (maybeToList)
import Text.PrettyPrint.HughesPJ (($$), empty)

generator :: Lang -> String -> [Expr] -> Choices -> CodeSpec -> DrasilState
generator l dt sd chs spec = DrasilState {
  -- constants
  codeSpec = spec,
  modular = modularity chs,
  inStruct = inputStructure chs,
  conStruct = constStructure chs,
  conRepr = constRepr chs,
  concMatches = mcm,
  spaceMatches = chooseSpace l chs,
  implType = impType chs,
  onSfwrC = onSfwrConstraint chs,
  onPhysC = onPhysConstraint chs,
  commented = comments chs,
  doxOutput = doxVerbosity chs,
  date = showDate $ dates chs,
  logKind  = logging chs,
  logName = logFile chs,
  auxiliaries = auxFiles chs,
  sampleData = sd,
  modules = modules',
  extLibMap = fromList elmap,
  libPaths = maybeToList pth,
  eMap = mem,
  libEMap = lem,
  clsMap = cdm,
  defList = nub $ keys mem ++ keys cdm,
  
  -- stateful
  currentModule = "",
  currentClass = "",
  _designLog = concLog $$ libLog,
  _loggedSpaces = []
}
  where (mcm, concLog) = runState (chooseConcept chs) empty
        showDate Show = dt
        showDate Hide = ""
        ((pth, elmap), libLog) = runState (chooseODELib l (odeLib chs) 
          (odes chs)) empty
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
  let (pckg, ds) = runState (genPackage unReprProg) g 
      code = makeCode (progMods $ packProg $ unReprPack pckg) 
        (ad "designLog.txt" (ds ^. designLog) : packAux (unReprPack pckg))
  createCodeFiles code
  setCurrentDirectory workingDir

genPackage :: (OOProg progRepr, PackageSym packRepr) => 
  (progRepr (Program progRepr) -> ProgData) -> 
  GenState (packRepr (Package packRepr))
genPackage unRepr = do
  g <- get
  ci <- genProgram
  p <- genProgram
  let info = unCI $ evalState ci initialState
      (reprPD, s) = runState p info
      pd = unRepr reprPD
      m = makefile (libPaths g) (implType g) (commented g) s pd
  i <- genSampleInput
  d <- genDoxConfig s
  return $ package pd (m:i++d)

genProgram :: (OOProg r) => GenState (GSProgram r)
genProgram = do
  g <- get
  ms <- chooseModules $ modular g
  let n = pName $ codeSpec g
  return $ prog n ms

chooseModules :: (OOProg r) => Modularity -> GenState [SFile r]
chooseModules Unmodular = liftS genUnmodular
chooseModules (Modular _) = genModules

genUnmodular :: (OOProg r) => GenState (SFile r)
genUnmodular = do
  g <- get
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
          
genModules :: (OOProg r) => GenState [SFile r]
genModules = do
  g <- get
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