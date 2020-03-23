module Language.Drasil.Code.Imperative.Generator (
  generator, generateCode
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.ConceptMatch (chooseConcept)
import Language.Drasil.Code.Imperative.SpaceMatch (chooseSpace)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..), 
  genDoxConfig, genModule)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (genModDef, genModFuncs)
import Language.Drasil.Code.Imperative.Modules (chooseInModule, genConstClass, 
  genConstMod, genInputClass, genInputConstraints, genInputDerived, 
  genInputFormat, genMain, genMainFunc, genOutputFormat, genOutputMod, 
  genSampleInput)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..), inMod,
  modExportMap, clsDefMap)
import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..))
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Code.ExtLibImport (auxMods, imports, modExports, 
  genExternalLibraryCall)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Chunk.Code (codeName)
import Language.Drasil.Chunk.CodeDefinition (odeDef)
import Language.Drasil.Data.ODELibPckg (ODELibPckg(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Choices(..), 
  Modularity(..), ImplementationType(..), Visibility(..), assocToMap, 
  getAdditionalVars)
import Language.Drasil.Mod (Func(..), packmodRequires)

import GOOL.Drasil (ProgramSym(..), ProgramSym, FileSym(..), ProgData(..), GS, 
  FS, initialState, unCI)

import System.Directory (setCurrentDirectory, createDirectoryIfMissing, 
  getCurrentDirectory)
import Control.Lens ((^.))
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (evalState, runState)
import Data.List (nub)
import Data.Map (fromList, member, keys)
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
  vMap = assocToMap (codequants spec ++ getAdditionalVars chs modules'),
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
        ols = odeLib chs
        els = map snd elmap
        (pth, elmap) = chooseODELib l ols
        mem = modExportMap (csi spec) chs modules' 
        lem = fromList (concatMap (^. modExports) els)
        cdm = clsDefMap (csi spec) chs modules'
        chooseODELib _ [] = (Nothing, [])
        chooseODELib lng (o:os) = if lng `elem` compatibleLangs o then 
          (libPath o, map (\ode -> (codeName $ odeDef ode, 
          genExternalLibraryCall (libSpec o) $ libCall o ode)) (odes chs)) else 
          chooseODELib lng os
        modules' = packmodRequires "Calculations" 
          "Provides functions for calculating the outputs" 
          (concatMap (^. imports) els) [] (map FCD (execOrder $ csi spec)) 
          : mods (csi spec) ++ concatMap (^. auxMods) els

generateCode :: (ProgramSym progRepr, PackageSym packRepr) => Lang -> 
  (progRepr (Program progRepr) -> ProgData) -> (packRepr (Package packRepr) -> 
  PackData) -> DrasilState -> IO ()
generateCode l unReprProg unReprPack g = do 
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False (getDir l)
  setCurrentDirectory (getDir l)
  createCodeFiles code
  setCurrentDirectory workingDir
  where pckg = runReader (genPackage unReprProg) g 
        code = makeCode (progMods $ packProg $ unReprPack pckg) (packAux $ 
          unReprPack pckg)

genPackage :: (ProgramSym progRepr, PackageSym packRepr) => 
  (progRepr (Program progRepr) -> ProgData) -> 
  Reader DrasilState (packRepr (Package packRepr))
genPackage unRepr = do
  g <- ask
  ci <- genProgram
  p <- genProgram
  let info = unCI $ evalState ci initialState
      (reprPD, s) = runState p info
      pd = unRepr reprPD
      n = pName $ csi $ codeSpec g
      m = makefile (libPaths g) (implType g) (commented g) s pd
  i <- genSampleInput
  d <- genDoxConfig n s
  return $ package pd (m:i++d)

genProgram :: (ProgramSym repr) => Reader DrasilState (GS (repr (Program repr)))
genProgram = do
  g <- ask
  ms <- chooseModules $ modular g
  let n = pName $ csi $ codeSpec g
  return $ prog n ms

chooseModules :: (ProgramSym repr) => Modularity -> 
  Reader DrasilState [FS (repr (RenderFile repr))]
chooseModules Unmodular = liftS genUnmodular
chooseModules (Modular _) = genModules

genUnmodular :: (ProgramSym repr) => 
  Reader DrasilState (FS (repr (RenderFile repr)))
genUnmodular = do
  g <- ask
  let n = pName $ csi $ codeSpec g
      cls = any (`member` clsMap g)
        ["get_input", "derived_values", "input_constraints"]
      getDesc Library = "library"
      getDesc Program = "program"
      mainIfExe Library = []
      mainIfExe Program = [genMainFunc]
  genModule n ("Contains the entire " ++ n ++ " " ++ getDesc (implType g))
    (map (fmap Just) (mainIfExe (implType g) ++ concatMap genModFuncs 
    (modules g)) ++ ((if cls then [] else [genInputFormat Primary, 
      genInputDerived Primary, genInputConstraints Primary]) ++ 
      [genOutputFormat])) 
    [genInputClass Auxiliary, genConstClass Auxiliary]
          
genModules :: (ProgramSym repr) => 
  Reader DrasilState [FS (repr (RenderFile repr))]
genModules = do
  g <- ask
  let mainIfExe Library = return []
      mainIfExe Program = sequence [genMain]
  mn     <- mainIfExe $ implType g
  inp    <- chooseInModule $ inMod g
  con    <- genConstMod 
  out    <- genOutputMod
  moddef <- traverse genModDef (modules g) -- hack ?
  return $ mn ++ inp ++ con ++ out ++ moddef

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"