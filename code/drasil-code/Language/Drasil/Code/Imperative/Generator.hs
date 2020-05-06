module Language.Drasil.Code.Imperative.Generator (
  generator, generateCode
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.ConceptMatch (chooseConcept)
import Language.Drasil.Code.Imperative.SpaceMatch (chooseSpace)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..), 
  genDoxConfig, genModule)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (genModDef, genModFuncs,
  genModClasses)
import Language.Drasil.Code.Imperative.Modules (chooseInModule, genConstClass, 
  genConstMod, genInputClass, genInputConstraints, genInputDerived, 
  genInputFormat, genMain, genMainFunc, genCalcMod, genCalcFunc, 
  genOutputFormat, genOutputMod, genSampleInput)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..), inMod)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..))
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Choices(..), 
  Lang(..), Modularity(..), ImplementationType(..), Visibility(..))

import GOOL.Drasil (GSProgram, SFile, OOProg, ProgramSym(..), ProgData(..), 
  initialState, unCI)

import System.Directory (setCurrentDirectory, createDirectoryIfMissing, 
  getCurrentDirectory)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (evalState, runState)
import Data.Map (member)

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

generateCode :: (OOProg progRepr, PackageSym packRepr) => Lang -> 
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
      n = pName $ csi $ codeSpec g
      m = makefile (implType g) (commented g) s pd
  i <- genSampleInput
  d <- genDoxConfig n s
  return $ package pd (m:i++d)

genProgram :: (OOProg r) => Reader DrasilState (GSProgram r)
genProgram = do
  g <- ask
  ms <- chooseModules $ modular g
  let n = pName $ csi $ codeSpec g
  return $ prog n ms

chooseModules :: (OOProg r) => Modularity -> Reader DrasilState [SFile r]
chooseModules Unmodular = liftS genUnmodular
chooseModules (Modular _) = genModules

genUnmodular :: (OOProg r) => Reader DrasilState (SFile r)
genUnmodular = do
  g <- ask
  let s = csi $ codeSpec g
      n = pName $ csi $ codeSpec g
      cls = any (`member` clsMap (codeSpec g)) 
        ["get_input", "derived_values", "input_constraints"]
      getDesc Library = "library"
      getDesc Program = "program"
      mainIfExe Library = []
      mainIfExe Program = [genMainFunc]
  genModule n ("Contains the entire " ++ n ++ " " ++ getDesc (implType g))
    (map (fmap Just) (mainIfExe (implType g) 
        ++ map genCalcFunc (execOrder $ csi $ codeSpec g) 
        ++ concatMap genModFuncs (mods s)) 
      ++ ((if cls then [] else [genInputFormat Primary, genInputDerived Primary, 
        genInputConstraints Primary]) ++ [genOutputFormat])) 
    ([genInputClass Auxiliary, genConstClass Auxiliary] 
      ++ map (fmap Just) (concatMap genModClasses $ mods s))
          
genModules :: (OOProg r) => Reader DrasilState [SFile r]
genModules = do
  g <- ask
  let s = csi $ codeSpec g
      mainIfExe Library = return []
      mainIfExe Program = sequence [genMain]
  mn     <- mainIfExe $ implType g
  inp    <- chooseInModule $ inMod g
  con    <- genConstMod 
  cal    <- genCalcMod
  out    <- genOutputMod
  moddef <- traverse genModDef (mods s) -- hack ?
  return $ mn ++ inp ++ con ++ cal : out ++ moddef

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"