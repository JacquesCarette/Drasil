module Language.Drasil.Code.Imperative.Generator (
  State(..), generator, generateCode
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.ConceptMatch (chooseConcept)
import Language.Drasil.Code.Imperative.GenerateGOOL (genDoxConfig)
import Language.Drasil.Code.Imperative.Import (genModDef)
import Language.Drasil.Code.Imperative.Modules (chooseInModule, genConstMod, 
  genMain, genOutputMod, genSampleInput)
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..))
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code (programName)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Choices(..), 
  Lang(..), Visibility(..))

import GOOL.Drasil (ProgramSym(..), RenderSym(..), ProgData(..), GOOLState)

import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Control.Monad.Reader (Reader, ask, runReader)
import qualified Control.Monad.State as S (State)

generator :: String -> [Expr] -> Choices -> CodeSpec -> State
generator dt sd chs spec = State {
  -- constants
  codeSpec = spec,
  date = showDate $ dates chs,
  inStruct = inputStructure chs,
  conStruct = constStructure chs,
  conRepr = constRepr chs,
  inMod = inputModule chs,
  logKind  = logging chs,
  commented = comments chs,
  doxOutput = doxVerbosity chs,
  concMatches = chooseConcept chs,
  auxiliaries = auxFiles chs,
  sampleData = sd,
  -- state
  currentModule = "",

  -- next depend on chs
  logName = logFile chs,
  onSfwrC = onSfwrConstraint chs,
  onPhysC = onPhysConstraint chs
}
  where showDate Show = dt
        showDate Hide = ""

generateCode :: (ProgramSym progRepr, PackageSym packRepr) => Lang -> 
  (progRepr (Program progRepr) -> S.State GOOLState ProgData) -> (packRepr (Package packRepr) -> 
  PackData) -> State -> IO ()
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
  (progRepr (Program progRepr) -> S.State GOOLState ProgData) -> 
  Reader State (packRepr (Package packRepr))
genPackage unRepr = do
  g <- ask
  p <- genProgram
  let pd = unRepr p
      n = case codeSpec g of CodeSpec {program = pr} -> programName pr
      m = makefile (commented g) pd
  i <- genSampleInput
  d <- genDoxConfig n pd
  return $ package pd (m:i++d)

genProgram :: (ProgramSym repr) => Reader State (repr (Program repr))
genProgram = do
  g <- ask
  ms <- genModules
  -- Below line of code cannot be simplified because program has a generic type
  let n = case codeSpec g of CodeSpec {program = p} -> programName p
  return $ prog n ms
          
genModules :: (RenderSym repr) => Reader State [repr (RenderFile repr)]
genModules = do
  g <- ask
  let s = csi $ codeSpec g
  mn     <- genMain
  inp    <- chooseInModule $ inMod g
  con    <- genConstMod 
  out    <- genOutputMod
  moddef <- traverse genModDef (mods s) -- hack ?
  return $ mn : inp ++ con ++ out ++ moddef

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"