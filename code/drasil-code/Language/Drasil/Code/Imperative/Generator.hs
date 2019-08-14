module Language.Drasil.Code.Imperative.Generator (
  State(..), generator, generateCode
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.GenerateGOOL (genDoxConfig)
import Language.Drasil.Code.Imperative.Import (genModDef)
import Language.Drasil.Code.Imperative.Modules (chooseInModule, genMain, 
  genOutputMod, genSampleInput)
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  ProgramSym(..), RenderSym(..), AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..), ProgData(..))
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code (programName)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Choices(..), 
  Lang(..), Visibility(..))

import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Control.Monad.Reader (Reader, ask, runReader)

generator :: String -> [Expr] -> Choices -> CodeSpec -> State
generator dt sd chs spec = State {
  -- constants
  codeSpec = spec,
  date = showDate $ dates chs,
  inStruct = inputStructure chs,
  inMod = inputModule chs,
  logKind  = logging chs,
  commented = comments chs,
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

generateCode :: (PackageSym repr) => Lang -> (repr (Package repr) -> PackData) 
  -> State -> IO ()
generateCode l unRepr g = do 
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False (getDir l)
  setCurrentDirectory (getDir l)
  createCodeFiles code
  setCurrentDirectory workingDir
  where pckg = runReader genPackage g
        code = makeCode (progMods $ packProg $ unRepr pckg) (packAux $ unRepr 
          pckg)

genPackage :: (PackageSym repr) => Reader State (repr (Package repr))
genPackage = do
  g <- ask
  p <- genProgram
  let n = case codeSpec g of CodeSpec {program = pr} -> programName pr
      m = makefile (commented g) p
  i <- genSampleInput
  d <- genDoxConfig n p
  return $ package p (m:i++d)

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
  out    <- genOutputMod
  moddef <- traverse genModDef (mods s) -- hack ?
  return $ mn : inp ++ out ++ moddef

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"