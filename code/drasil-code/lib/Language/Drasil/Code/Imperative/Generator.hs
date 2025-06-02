-- | Defines generation functions for SCS code packages.
module Language.Drasil.Code.Imperative.Generator (
  generator, generateCode, generateCodeProc
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.ConceptMatch (chooseConcept)
import Language.Drasil.Code.Imperative.Descriptions (unmodularDesc)
import Language.Drasil.Code.Imperative.SpaceMatch (chooseSpace)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genDoxConfig, genReadMe, genModuleWithImports, genModuleWithImportsProc)
import Language.Drasil.Code.Imperative.GenODE (chooseODELib)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (genModDef, genModDefProc,
  genModFuncs, genModFuncsProc, genModClasses)
import Language.Drasil.Code.Imperative.Modules (genInputMod, genInputModProc,
  genConstClass, genConstMod, checkConstClass, genInputClass,
  genInputConstraints, genInputConstraintsProc, genInputDerived,
  genInputDerivedProc, genInputFormat, genInputFormatProc, genMain, genMainProc,
  genMainFunc, genMainFuncProc, genCalcMod, genCalcModProc, genCalcFunc,
  genCalcFuncProc, genOutputFormat, genOutputFormatProc, genOutputMod,
  genOutputModProc, genSampleInput)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..),
  ScopeType(..), designLog, modExportMap, clsDefMap, genICName)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), AuxiliarySym(..))
import Language.Drasil.Code.Imperative.ReadMe.Import (ReadMeInfo(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..), ad)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer(sampleInputName)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Code.ExtLibImport (auxMods, imports, modExports)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Choices (Choices(..), Modularity(..), Architecture(..),
  Visibility(..), DataInfo(..), Constraints(..), choicesSent, DocConfig(..),
  LogConfig(..), OptionalFeatures(..), InternalConcept(..))
import Language.Drasil.CodeSpec (CodeSpec(..), HasOldCodeSpec(..), getODE)
import Language.Drasil.Printers (SingleLine(OneLine), sentenceDoc)

import Drasil.GOOL (OOProg, VisibilityTag(..),
  ProgData(..), initialState)
import qualified Drasil.GOOL as OO (GSProgram, SFile, ProgramSym(..), unCI)
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as Proc (GSProgram, SFile, ProgramSym(..), unCI)
import System.Drasil hiding (systemdb)

import System.Directory (setCurrentDirectory, createDirectoryIfMissing,
  getCurrentDirectory)
import Control.Lens ((^.))
import Control.Monad.State (get, evalState, runState)
import qualified Data.Set as Set (fromList)
import Data.Map (fromList, member, keys, elems)
import Data.Maybe (maybeToList, catMaybes)
import Text.PrettyPrint.HughesPJ (isEmpty, vcat)

-- | Initializes the generator's 'DrasilState'.
-- 'String' parameter is a string representing the date.
-- \['Expr'\] parameter is the sample input values provided by the user.
generator :: Lang -> String -> [Expr] -> Choices -> CodeSpec -> DrasilState
generator l dt sd chs spec = DrasilState {
  -- constants
  codeSpec = spec,
  modular = modularity $ architecture chs,
  inStruct = inputStructure $ dataInfo chs,
  conStruct = constStructure $ dataInfo chs,
  conRepr = constRepr $ dataInfo chs,
  concMatches = mcm,
  spaceMatches = chooseSpace l chs,
  implType = impType $ architecture chs,
  onSfwrC = onSfwrConstraint $ srsConstraints chs,
  onPhysC = onPhysConstraint $ srsConstraints chs,
  commented = comments $ docConfig $ optFeats chs,
  doxOutput = doxVerbosity $ docConfig $ optFeats chs,
  date = showDate $ dates $ docConfig $ optFeats chs,
  logKind  = logging $ logConfig $ optFeats chs,
  logName = logFile $ logConfig $ optFeats chs,
  auxiliaries = auxFiles $ optFeats chs,
  sampleData = sd,
  dsICNames = icNames chs,
  modules = modules',
  extLibNames = nms,
  extLibMap = fromList elmap,
  libPaths = maybeToList pth,
  eMap = mem,
  libEMap = lem,
  clsMap = cdm,
  defSet = Set.fromList $ keys mem ++ keys cdm,
  getVal = folderVal chs,
  -- stateful
  currentModule = "",
  currentClass = "",
  _designLog = des,
  _loggedSpaces = [], -- Used to prevent duplicate logs added to design log
  currentScope = Global
}
  where (mcm, concLog) = runState (chooseConcept chs) []
        showDate Show = dt
        showDate Hide = ""
        ((pth, elmap, lname), libLog) = runState (chooseODELib l $ getODE $ extLibs chs) []
        els = map snd elmap
        nms = [lname]
        mem = modExportMap (spec ^. oldCodeSpec) chs modules'
        lem = fromList (concatMap (^. modExports) els)
        cdm = clsDefMap (spec ^. oldCodeSpec) chs modules'
        modules' = (spec ^. modsO) ++ concatMap (^. auxMods) els
        nonPrefChs = choicesSent chs
        des = vcat . map (sentenceDoc (spec ^. systemdbO) Implementation OneLine) $
          (nonPrefChs ++ concLog ++ libLog)

-- OO Versions --

-- | Generates a package with the given 'DrasilState'. The passed
-- un-representation functions determine which target language the package will
-- be generated in.
generateCode :: (OOProg progRepr, PackageSym packRepr) => Lang ->
  (progRepr (OO.Program progRepr) -> ProgData) -> (packRepr (Package packRepr) ->
  PackData) -> DrasilState -> IO ()
generateCode l unReprProg unReprPack g = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False (getDir l)
  setCurrentDirectory (getDir l)
  let (pckg, ds) = runState (genPackage unReprProg) g
      baseAux = [ad "designLog.txt" (ds ^. designLog) | not $ isEmpty $
          ds ^. designLog] ++ packAux (unReprPack pckg)
      aux
        | l == Python = ad "__init__.py" mempty : baseAux
        | otherwise   = baseAux
      code = makeCode (progMods $ packProg $ unReprPack pckg) aux
  createCodeFiles code
  setCurrentDirectory workingDir

-- | Generates a package, including a Makefile, sample input file, and Doxygen
-- configuration file (all subject to the user's choices).
-- The passed un-representation function determines which target language the
-- package will be generated in.
-- GOOL's static code analysis interpreter is called to initialize the state
-- used by the language renderer.
genPackage :: (OOProg progRepr, PackageSym packRepr) =>
  (progRepr (OO.Program progRepr) -> ProgData) ->
  GenState (packRepr (Package packRepr))
genPackage unRepr = do
  g <- get
  ci <- genProgram
  p <- genProgram
  let info = OO.unCI $ evalState ci initialState
      (reprPD, s) = runState p info
      pd = unRepr reprPD
      m = makefile (libPaths g) (implType g) (commented g) s pd
      as = map name (codeSpec g ^. authorsO)
      cfp = codeSpec g ^. configFilesO
      db = codeSpec g ^. systemdbO
      -- prps = show $ sentenceDoc db Implementation OneLine
      --   (foldlSent $ purpose $ codeSpec g)
      prps = show $ sentenceDoc db Implementation OneLine 
        (foldlSent $ codeSpec g ^. purpose)  
      bckgrnd = show $ sentenceDoc db Implementation OneLine
        (foldlSent $ codeSpec g ^. background)
      mtvtn = show $ sentenceDoc db Implementation OneLine
        (foldlSent $ codeSpec g ^. motivation)
      scp = show $ sentenceDoc db Implementation OneLine
        (foldlSent $ codeSpec g ^. scope)
  i <- genSampleInput
  d <- genDoxConfig s
  rm <- genReadMe ReadMeInfo {
        langName = "",
        langVersion = "",
        invalidOS = Nothing,
        implementType = implType g,
        extLibNV = extLibNames g,
        extLibFP = libPaths g,
        contributors = as,
        configFP = cfp,
        caseName = "",
        examplePurpose = prps,
        exampleDescr = bckgrnd,
        exampleMotivation = mtvtn,
        exampleScope = scp,
        folderNum = getVal g,
        inputOutput = (sampleInputName, "output.txt")} -- This needs a more permanent solution
  return $ package pd (m:catMaybes [i,rm,d])

-- | Generates an SCS program based on the problem and the user's design choices.
genProgram :: (OOProg r) => GenState (OO.GSProgram r)
genProgram = do
  g <- get
  ms <- chooseModules $ modular g
  let n = codeSpec g ^. pNameO 
  let p = show $ sentenceDoc (codeSpec g ^. systemdbO) Implementation OneLine $ foldlSent $ codeSpec g ^. purpose
  return $ OO.prog n p ms

-- | Generates either a single module or many modules, based on the users choice
-- of modularity.
chooseModules :: (OOProg r) => Modularity -> GenState [OO.SFile r]
chooseModules Unmodular = liftS genUnmodular
chooseModules Modular = genModules

-- | Generates an entire SCS program as a single module.
genUnmodular :: (OOProg r) => GenState (OO.SFile r)
genUnmodular = do
  g <- get
  umDesc <- unmodularDesc
  giName <- genICName GetInput
  dvName <- genICName DerivedValuesFn
  icName <- genICName InputConstraintsFn
  let n = codeSpec g ^. pNameO
      cls = any (`member` clsMap g) [giName, dvName, icName]
  genModuleWithImports n umDesc (concatMap (^. imports) (elems $ extLibMap g))
    (genMainFunc
      : map (fmap Just) (map genCalcFunc (codeSpec g ^. execOrderO)
        ++ concatMap genModFuncs (modules g))
      ++ ((if cls then [] else [genInputFormat Pub, genInputDerived Pub,
        genInputConstraints Pub]) ++ [genOutputFormat]))
    ([genInputClass Auxiliary, genConstClass Auxiliary]
      ++ map (fmap Just) (concatMap genModClasses $ modules g))

-- | Generates all modules for an SCS program.
genModules :: (OOProg r) => GenState [OO.SFile r]
genModules = do
  g <- get
  mn     <- genMain
  inp    <- genInputMod
  con    <- genConstMod
  cal    <- genCalcMod
  out    <- genOutputMod
  moddef <- traverse genModDef (modules g) -- hack ?
  return $ mn : inp ++ con ++ cal : out ++ moddef

-- Procedural Versions --
-- | Generates a package with the given 'DrasilState'. The passed
-- un-representation functions determine which target language the package will
-- be generated in.
generateCodeProc :: (ProcProg progRepr, PackageSym packRepr) => Lang ->
  (progRepr (Proc.Program progRepr) -> ProgData) -> (packRepr (Package packRepr) ->
  PackData) -> DrasilState -> IO ()
generateCodeProc l unReprProg unReprPack g = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False (getDir l)
  setCurrentDirectory (getDir l)
  let (pckg, ds) = runState (genPackageProc unReprProg) g
      baseAux = [ad "designLog.txt" (ds ^. designLog) | not $ isEmpty $
          ds ^. designLog] ++ packAux (unReprPack pckg)
      code = makeCode (progMods $ packProg $ unReprPack pckg) baseAux
  createCodeFiles code
  setCurrentDirectory workingDir

-- | Generates a package, including a Makefile, sample input file, and Doxygen
-- configuration file (all subject to the user's choices).
-- The passed un-representation function determines which target language the
-- package will be generated in.
-- GOOL's static code analysis interpreter is called to initialize the state
-- used by the language renderer.
genPackageProc :: (ProcProg progRepr, PackageSym packRepr) =>
  (progRepr (Proc.Program progRepr) -> ProgData) ->
  GenState (packRepr (Package packRepr))
genPackageProc unRepr = do
  g <- get
  ci <- genProgramProc
  p <- genProgramProc
  let info = Proc.unCI $ evalState ci initialState
      (reprPD, s) = runState p info
      pd = unRepr reprPD
      m = makefile (libPaths g) (implType g) (commented g) s pd
      as = map name (codeSpec g ^. authorsO)
      cfp = codeSpec g ^. configFilesO
      db = codeSpec g ^. systemdbO
      prps = show $ sentenceDoc db Implementation OneLine
        (foldlSent $ codeSpec g ^. purpose)
      bckgrnd = show $ sentenceDoc db Implementation OneLine
        (foldlSent $ codeSpec g ^. background)
      mtvtn = show $ sentenceDoc db Implementation OneLine
        (foldlSent $ codeSpec g ^. motivation)
      scp = show $ sentenceDoc db Implementation OneLine
        (foldlSent $ codeSpec g ^. scope)
  i <- genSampleInput
  d <- genDoxConfig s
  rm <- genReadMe ReadMeInfo {
        langName = "",
        langVersion = "",
        invalidOS = Nothing,
        implementType = implType g,
        extLibNV = extLibNames g,
        extLibFP = libPaths g,
        contributors = as,
        configFP = cfp,
        caseName = "",
        examplePurpose = prps,
        exampleDescr = bckgrnd,
        exampleMotivation = mtvtn,
        exampleScope = scp,
        folderNum = getVal g,
        inputOutput = (sampleInputName, "output.txt")} -- This needs a more permanent solution
  return $ package pd (m:catMaybes [i,rm,d])

-- | Generates an SCS program based on the problem and the user's design choices.
genProgramProc :: (ProcProg r) => GenState (Proc.GSProgram r)
genProgramProc = do
  g <- get
  ms <- chooseModulesProc $ modular g
  let n = codeSpec g ^. pNameO
  let p = show $ sentenceDoc (codeSpec g ^. systemdbO) Implementation OneLine $ foldlSent $ codeSpec g ^. purpose
  return $ Proc.prog n p ms

-- | Generates either a single module or many modules, based on the users choice
-- of modularity.
chooseModulesProc :: (ProcProg r) => Modularity -> GenState [Proc.SFile r]
chooseModulesProc Unmodular = liftS genUnmodularProc
chooseModulesProc Modular = genModulesProc

-- | Generates an entire SCS program as a single module.
genUnmodularProc :: (ProcProg r) => GenState (Proc.SFile r)
genUnmodularProc = do
  g <- get
  umDesc <- unmodularDesc
  giName <- genICName GetInput
  dvName <- genICName DerivedValuesFn
  icName <- genICName InputConstraintsFn
  let n = codeSpec g ^. pNameO
      cls = any (`member` clsMap g) [giName, dvName, icName]
  if cls then error "genUnmodularProc: Procedural renderers do not support classes"
  else genModuleWithImportsProc n umDesc (concatMap (^. imports) (elems $ extLibMap g))
        (genMainFuncProc
          : map (fmap Just) (map genCalcFuncProc (codeSpec g ^. execOrderO)
            ++ concatMap genModFuncsProc (modules g))
          ++ ([genInputFormatProc Pub, genInputDerivedProc Pub,
              genInputConstraintsProc Pub] ++ [genOutputFormatProc]))

-- | Generates all modules for an SCS program.
genModulesProc :: (ProcProg r) => GenState [Proc.SFile r]
genModulesProc = do
  g <- get
  mn     <- genMainProc
  inp    <- genInputModProc
  con    <- checkConstClass
  cal    <- genCalcModProc
  out    <- genOutputModProc
  moddef <- traverse genModDefProc (modules g) -- hack ?
  if con then error "genModulesProc: Procedural renderers do not support classes"
  else return $ mn : inp ++ cal : out ++ moddef

-- | Private utilities used in 'generateCode'.
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"
getDir Swift = "swift"
getDir Julia = "julia"
