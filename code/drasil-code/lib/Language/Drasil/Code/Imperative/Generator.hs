-- | Defines generation functions for SCS code packages.
module Language.Drasil.Code.Imperative.Generator (
  generator, generateCode
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.ConceptMatch (chooseConcept)
import Language.Drasil.Code.Imperative.Descriptions (unmodularDesc)
import Language.Drasil.Code.Imperative.SpaceMatch (chooseSpace)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genDoxConfig, genReadMe, genModuleWithImports)
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
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..),
  PackageSym(..), AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.Data (PackData(..), ad)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer(sampleInputName)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Code.ExtLibImport (auxMods, imports, modExports)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Choices (Choices(..), Modularity(..), Architecture(..),
  Visibility(..), DataInfo(..), Constraints(..), choicesSent, DocConfig(..),
  LogConfig(..), OptionalFeatures(..))
import Language.Drasil.CodeSpec (CodeSpec(..), getODE)
import Language.Drasil.Printers (Linearity(Linear), sentenceDoc)

import GOOL.Drasil (GSProgram, SFile, OOProg, ProgramSym(..), ScopeTag(..),
  ProgData(..), initialState, unCI)

import System.Directory (setCurrentDirectory, createDirectoryIfMissing,
  getCurrentDirectory)
import Control.Lens ((^.))
import Control.Monad.State (get, evalState, runState)
import Data.List (nub)
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
  modules = modules',
  extLibNames = nms,
  extLibMap = fromList elmap,
  libPaths = maybeToList pth,
  eMap = mem,
  libEMap = lem,
  clsMap = cdm,
  defList = nub $ keys mem ++ keys cdm,
  getVal = folderVal chs,
  -- stateful
  currentModule = "",
  currentClass = "",
  _designLog = des,
  _loggedSpaces = [] -- Used to prevent duplicate logs added to design log
}
  where (mcm, concLog) = runState (chooseConcept chs) []
        showDate Show = dt
        showDate Hide = ""
        ((pth, elmap, lname), libLog) = runState (chooseODELib l $ getODE $ extLibs chs) []
        els = map snd elmap
        nms = [lname]
        mem = modExportMap spec chs modules'
        lem = fromList (concatMap (^. modExports) els)
        cdm = clsDefMap spec chs modules'
        modules' = mods spec ++ concatMap (^. auxMods) els
        nonPrefChs = choicesSent chs
        des = vcat . map (sentenceDoc (sysinfodb spec) Implementation Linear) $
          (nonPrefChs ++ concLog ++ libLog)

-- | Generates a package with the given 'DrasilState'. The passed
-- un-representation functions determine which target language the package will
-- be generated in.
generateCode :: (OOProg progRepr, PackageSym packRepr) => Lang ->
  (progRepr (Program progRepr) -> ProgData) -> (packRepr (Package packRepr) ->
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
      as = case codeSpec g of CodeSpec {authors = a} -> map name a
      cfp = configFiles $ codeSpec g
      db = sysinfodb $ codeSpec g
      prps = show $ sentenceDoc db Implementation Linear
        (foldlSent $ purpose $ codeSpec g)
      bckgrnd = show $ sentenceDoc db Implementation Linear
        (foldlSent $ background $ codeSpec g)
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
        folderNum = getVal g,
        inputOutput = (sampleInputName, "output.txt")} -- This needs a more permanent solution
  return $ package pd (m:catMaybes [i,rm,d])

-- | Generates an SCS program based on the problem and the user's design choices.
genProgram :: (OOProg r) => GenState (GSProgram r)
genProgram = do
  g <- get
  ms <- chooseModules $ modular g
  let n = pName $ codeSpec g
  let p = show $ sentenceDoc (sysinfodb $ codeSpec g) Implementation Linear $ foldlSent $ purpose $ codeSpec g
  return $ prog n p ms

-- | Generates either a single module or many modules, based on the users choice
-- of modularity.
chooseModules :: (OOProg r) => Modularity -> GenState [SFile r]
chooseModules Unmodular = liftS genUnmodular
chooseModules (Modular _) = genModules

-- | Generates an entire SCS program as a single module.
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

-- | Generates all modules for an SCS program.
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

-- | Private utilities used in 'generateCode'.
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"
getDir Swift = "swift"
