{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import (generator, generateCode) where

import Utils.Drasil (stringList)

import Language.Drasil hiding (int, ($.), log, ln, exp,
  sin, cos, tan, csc, sec, cot, arcsin, arccos, arctan)
import Database.Drasil (ChunkDB, symbResolve)
import Language.Drasil.Code.Code as C (CodeType(List, Object))
import Language.Drasil.Code.Imperative.Symantics (Label, PackageSym(..), 
  ProgramSym(..), RenderSym(..), AuxiliarySym(..), PermanenceSym(..), 
  BodySym(..), BlockSym(..), StateTypeSym(..), VariableSym(..), ValueSym(..),
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  FunctionSym(..), SelectorFunction(..), StatementSym(..), 
  ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..), ParameterSym(..),
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.Data (PackData(..), ProgData(..))
import Language.Drasil.Code.Imperative.Helpers (convType)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code (CodeChunk, CodeIdea(codeName, codeChunk),
  codeType, codevar, quantvar, quantfunc, physLookup, sfwrLookup, programName)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams)
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))
import qualified Language.Drasil.CodeSpec as CS (Mod(..))
import Language.Drasil.Code.DataDesc (DataItem, LinePattern(Repeat, Straight), 
  Data(Line, Lines, JunkData, Singleton), DataDesc, isLine, isLines, getInputs,
  getPatternInputs, junkLine, singleton)
import Language.Drasil.Printers (Linearity(Linear), exprDoc, sentenceDoc, 
  unitDoc)

import Prelude hiding (sin, cos, tan, log, exp, const, print)
import Data.List (nub, intersperse, intercalate, (\\))
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Data.Map (Map, member)
import qualified Data.Map as Map (lookup, elems)
import Data.Maybe (fromMaybe, maybe, maybeToList, catMaybes, mapMaybe)
import Control.Applicative ((<$>))
import Control.Monad (liftM2,liftM3)
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Control.Lens ((^.), view)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), empty, parens, render, text)

-- Private State, used to push these options around the generator
data State = State {
  codeSpec :: CodeSpec,
  date :: String,
  inStruct :: Structure,
  inMod :: InputModule,
  logName :: String,
  logKind :: Logging,
  commented :: [Comments],
  currentModule :: String,

  onSfwrC :: ConstraintBehaviour,
  onPhysC :: ConstraintBehaviour
}

sfwrCBody :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr) 
  => [(q,[Constraint])] -> Reader State [repr (Statement repr)]
sfwrCBody cs = do
  g <- ask
  let cb = onSfwrC g
  chooseConstr cb cs

physCBody :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr) 
  => [(q,[Constraint])] -> Reader State [repr (Statement repr)]
physCBody cs = do
  g <- ask
  let cb = onPhysC g
  chooseConstr cb cs

-- function to choose how to deal with
-- 1. constraints
-- 2. how to structure the input "module"
-- 3. logging assignments
chooseConstr :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, 
  RenderSym repr) => ConstraintBehaviour -> [(q,[Constraint])] -> 
  Reader State [repr (Statement repr)]
chooseConstr Warning   cs = do
  checks <- mapM constrWarn cs
  return $ concat checks
chooseConstr Exception cs = do
  checks <- mapM constrExc cs
  return $ concat checks

chooseInModule :: (RenderSym repr) => InputModule -> Reader State 
  [repr (RenderFile repr)]
chooseInModule Combined = genInputModCombined
chooseInModule Separated = genInputModSeparated

chooseInStructure :: (RenderSym repr) => Structure -> Reader State 
  (Maybe (repr (Class repr)))
chooseInStructure Unbundled = return Nothing
chooseInStructure Bundled = genInputClass

chooseLogging :: (RenderSym repr) => Logging -> (repr (Variable repr) -> 
  Reader State (Maybe (repr (Statement repr))))
chooseLogging LogVar v = Just <$> loggedVar v
chooseLogging LogAll v = Just <$> loggedVar v
chooseLogging _      _ = return Nothing

initLogFileVar :: (RenderSym repr) => Logging -> [repr (Statement repr)]
initLogFileVar LogVar = [varDec varLogFile]
initLogFileVar LogAll = [varDec varLogFile]
initLogFileVar _ = []

varLogFile :: (RenderSym repr) => repr (Variable repr)
varLogFile = var "outfile" outfile

valLogFile :: (RenderSym repr) => repr (Value repr)
valLogFile = valueOf varLogFile

generator :: String -> Choices -> CodeSpec -> State
generator dt chs spec = State {
  -- constants
  codeSpec = spec,
  date = showDate $ dates chs,
  inStruct = inputStructure chs,
  inMod = inputModule chs,
  logKind  = logging chs,
  commented = comments chs,
  -- state
  currentModule = "",

  -- next depend on chs
  logName = logFile chs,
  onSfwrC = onSfwrConstraint chs,
  onPhysC = onPhysConstraint chs
}
  where showDate Show = dt
        showDate Hide = ""

maybeLog :: (RenderSym repr) => repr (Variable repr) ->
  Reader State [repr (Statement repr)]
maybeLog v = do
  g <- ask
  l <- chooseLogging (logKind g) v
  return $ maybeToList l

publicMethod :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (MethodType repr) -> Label -> String -> [c] -> Maybe String -> 
  [repr (Block repr)] -> Reader State (repr (Method repr))
publicMethod = genMethodCall public static_

publicInOutFunc :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  Label -> String -> [c] -> [c] -> [c] -> [repr (Block repr)] -> 
  Reader State (repr (Method repr))
publicInOutFunc = genInOutFunc public static_

publicClass :: (RenderSym repr) => String -> Label -> Maybe Label -> 
  [repr (StateVar repr)] -> [repr (Method repr)] -> 
  Reader State (repr (Class repr))
publicClass desc n l vs ms = do
  g <- ask
  return $ if CommentClass `elem` commented g 
    then docClass desc (pubClass n l vs ms) 
    else pubClass n l vs ms

generateCode :: (PackageSym repr) => Lang -> (repr (Package repr) -> PackData) 
  -> State -> IO ()
generateCode l unRepr g =
  do workingDir <- getCurrentDirectory
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
  d <- genDoxConfig n p
  return $ package p (m:d)

genDoxConfig :: (AuxiliarySym repr) => String -> repr (Program repr) ->
  Reader State [repr (Auxiliary repr)]
genDoxConfig n p = do
  g <- ask
  let cms = commented g
  return [doxConfig n p | not (null cms)]

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

liftS :: Reader a b -> Reader a [b]
liftS = fmap (: [])

getTermDoc :: (NamedIdea c) => UID -> Map UID c -> Reader State Doc
getTermDoc cname m = do
  g <- ask
  let db = sysinfodb $ csi $ codeSpec g
  return $ (maybe (text "No description given") (sentenceDoc db 
    Implementation Linear . phraseNP . view term) . Map.lookup cname) m

getUnitsDoc :: (MayHaveUnit c) => UID -> Map UID c -> Doc
getUnitsDoc cname m = maybe empty (parens . unitDoc Linear . usymb) 
  (Map.lookup cname m >>= getUnit)

----- Descriptions -----

modDesc :: Reader State [String] -> Reader State String
modDesc = fmap ((++) "Provides " . stringList)

inputParametersDesc :: Reader State [String]
inputParametersDesc = do
  g <- ask
  ifDesc <- inputFormatDesc
  dvDesc <- derivedValuesDesc
  icDesc <- inputConstraintsDesc
  let im = inMod g
      st = inStruct g
      ipDesc Separated = inDesc st
      ipDesc Combined = inDesc st ++ [ifDesc, dvDesc, icDesc]
      inDesc Bundled = ["the structure for holding input values"]
      inDesc Unbundled = [""]
  return $ ipDesc im

inputFormatDesc :: Reader State String
inputFormatDesc = do
  g <- ask
  let ifDesc Nothing = ""
      ifDesc _ = "the function for reading inputs"
  return $ ifDesc $ Map.lookup "get_input" (eMap $ codeSpec g)

derivedValuesDesc :: Reader State String
derivedValuesDesc = do
  g <- ask
  let dvDesc Nothing = ""
      dvDesc _ = "the function for calculating derived values"
  return $ dvDesc $ Map.lookup "derived_values" (eMap $ codeSpec g)

inputConstraintsDesc :: Reader State String
inputConstraintsDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc Nothing = ""
      icDesc _ = "the function for checking the " ++ pAndS ++ 
        " on the input"
  return $ icDesc $ Map.lookup "input_constraints" (eMap $ codeSpec g)

outputFormatDesc :: Reader State String
outputFormatDesc = do
  g <- ask
  let ofDesc Nothing = ""
      ofDesc _ = "the function for writing outputs"
  return $ ofDesc $ Map.lookup "write_output" (eMap $ codeSpec g)

inputClassDesc :: Reader State String
inputClassDesc = do
  g <- ask
  let inClassD [] = ""
      inClassD _ = "Structure for holding the " ++ stringList [
        inPs $ extInputs $ csi $ codeSpec g,
        dVs $ Map.lookup "derived_values" (eMap $ codeSpec g)]
      inPs [] = ""
      inPs _ = "input values"
      dVs Nothing = ""
      dVs _ = "derived values"
  return $ inClassD $ inputs $ csi $ codeSpec g

inFmtFuncDesc :: Reader State String
inFmtFuncDesc = do
  g <- ask
  let ifDesc Nothing = ""
      ifDesc _ = "Reads input from a file with the given file name"
  return $ ifDesc $ Map.lookup "get_input" (eMap $ codeSpec g)

inConsFuncDesc :: Reader State String
inConsFuncDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc Nothing = ""
      icDesc _ = "Verifies that input values satisfy the " ++ pAndS
  return $ icDesc $ Map.lookup "input_constraints" (eMap $ codeSpec g)

dvFuncDesc :: Reader State String
dvFuncDesc = do
  g <- ask
  let dvDesc Nothing = ""
      dvDesc _ = "Calculates values that can be immediately derived from the" ++
        " inputs"
  return $ dvDesc $ Map.lookup "derived_values" (eMap $ codeSpec g)

woFuncDesc :: Reader State String
woFuncDesc = do
  g <- ask
  let woDesc Nothing = ""
      woDesc _ = "Writes the output values to output.txt"
  return $ woDesc $ Map.lookup "write_output" (eMap $ codeSpec g)

physAndSfwrCons :: Reader State String
physAndSfwrCons = do
  g <- ask
  let cns = concat $ Map.elems (cMap $ csi $ codeSpec g)
  return $ stringList [
    if null (map isPhysC cns) then "" else "physical constraints",
    if null (map isSfwrC cns) then "" else "software constraints"]

------- INPUT ----------

genInputModSeparated :: (RenderSym repr) => 
  Reader State [repr (RenderFile repr)]
genInputModSeparated = do
  g <- ask
  ipDesc <- modDesc inputParametersDesc
  ifDesc <- modDesc (liftS inputFormatDesc)
  dvDesc <- modDesc (liftS derivedValuesDesc)
  icDesc <- modDesc (liftS inputConstraintsDesc)
  sequence 
    [genModule "InputParameters" ipDesc 
      Nothing (Just $ fmap maybeToList (chooseInStructure $ inStruct g)),
    genModule "InputFormat" ifDesc
      (Just $ fmap maybeToList genInputFormat) Nothing,
    genModule "DerivedValues" dvDesc
      (Just $ fmap maybeToList genInputDerived) Nothing,
    genModule "InputConstraints" icDesc 
      (Just $ fmap maybeToList genInputConstraints) Nothing]

genInputModCombined :: (RenderSym repr) => Reader State [repr (RenderFile repr)]
genInputModCombined = do
  g <- ask
  ipDesc <- modDesc inputParametersDesc
  liftS $ genModule "InputParameters" ipDesc
    (Just $ concat <$> mapM (fmap maybeToList) 
    [genInputFormat, genInputDerived, genInputConstraints]) 
    (Just $ fmap maybeToList (chooseInStructure $ inStruct g))

genInputClass :: (RenderSym repr) => Reader State (Maybe (repr (Class repr)))
genInputClass = do
  g <- ask
  let ins       = inputs $ csi $ codeSpec g
      genClass :: (RenderSym repr) => [String] -> Reader State (Maybe 
        (repr (Class repr)))
      genClass [] = return Nothing 
      genClass _ = do
        let inputVars = map (\x -> pubMVar 0 (var (codeName x) (convType $ 
              codeType x))) ins
        icDesc <- inputClassDesc
        cls <- publicClass icDesc "InputParameters" Nothing inputVars []
        return $ Just cls
  genClass $ mapMaybe (\x -> Map.lookup (codeName x) (eMap $ codeSpec g)) ins

genInputConstraints :: (RenderSym repr) => Reader State 
  (Maybe (repr (Method repr)))
genInputConstraints = do
  g <- ask
  let cm = cMap $ csi $ codeSpec g
      genConstraints :: (RenderSym repr) => Maybe String -> Reader State 
        (Maybe (repr (Method repr)))
      genConstraints Nothing = return Nothing
      genConstraints (Just _) = do
        h <- ask
        parms <- getConstraintParams
        let varsList = filter (\i -> member (i ^. uid) cm) (inputs $ csi $ 
              codeSpec h)
            sfwrCs   = map (sfwrLookup cm) varsList
            physCs   = map (physLookup cm) varsList
        sf <- sfwrCBody sfwrCs
        hw <- physCBody physCs
        desc <- inConsFuncDesc
        mthd <- publicMethod (mState void) "input_constraints" desc parms 
          Nothing [block sf, block hw]
        return $ Just mthd
  genConstraints $ Map.lookup "input_constraints" (eMap $ codeSpec g)

genInputDerived :: (RenderSym repr) => Reader State 
  (Maybe (repr (Method repr)))
genInputDerived = do
  g <- ask
  let dvals = derivedInputs $ csi $ codeSpec g
      genDerived :: (RenderSym repr) => Maybe String -> Reader State 
        (Maybe (repr (Method repr)))
      genDerived Nothing = return Nothing
      genDerived (Just _) = do
        ins <- getDerivedIns
        outs <- getDerivedOuts
        bod <- mapM (\x -> genCalcBlock CalcAssign x (codeEquat x)) dvals
        desc <- dvFuncDesc
        mthd <- publicInOutFunc "derived_values" desc ins outs [] bod
        return $ Just mthd
  genDerived $ Map.lookup "derived_values" (eMap $ codeSpec g)

constrWarn :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr)
  => (q,[Constraint]) -> Reader State [repr (Statement repr)]
constrWarn c = do
  let q = fst c
      cs = snd c
  conds <- mapM (convExpr . renderC q) cs
  msgs <- mapM (constraintViolatedMsg q "suggested") cs
  return $ zipWith (\cond m -> ifNoElse [((?!) cond, bodyStatements $
    printStr "Warning: " : m)]) conds msgs

constrExc :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr) 
  => (q,[Constraint]) -> Reader State [repr (Statement repr)]
constrExc c = do
  let q = fst c
      cs = snd c
  conds <- mapM (convExpr . renderC q) cs
  msgs <- mapM (constraintViolatedMsg q "expected") cs
  return $ zipWith (\cond m -> ifNoElse [((?!) cond, bodyStatements $ 
    m ++ [throw "InputError"])]) conds msgs

constraintViolatedMsg :: (CodeIdea q, HasUID q, HasCodeType q, RenderSym repr) 
  => q -> String -> Constraint -> Reader State [repr (Statement repr)]
constraintViolatedMsg q s c = do
  pc <- printConstraint c 
  v <- mkVal q
  return $ [printStr $ codeName q ++ " has value ",
    print v,
    printStr $ " but " ++ s ++ " to be "] ++ pc

printConstraint :: (RenderSym repr) => Constraint -> 
  Reader State [repr (Statement repr)]
printConstraint c = do
  g <- ask
  let db = sysinfodb $ csi $ codeSpec g
      printConstraint' :: (RenderSym repr) => Constraint -> Reader State 
        [repr (Statement repr)]
      printConstraint' (Range _ (Bounded (_,e1) (_,e2))) = do
        lb <- convExpr e1
        ub <- convExpr e2
        return $ [printStr "between ",
          print lb] ++ printExpr e1 db ++
          [printStr " and ", print ub] ++ printExpr e2 db ++ [printStrLn "."]
      printConstraint' (Range _ (UpTo (_,e))) = do
        ub <- convExpr e
        return $ [printStr "below ",
          print ub] ++ printExpr e db ++ [printStrLn "."]
      printConstraint' (Range _ (UpFrom (_,e))) = do
        lb <- convExpr e
        return $ [printStr "above ",
          print lb] ++ printExpr e db ++ [printStrLn "."]
      printConstraint' (EnumeratedReal _ ds) = return [
        printStrLn $ "one of: " ++ intercalate ", " (map show ds)]
      printConstraint' (EnumeratedStr _ ss) = return [
        printStrLn $ "one of: " ++ intercalate ", " ss]
  printConstraint' c

printExpr :: (RenderSym repr) => Expr -> ChunkDB -> [repr (Statement repr)]
printExpr (Dbl _) _ = []
printExpr (Int _) _ = []
printExpr e db = [printStr $ " (" ++ render (exprDoc db Implementation Linear e)
  ++ ")"]

genInputFormat :: (RenderSym repr) => Reader State 
  (Maybe (repr (Method repr)))
genInputFormat = do
  g <- ask
  let dd = junkLine : intersperse junkLine (map singleton (extInputs $ csi $
        codeSpec g))
      genInFormat :: (RenderSym repr) => Maybe String -> Reader State 
        (Maybe (repr (Method repr)))
      genInFormat Nothing = return Nothing
      genInFormat (Just _) = do
        ins <- getInputFormatIns
        outs <- getInputFormatOuts
        bod <- readData dd
        desc <- inFmtFuncDesc
        mthd <- publicInOutFunc "get_input" desc ins outs [] bod
        return $ Just mthd
  genInFormat $ Map.lookup "get_input" (eMap $ codeSpec g)

---- CONST ----

{-
genConstMod :: Reader State Module
genConstMod = buildModule "Constants" []
  (map (\x -> VarDecDef (codeName x) (convType $ codeType x) (convExpr $ codeEquat x)) (const $ codeSpec g))
  [] [{- genConstClassD g -}]

genConstClassD :: Reader State Class
genConstClassD = pubClass "Constants" Nothing genVars []
  where genVars = map (\x -> pubGVar 0 (convType $ codeType x) (codeName x)) (const $ codeSpec g)
-}

------- CALC ----------
{-
genCalcMod :: String -> [CodeDefinition] -> Reader State Module
genCalcMod n defs = buildModule n [] [] (map genCalcFunc (filter (validExpr . codeEquat) defs)) []
-}
genCalcFunc :: (RenderSym repr) => CodeDefinition -> Reader State (repr
  (Method repr))
genCalcFunc cdef = do
  parms <- getCalcParams cdef
  let nm = codeName cdef
      tp = convType $ codeType cdef
  blck <- genCalcBlock CalcReturn cdef (codeEquat cdef)
  desc <- returnComment $ cdef ^. uid
  publicMethod
    (mState tp)
    nm
    ("Calculates " ++ desc)
    parms
    (Just desc)
    [blck]

data CalcType = CalcAssign | CalcReturn deriving Eq

genCalcBlock :: (RenderSym repr) => CalcType -> CodeDefinition -> Expr ->
  Reader State (repr (Block repr))
genCalcBlock t v (Case c e) = genCaseBlock t v c e
genCalcBlock t v e
    | t == CalcAssign  = fmap block $ liftS $ do { vv <- mkVar v; ee <-
      convExpr e; l <- maybeLog vv; return $ multi $ assign vv ee : l}
    | otherwise        = block <$> liftS (returnState <$> convExpr e)

genCaseBlock :: (RenderSym repr) => CalcType -> CodeDefinition -> Completeness 
  -> [(Expr,Relation)] -> Reader State (repr (Block repr))
genCaseBlock _ _ _ [] = error $ "Case expression with no cases encountered" ++
  " in code generator"
genCaseBlock t v c cs = do
  ifs <- mapM (\(e,r) -> liftM2 (,) (convExpr r) (calcBody e)) (ifEs c)
  els <- elseE c
  return $ block [ifCond ifs els]
  where calcBody e = fmap body $ liftS $ genCalcBlock t v e
        ifEs Complete = init cs
        ifEs Incomplete = cs
        elseE Complete = calcBody $ fst $ last cs
        elseE Incomplete = return $ oneLiner $ throw $  
          "Undefined case encountered in function " ++ codeName v

----- OUTPUT -------

genOutputMod :: (RenderSym repr) => Reader State [repr (RenderFile repr)]
genOutputMod = do
  outformat <- genOutputFormat
  ofDesc <- modDesc $ liftS outputFormatDesc
  let outf = maybeToList outformat
  liftS $ genModule "OutputFormat" ofDesc
    (Just $ return outf) Nothing

genOutputFormat :: (RenderSym repr) => Reader State (Maybe (repr (Method repr)))
genOutputFormat = do
  g <- ask
  let genOutput :: (RenderSym repr) => Maybe String -> Reader State 
        (Maybe (repr (Method repr)))
      genOutput Nothing = return Nothing
      genOutput (Just _) = do
        let l_outfile = "outputfile"
            var_outfile = var l_outfile outfile
            v_outfile = valueOf var_outfile
        parms <- getOutputParams
        outp <- mapM (\x -> do
          v <- mkVal x
          return [ printFileStr v_outfile (codeName x ++ " = "),
                   printFileLn v_outfile v
                 ] ) (outputs $ csi $ codeSpec g)
        desc <- woFuncDesc
        mthd <- publicMethod (mState void) "write_output" desc parms Nothing 
          [block $ [
          varDec var_outfile,
          openFileW var_outfile (litString "output.txt") ] ++
          concat outp ++ [ closeFile v_outfile ]]
        return $ Just mthd
  genOutput $ Map.lookup "write_output" (eMap $ codeSpec g)

-----

genMethodCall :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (Scope repr) -> repr (Permanence repr) -> repr (MethodType repr) -> 
  Label -> String -> [c] -> Maybe String -> [repr (Block repr)] -> 
  Reader State (repr (Method repr))
genMethodCall s pr t n desc p r b = do
  g <- ask
  vars <- mapM mkVar p
  let ps = map mkParam vars
      doLog = logKind g
      loggedBody LogFunc = loggedMethod (logName g) n vars b
      loggedBody LogAll  = loggedMethod (logName g) n vars b
      loggedBody _       = b
      bod = body $ loggedBody doLog
      fn = function n s pr t ps bod
  pComms <- mapM (paramComment . (^. uid)) p
  return $ if CommentFunc `elem` commented g
    then docFunc desc pComms r fn else fn

genInOutFunc :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (Scope repr) -> repr (Permanence repr) -> Label -> String -> [c] ->
  [c] -> [c] -> [repr (Block repr)] -> Reader State (repr (Method repr))
genInOutFunc s pr n desc ins outs both b = do
  g <- ask
  inVs <- mapM mkVar ins
  outVs <- mapM mkVar outs
  bothVs <- mapM mkVar both
  let doLog = logKind g
      loggedBody LogFunc = loggedMethod (logName g) n inVs b
      loggedBody LogAll  = loggedMethod (logName g) n inVs b
      loggedBody _       = b
      bod = body $ loggedBody doLog
      fn = inOutFunc n s pr inVs outVs bothVs bod
  pComms <- mapM (paramComment . (^. uid)) ins
  oComms <- mapM (paramComment . (^. uid)) outs
  bComms <- mapM (paramComment . (^. uid)) both
  return $ if CommentFunc `elem` commented g 
    then docInOutFunc desc pComms oComms bComms fn else fn

getComment :: (NamedIdea c, MayHaveUnit c) => UID -> Map UID c -> 
  Reader State String
getComment l m = do
  t <- getTermDoc l m
  let u = getUnitsDoc l m
  return $ render $ t <+> u

paramComment :: UID -> Reader State String
paramComment l = do
  g <- ask
  let m = vMap $ codeSpec g
  getComment l m

returnComment :: UID -> Reader State String
returnComment l = do
  g <- ask
  let m = fMap $ codeSpec g
  getComment l m

loggedMethod :: (RenderSym repr) => Label -> Label -> [repr (Variable repr)] -> 
  [repr (Block repr)] -> [repr (Block repr)]
loggedMethod lName n vars b = block [
      varDec varLogFile,
      openFileA varLogFile (litString lName),
      printFileStrLn valLogFile ("function " ++ n ++ " called with inputs: {"),
      multi $ printInputs vars,
      printFileStrLn valLogFile "  }",
      closeFile valLogFile ]
      : b
  where
    printInputs [] = []
    printInputs [v] = [
      printFileStr valLogFile ("  " ++ variableName v ++ " = "), 
      printFileLn valLogFile (valueOf v)]
    printInputs (v:vs) = [
      printFileStr valLogFile ("  " ++ variableName v ++ " = "), 
      printFile valLogFile (valueOf v), 
      printFileStrLn valLogFile ", "] ++ printInputs vs
    

---- MAIN ---

genModule :: (RenderSym repr) => Name -> String
               -> Maybe (Reader State [repr (Method repr)])
               -> Maybe (Reader State [repr (Class repr)])
               -> Reader State (repr (RenderFile repr))
genModule n desc maybeMs maybeCs = do
  g <- ask
  let ls = fromMaybe [] (Map.lookup n (dMap $ codeSpec g))
      updateState = withReader (\s -> s { currentModule = n })
      -- Below line of code cannot be simplified because authors has a generic type
      as = case csi (codeSpec g) of CSI {authors = a} -> map name a
  cs <- maybe (return []) updateState maybeCs
  ms <- maybe (return []) updateState maybeMs
  let commMod | CommentMod `elem` commented g                   = docMod desc 
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = docMod "" []      (date g)
              | otherwise                                       = id
  return $ commMod $ fileDoc $ buildModule n ls ms cs


genMain :: (RenderSym repr) => Reader State (repr (RenderFile repr))
genMain = genModule "Control" "Controls the flow of the program" 
  (Just $ liftS genMainFunc) Nothing

genMainFunc :: (RenderSym repr) => Reader State (repr (Method repr))
genMainFunc = do
    g <- ask
    v_filename <- mkVar $ codevar inFileName
    logInFile <- maybeLog v_filename
    ip <- getInputDecl
    gi <- getInputCall
    dv <- getDerivedCall
    ic <- getConstraintCall
    varDef <- mapM getCalcCall (execOrder $ csi $ codeSpec g)
    wo <- getOutputCall
    return $ (if CommentFunc `elem` commented g then docMain else mainMethod)
      "" $ bodyStatements $
      initLogFileVar (logKind g) ++
      varDecDef v_filename (arg 0) : logInFile ++
      catMaybes ([ip, gi, dv, ic] ++ varDef ++ [wo])

getInputDecl :: (RenderSym repr) => Reader State (Maybe (repr (
  Statement repr)))
getInputDecl = do
  g <- ask
  v_params <- mkVar (codevar inParams)
  let getDecl _ [] = return Nothing
      getDecl Unbundled ins = do
        vars <- mapM mkVar ins
        return $ Just $ multi $ map varDec vars
      getDecl Bundled _ = return $ Just $ extObjDecNewVoid "InputParameters"
        v_params 
  getDecl (inStruct g) (inputs $ csi $ codeSpec g)

getFuncCall :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => String 
  -> repr (StateType repr) -> Reader State [c] -> 
  Reader State (Maybe (repr (Value repr)))
getFuncCall n t funcPs = do
  g <- ask
  let getCall Nothing = return Nothing
      getCall (Just m) = do
        cs <- funcPs
        pvals <- mapM mkVal cs
        val <- fApp m n t pvals
        return $ Just val
  getCall $ Map.lookup n (eMap $ codeSpec g)

getInOutCall :: (RenderSym repr, HasCodeType c, CodeIdea c) => String -> 
  Reader State [c] -> Reader State [c] -> Reader State [c] -> 
  Reader State (Maybe (repr (Statement repr)))
getInOutCall n inFunc outFunc bothFunc = do
  g <- ask
  let getCall Nothing = return Nothing
      getCall (Just m) = do
        ins' <- inFunc
        ins <- mapM mkVar ins'
        outs' <- outFunc
        outs <- mapM mkVar outs'
        both' <- bothFunc
        both <- mapM mkVar both'
        stmt <- fAppInOut m n (map valueOf ins) outs both
        return $ Just stmt
  getCall $ Map.lookup n (eMap $ codeSpec g)

getInputCall :: (RenderSym repr) => Reader State (Maybe (repr (Statement repr)))
getInputCall = getInOutCall "get_input" getInputFormatIns getInputFormatOuts 
  (return [])

getDerivedCall :: (RenderSym repr) => Reader State 
  (Maybe (repr (Statement repr)))
getDerivedCall = getInOutCall "derived_values" getDerivedIns getDerivedOuts 
  (return [])

getConstraintCall :: (RenderSym repr) => Reader State 
  (Maybe (repr (Statement repr)))
getConstraintCall = do
  val <- getFuncCall "input_constraints" void getConstraintParams
  return $ fmap valState val

getCalcCall :: (RenderSym repr) => CodeDefinition -> Reader State 
  (Maybe (repr (Statement repr)))
getCalcCall c = do
  g <- ask
  val <- getFuncCall (codeName c) (convType $ codeType c) (getCalcParams c)
  v <- maybe (error $ (c ^. uid) ++ " missing from VarMap") mkVar 
    (Map.lookup (c ^. uid) (vMap $ codeSpec g))
  l <- maybeLog v
  return $ fmap (multi . (: l) . varDecDef v) val

getOutputCall :: (RenderSym repr) => Reader State 
  (Maybe (repr (Statement repr)))
getOutputCall = do
  val <- getFuncCall "write_output" void getOutputParams
  return $ fmap valState val

getInputFormatIns :: Reader State [CodeChunk]
getInputFormatIns = do
  g <- ask
  let getIns :: Structure -> [CodeChunk]
      getIns Unbundled = []
      getIns Bundled = [codevar inParams]
  getParams $ codevar inFileName : getIns (inStruct g)

getInputFormatOuts :: Reader State [CodeChunk]
getInputFormatOuts = do
  g <- ask
  let getOuts :: Structure -> [CodeChunk]
      getOuts Unbundled = extInputs $ csi $ codeSpec g
      getOuts Bundled = []
  getParams $ getOuts (inStruct g)

getDerivedIns :: Reader State [CodeChunk]
getDerivedIns = do
  g <- ask
  let s = csi $ codeSpec g
      dvals = derivedInputs s
      reqdVals = concatMap (flip codevars (sysinfodb s) . codeEquat) dvals
  getParams reqdVals

getDerivedOuts :: Reader State [CodeChunk]
getDerivedOuts = do
  g <- ask
  let getOuts :: Structure -> [CodeChunk]
      getOuts Unbundled = map codeChunk $ derivedInputs $ csi $ codeSpec g
      getOuts Bundled = []
  getParams $ getOuts (inStruct g)

getConstraintParams :: Reader State [CodeChunk]
getConstraintParams = do 
  g <- ask
  let cm = cMap $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      db = sysinfodb $ csi $ codeSpec g
      varsList = filter (\i -> member (i ^. uid) cm) (inputs $ csi $ codeSpec g)
      reqdVals = nub $ varsList ++ concatMap (\v -> constraintvarsandfuncs v db 
        mem) (getConstraints cm varsList)
  getParams reqdVals

getCalcParams :: CodeDefinition -> Reader State [CodeChunk]
getCalcParams c = do
  g <- ask
  getParams $ codevars' (codeEquat c) $ sysinfodb $ csi $ codeSpec g

getOutputParams :: Reader State [CodeChunk]
getOutputParams = do
  g <- ask
  getParams $ outputs $ csi $ codeSpec g

-----

loggedVar :: (RenderSym repr) => repr (Variable repr) -> 
  Reader State (repr (Statement repr))
loggedVar v = do
    g <- ask
    return $ multi [
      openFileA varLogFile (litString $ logName g),
      printFileStr valLogFile ("var '" ++ variableName v ++ "' assigned to "),
      printFile valLogFile (valueOf v),
      printFileStrLn valLogFile (" in module " ++ currentModule g),
      closeFile valLogFile ]

-- helpers

value :: (RenderSym repr) => UID -> String -> repr (StateType repr) -> 
  Reader State (repr (Value repr))
value u s t = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
  maybe (do { v <- variable s t; return $ valueOf v }) 
    (convExpr . codeEquat) (Map.lookup u mm)

variable :: (RenderSym repr) => String -> repr (StateType repr) -> 
  Reader State (repr (Variable repr))
variable s t = do
  g <- ask
  let cs = csi $ codeSpec g
  if s `elem` map codeName (inputs cs) 
    then inputVariable (inStruct g) s t
    else return $ var s t

inputVariable :: (RenderSym repr) => Structure -> String -> 
  repr (StateType repr) -> Reader State (repr (Variable repr))
inputVariable Unbundled s t = return $ var s t
inputVariable Bundled s t = do
  ip <- mkVar (codevar inParams)
  return $ ip $-> var s t
  
fApp :: (RenderSym repr) => String -> String -> repr (StateType repr) -> 
  [repr (Value repr)] -> Reader State (repr (Value repr))
fApp m s t vl = do
  g <- ask
  return $ if m /= currentModule g then extFuncApp m s t vl else funcApp s t vl

fAppInOut :: (RenderSym repr) => String -> String -> [repr (Value repr)] -> 
  [repr (Variable repr)] -> [repr (Variable repr)] -> 
  Reader State (repr (Statement repr))
fAppInOut m n ins outs both = do
  g <- ask
  return $ if m /= currentModule g then extInOutCall m n ins outs both
    else inOutCall n ins outs both


getParams :: (CodeIdea c) => [c] -> Reader State [CodeChunk]
getParams cs' = do
  g <- ask
  let cs = map codeChunk cs'
      ins = inputs $ csi $ codeSpec g
      consts = map codeChunk $ constants $ csi $ codeSpec g
      inpVars = filter (`elem` ins) cs
      conVars = filter (`elem` consts) cs
      csSubIns = cs \\ (ins ++ consts)
      inVs = getInputVars (inStruct g) inpVars
      conVs = getConstVars conVars
  return $ inVs ++ conVs ++ csSubIns

-- getParams :: (RenderSym repr, CodeIdea c) => [c] -> Reader State 
--   [repr (Parameter repr)]
-- getParams cs = do
--   vs <- getVars cs
--   return $ map mkParam vs 

mkParam :: (RenderSym repr) => repr (Variable repr) -> repr (Parameter repr)
mkParam v = paramFunc (getType $ variableType v) v
  where paramFunc (C.List _) = pointerParam
        paramFunc (C.Object _) = pointerParam
        paramFunc _ = stateParam

mkVal :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Value repr))
mkVal v = value (v ^. uid) (codeName v) (convType $ codeType v)

mkVar :: (RenderSym repr, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Variable repr))
mkVar v = variable (codeName v) (convType $ codeType v)

getInputVars :: Structure -> [CodeChunk] -> [CodeChunk]
getInputVars _ [] = []
getInputVars Unbundled cs = cs
getInputVars Bundled _ = [codevar inParams]

-- Right now, we always inline constants. In the future, this will be captured by a choice and this function should be updated to read that choice
getConstVars :: [CodeChunk] -> [CodeChunk]
getConstVars _ = []

convExpr :: (RenderSym repr) => Expr -> Reader State (repr (Value repr))
convExpr (Dbl d) = return $ litFloat d
convExpr (Int i) = return $ litInt i
convExpr (Str s) = return $ litString s
convExpr (Perc a b) = return $ litFloat $ fromIntegral a / (10 ** fromIntegral b)
convExpr (AssocA Add l) = foldl1 (#+)  <$> mapM convExpr l
convExpr (AssocA Mul l) = foldl1 (#*)  <$> mapM convExpr l
convExpr (AssocB And l) = foldl1 (?&&) <$> mapM convExpr l
convExpr (AssocB Or l)  = foldl1 (?||) <$> mapM convExpr l
convExpr Deriv{} = return $ litString "**convExpr :: Deriv unimplemented**"
convExpr (C c)   = do
  g <- ask
  let v = quantvar (lookupC g c)
  mkVal v
convExpr (FCall (C c) x) = do
  g <- ask
  let info = sysinfodb $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      funcCd = quantfunc (symbResolve info c)
      funcNm = codeName funcCd
      funcTp = convType $ codeType funcCd
  args <- mapM convExpr x
  maybe (error $ "Call to non-existent function" ++ funcNm) 
    (\f -> fApp f funcNm funcTp args) (Map.lookup funcNm mem)
convExpr FCall{}   = return $ litString "**convExpr :: FCall unimplemented**"
convExpr (UnaryOp o u) = fmap (unop o) (convExpr u)
convExpr (BinaryOp Frac (Int a) (Int b)) =
  return $ litFloat (fromIntegral a) #/ litFloat (fromIntegral b) -- hack to deal with integer division
convExpr (BinaryOp o a b)  = liftM2 (bfunc o) (convExpr a) (convExpr b)
convExpr (Case c l)      = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen"
    doit [(e,_)] = convExpr e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExpr cond) (convExpr e) 
      (convExpr (Case c xs))
convExpr Matrix{}    = error "convExpr: Matrix"
convExpr Operator{} = error "convExpr: Operator"
convExpr IsIn{}    = error "convExpr: IsIn"
convExpr (RealI c ri)  = do
  g <- ask
  convExpr $ renderRealInt (lookupC g c) ri

getUpperBound :: Expr -> Expr
getUpperBound (BinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

lookupC :: State -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ csi $ codeSpec g)

renderC :: (HasUID c, HasSymbol c) => c -> Constraint -> Expr
renderC s (Range _ rr)          = renderRealInt s rr
renderC s (EnumeratedReal _ rr) = IsIn (sy s) (DiscreteD rr)
renderC s (EnumeratedStr _ rr)  = IsIn (sy s) (DiscreteS rr)

renderRealInt :: (HasUID c, HasSymbol c) => c -> RealInterval Expr Expr -> Expr
renderRealInt s (Bounded (Inc,a) (Inc,b)) = (a $<= sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Inc,a) (Exc,b)) = (a $<= sy s) $&& (sy s $<  b)
renderRealInt s (Bounded (Exc,a) (Inc,b)) = (a $<  sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Exc,a) (Exc,b)) = (a $<  sy s) $&& (sy s $<  b)
renderRealInt s (UpTo (Inc,a))    = sy s $<= a
renderRealInt s (UpTo (Exc,a))    = sy s $< a
renderRealInt s (UpFrom (Inc,a))  = sy s $>= a
renderRealInt s (UpFrom (Exc,a))  = sy s $>  a

unop :: (RenderSym repr) => UFunc -> (repr (Value repr) -> repr (Value repr))
unop Sqrt = (#/^)
unop Log  = log
unop Ln   = ln
unop Abs  = (#|)
unop Exp  = exp
unop Sin  = sin
unop Cos  = cos
unop Tan  = tan
unop Csc  = csc
unop Sec  = sec
unop Cot  = cot
unop Arcsin = arcsin
unop Arccos = arccos
unop Arctan = arctan
unop Dim  = listSize
unop Norm = error "unop: Norm not implemented"
unop Not  = (?!)
unop Neg  = (#~)

bfunc :: (RenderSym repr) => BinOp -> (repr (Value repr) -> repr
  (Value repr) -> repr (Value repr))
bfunc Eq    = (?==)
bfunc NEq   = (?!=)
bfunc Gt    = (?>)
bfunc Lt    = (?<)
bfunc LEq   = (?<=)
bfunc GEq   = (?>=)
bfunc Cross = error "bfunc: Cross not implemented"
bfunc Pow   = (#^)
bfunc Subt  = (#-)
bfunc Impl  = error "convExpr :=>"
bfunc Iff   = error "convExpr :<=>"
bfunc Dot   = error "convExpr DotProduct"
bfunc Frac  = (#/)
bfunc Index = listAccess

-- medium hacks --
genModDef :: (RenderSym repr) => CS.Mod -> Reader State (repr (RenderFile repr))
genModDef (CS.Mod n desc fs) = genModule n desc (Just $ mapM genFunc fs) Nothing

genFunc :: (RenderSym repr) => Func -> Reader State (repr (Method repr))
genFunc (FDef (FuncDef n desc parms o rd s)) = do
  g <- ask
  stmts <- mapM convStmt s
  vars <- mapM mkVar (fstdecl (sysinfodb $ csi $ codeSpec g) s \\ parms)
  publicMethod (mState $ convType o) n desc parms rd [block $ map varDec 
    vars ++ stmts]
genFunc (FData (FuncData n desc ddef)) = genDataFunc n desc ddef
genFunc (FCD cd) = genCalcFunc cd

convStmt :: (RenderSym repr) => FuncStmt -> Reader State (repr (Statement repr))
convStmt (FAsg v e) = do
  e' <- convExpr e
  v' <- mkVar v
  l <- maybeLog v'
  return $ multi $ assign v' e' : l
convStmt (FFor v e st) = do
  stmts <- mapM convStmt st
  e' <- convExpr $ getUpperBound e
  return $ forRange (codeName v) (litInt 0) e' (litInt 1) (bodyStatements stmts)
convStmt (FWhile e st) = do
  stmts <- mapM convStmt st
  e' <- convExpr e
  return $ while e' (bodyStatements stmts)
convStmt (FCond e tSt []) = do
  stmts <- mapM convStmt tSt
  e' <- convExpr e
  return $ ifNoElse [(e', bodyStatements stmts)]
convStmt (FCond e tSt eSt) = do
  stmt1 <- mapM convStmt tSt
  stmt2 <- mapM convStmt eSt
  e' <- convExpr e
  return $ ifCond [(e', bodyStatements stmt1)] (bodyStatements stmt2)
convStmt (FRet e) = do
  e' <- convExpr e
  return $ returnState e'
convStmt (FThrow s) = return $ throw s
convStmt (FTry t c) = do
  stmt1 <- mapM convStmt t
  stmt2 <- mapM convStmt c
  return $ tryCatch (bodyStatements stmt1) (bodyStatements stmt2)
convStmt FContinue = return continue
convStmt (FDec v) = do
  vari <- mkVar v
  let convDec (C.List _) = listDec 0 vari
      convDec _ = varDec vari
  return $ convDec (codeType v) 
convStmt (FProcCall n l) = do
  e' <- convExpr (FCall (asExpr n) l)
  return $ valState e'
convStmt (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ valState $ listAppend a' b'

genDataFunc :: (RenderSym repr) => Name -> String -> DataDesc -> 
  Reader State (repr (Method repr))
genDataFunc nameTitle desc ddef = do
  let parms = getInputs ddef
  bod <- readData ddef
  publicMethod (mState void) nameTitle desc (codevar inFileName : parms) 
    Nothing bod

-- this is really ugly!!
readData :: (RenderSym repr) => DataDesc -> Reader State
  [repr (Block repr)]
readData ddef = do
  inD <- mapM inData ddef
  v_filename <- mkVal $ codevar inFileName
  return [block $ 
    varDec var_infile :
    (if any (\d -> isLine d || isLines d) ddef then [varDec var_line, listDec 0 var_linetokens] else []) ++
    [listDec 0 var_lines | any isLines ddef] ++
    openFileR var_infile v_filename :
    concat inD ++ [
    closeFile v_infile ]]
  where inData :: (RenderSym repr) => Data -> Reader State [repr (Statement repr)]
        inData (Singleton v) = do
            vv <- mkVar v
            l <- maybeLog vv
            return [multi $ getFileInput v_infile vv : l]
        inData JunkData = return [discardFileLine v_infile]
        inData (Line lp d) = do
          lnI <- lineData Nothing lp
          logs <- getEntryVarLogs lp
          return $ [getFileInputLine v_infile var_line, 
            stringSplit d var_linetokens v_line] ++ lnI ++ logs
        inData (Lines lp ls d) = do
          lnV <- lineData (Just "_temp") lp
          logs <- getEntryVarLogs lp
          let readLines Nothing = [getFileInputAll v_infile var_lines,
                forRange l_i (litInt 0) (listSize v_lines) (litInt 1)
                  (bodyStatements $ stringSplit d var_linetokens (
                  listAccess v_lines v_i) : lnV)]
              readLines (Just numLines) = [forRange l_i (litInt 0) 
                (litInt numLines) (litInt 1)
                (bodyStatements $
                  [getFileInputLine v_infile var_line,
                   stringSplit d var_linetokens v_line
                  ] ++ lnV)]
          return $ readLines ls ++ logs
        ---------------
        lineData :: (RenderSym repr) => Maybe String -> LinePattern -> 
          Reader State [repr (Statement repr)]
        lineData s p@(Straight _) = do
          vs <- getEntryVars s p
          return [stringListVals vs v_linetokens]
        lineData s p@(Repeat ds) = do
          vs <- getEntryVars s p
          return $ clearTemps s ds ++ stringListLists vs v_linetokens 
            : appendTemps s ds
        ---------------
        clearTemps :: (RenderSym repr) => Maybe String -> [DataItem] -> 
          [repr (Statement repr)]
        clearTemps Nothing _ = []
        clearTemps (Just sfx) es = map (clearTemp sfx) es
        ---------------
        clearTemp :: (RenderSym repr) => String -> DataItem -> 
          repr (Statement repr)
        clearTemp sfx v = listDecDef (var (codeName v ++ sfx) 
          (listInnerType $ convType $ codeType v)) []
        ---------------
        appendTemps :: (RenderSym repr) => Maybe String -> [DataItem] -> 
          [repr (Statement repr)]
        appendTemps Nothing _ = []
        appendTemps (Just sfx) es = map (appendTemp sfx) es
        ---------------
        appendTemp :: (RenderSym repr) => String -> DataItem -> 
          repr (Statement repr)
        appendTemp sfx v = valState $ listAppend 
          (valueOf $ var (codeName v) (convType $ codeType v)) 
          (valueOf $ var (codeName v ++ sfx) (convType $ codeType v))
        ---------------
        l_line, l_lines, l_linetokens, l_infile, l_i :: Label
        var_line, var_lines, var_linetokens, var_infile :: 
          (RenderSym repr) => repr (Variable repr)
        v_line, v_lines, v_linetokens, v_infile, v_i ::
          (RenderSym repr) => repr (Value repr)
        l_line = "line"
        var_line = var l_line string
        v_line = valueOf var_line
        l_lines = "lines"
        var_lines = var l_lines (listType static_ string)
        v_lines = valueOf var_lines
        l_linetokens = "linetokens"
        var_linetokens = var l_linetokens (listType static_ string)
        v_linetokens = valueOf var_linetokens
        l_infile = "infile"
        var_infile = var l_infile infile
        v_infile = valueOf var_infile
        l_i = "i"
        v_i = valueOf $ var l_i int

getEntryVars :: (RenderSym repr) => Maybe String -> LinePattern -> 
  Reader State [repr (Variable repr)]
getEntryVars s lp = mapM (maybe mkVar (\st v -> variable (codeName v ++ st) 
  (listInnerType $ convType $ codeType v)) s) (getPatternInputs lp)

getEntryVarLogs :: (RenderSym repr) => LinePattern -> 
  Reader State [repr (Statement repr)]
getEntryVarLogs lp = do
  vs <- getEntryVars Nothing lp
  logs <- mapM maybeLog vs
  return $ concat logs
