module Language.Drasil.Code.Imperative.Modules (
  genMain, chooseInModule, genOutputMod
) where

import Language.Drasil
import Database.Drasil (ChunkDB)
import Language.Drasil.Code.Imperative.Descriptions (derivedValuesDesc, 
  dvFuncDesc, inConsFuncDesc, inFmtFuncDesc, inputClassDesc, 
  inputConstraintsDesc, inputFormatDesc, inputParametersDesc, modDesc, 
  outputFormatDesc, woFuncDesc)
import Language.Drasil.Code.Imperative.FunctionCalls (getCalcCall,
  getConstraintCall, getDerivedCall, getInputCall, getOutputCall)
import Language.Drasil.Code.Imperative.GenerateGOOL (genModule, publicClass)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (CalcType(CalcAssign), convExpr,
  genCalcBlock, mkVal, mkVar, publicInOutFunc, publicMethod, readData, renderC)
import Language.Drasil.Code.Imperative.Logging (maybeLog, varLogFile)
import Language.Drasil.Code.Imperative.Parameters (getConstraintParams, 
  getDerivedIns, getDerivedOuts, getInputFormatIns, getInputFormatOuts, 
  getOutputParams)
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (RenderSym(..),
  BodySym(..), BlockSym(..), StateTypeSym(..), VariableSym(..), ValueSym(..),
  BooleanExpression(..), StatementSym(..), ControlStatementSym(..), 
  MethodTypeSym(..), MethodSym(..), StateVarSym(..), ClassSym(..))
import Language.Drasil.Code.Imperative.GOOL.Helpers (convType)
import Language.Drasil.Chunk.Code (CodeIdea(codeName), codeType, codevar, 
  physLookup, sfwrLookup)
import Language.Drasil.Chunk.CodeDefinition (codeEquat)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams)
import Language.Drasil.Code.DataDesc (junkLine, singleton)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..),
  Comments(CommentFunc), ConstraintBehaviour(..), InputModule(..), Logging(..), 
  Structure(..))
import Language.Drasil.Printers (Linearity(Linear), exprDoc)

import Prelude hiding (print)
import Data.List (intersperse, intercalate)
import Data.Map (member)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybeToList, catMaybes, mapMaybe)
import Control.Applicative ((<$>))
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (render)

---- MAIN ---

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

initLogFileVar :: (RenderSym repr) => Logging -> [repr (Statement repr)]
initLogFileVar LogVar = [varDec varLogFile]
initLogFileVar LogAll = [varDec varLogFile]
initLogFileVar _ = []

------- INPUT ----------

chooseInModule :: (RenderSym repr) => InputModule -> Reader State 
  [repr (RenderFile repr)]
chooseInModule Combined = genInputModCombined
chooseInModule Separated = genInputModSeparated

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

chooseInStructure :: (RenderSym repr) => Structure -> Reader State 
  (Maybe (repr (Class repr)))
chooseInStructure Unbundled = return Nothing
chooseInStructure Bundled = genInputClass

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

chooseConstr :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, 
  RenderSym repr) => ConstraintBehaviour -> [(q,[Constraint])] -> 
  Reader State [repr (Statement repr)]
chooseConstr Warning   cs = do
  checks <- mapM constrWarn cs
  return $ concat checks
chooseConstr Exception cs = do
  checks <- mapM constrExc cs
  return $ concat checks

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
