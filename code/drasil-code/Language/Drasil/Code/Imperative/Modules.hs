module Language.Drasil.Code.Imperative.Modules (
  genMain, chooseInModule, genConstMod, genOutputMod, genSampleInput
) where

import Language.Drasil
import Database.Drasil (ChunkDB)
import Language.Drasil.Code.Imperative.Descriptions (constClassDesc, 
  constModDesc, derivedValuesDesc, dvFuncDesc, inConsFuncDesc, inFmtFuncDesc, 
  inputClassDesc, inputConstraintsDesc, inputConstructorDesc, inputFormatDesc, 
  inputParametersDesc, modDesc, outputFormatDesc, woFuncDesc)
import Language.Drasil.Code.Imperative.FunctionCalls (getCalcCall,
  getAllInputCalls, getOutputCall)
import Language.Drasil.Code.Imperative.GenerateGOOL (genModule, publicClass)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (CalcType(CalcAssign), convExpr,
  genCalcBlock, genConstructor, mkVal, mkVar, privateInOutMethod, privateMethod,
  publicFunc, publicInOutFunc, readData, renderC)
import Language.Drasil.Code.Imperative.Logging (maybeLog, varLogFile)
import Language.Drasil.Code.Imperative.Parameters (getConstraintParams, 
  getDerivedIns, getDerivedOuts, getInConstructorParams, getInputFormatIns, 
  getInputFormatOuts, getOutputParams)
import Language.Drasil.Code.Imperative.State (DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (AuxiliarySym(..))
import Language.Drasil.Chunk.Code (CodeIdea(codeName, codeChunk), CodeChunk, 
  codeType, codevar, physLookup, sfwrLookup)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.Code.DataDesc (DataDesc, junkLine, singleton)
import Language.Drasil.CodeSpec (AuxFile(..), CodeSpec(..), CodeSystInfo(..),
  Comments(CommentFunc), ConstantStructure(..), ConstantRepr(..), 
  ConstraintBehaviour(..), InputModule(..), Logging(..))
import Language.Drasil.Printers (Linearity(Linear), exprDoc)

import GOOL.Drasil (RenderSym(..), BodySym(..), BlockSym(..), 
  PermanenceSym(..), TypeSym(..), VariableSym(..), ValueSym(..), 
  BooleanExpression(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ScopeTag(..), 
  convType, GS, FS, MS)

import Prelude hiding (print)
import Data.List (intersperse, intercalate, partition)
import Data.Map (member)
import qualified Data.Map as Map (lookup, filter)
import Data.Maybe (maybeToList, catMaybes)
import Control.Applicative ((<$>))
import Control.Monad.Reader (Reader, ask, asks)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (render)

---- MAIN ---

genMain :: (RenderSym repr) => Reader DrasilState (FS (repr (RenderFile repr)))
genMain = genModule "Control" "Controls the flow of the program" 
  (Just $ liftS genMainFunc) Nothing

genMainFunc :: (RenderSym repr) => Reader DrasilState (MS (repr (Method repr)))
genMainFunc = do
    g <- ask
    v_filename <- mkVar $ codevar inFileName
    logInFile <- maybeLog v_filename
    ip <- getInputDecl
    co <- initConsts
    ics <- getAllInputCalls
    varDef <- mapM getCalcCall (execOrder $ csi $ codeSpec g)
    wo <- getOutputCall
    return $ (if CommentFunc `elem` commented g then docMain else mainFunction)
      $ bodyStatements $
      initLogFileVar (logKind g) ++
      varDecDef v_filename (arg 0) : logInFile ++
      catMaybes [ip, co] ++ ics ++ catMaybes (varDef ++ [wo])

getInputDecl :: (RenderSym repr) => Reader DrasilState 
  (Maybe (GS (repr (Statement repr))))
getInputDecl = do
  g <- ask
  v_params <- mkVar (codevar inParams)
  constrParams <- getInConstructorParams 
  cps <- mapM mkVal constrParams
  let cname = "InputParameters"
      getDecl ([],[]) = constIns (partition (flip member (Map.filter (cname ==) 
        (eMap $ codeSpec g)) . codeName) (map codeChunk $ constants $ csi $ 
        codeSpec g)) (conRepr g)
      getDecl ([],ins) = do
        vars <- mapM mkVar ins
        return $ Just $ multi $ map varDec vars
      getDecl (_,[]) = return $ Just $ extObjDecNew cname v_params cps
      getDecl _ = error ("Inputs or constants are only partially contained in " 
        ++ cname ++ " class")
      constIns ([],[]) _ = return Nothing
      constIns _ Const = return Nothing
      constIns cs Var = getDecl cs 
  getDecl (partition (flip member (Map.filter (cname ==) (eMap $ codeSpec g)) 
    . codeName) (inputs $ csi $ codeSpec g))

initConsts :: (RenderSym repr) => Reader DrasilState 
  (Maybe (GS (repr (Statement repr))))
initConsts = do
  g <- ask
  v_consts <- mkVar (codevar consts)
  let cname = "Constants"
      getDecl _ Inline = return Nothing
      getDecl ([],[]) _ = return Nothing
      getDecl (_,[]) _ = asks (constCont . conRepr)
      getDecl ([],cs) _ = getDecl' $ partition (flip member (eMap $ codeSpec g) 
        . codeName) cs 
      getDecl _ _ = error "Only some constants associated with Constants module in export map"
      constCont Var = Just $ extObjDecNewNoParams cname v_consts
      constCont Const = Nothing
      getDecl' (_,[]) = return Nothing
      getDecl' ([],cs) = do 
        vars <- mapM mkVar cs
        vals <- mapM (convExpr . codeEquat) cs
        logs <- mapM maybeLog vars
        return $ Just $ multi $ zipWith (defFunc $ conRepr g) vars vals ++ concat logs
      getDecl' _ = error "Only some constants present in export map"
      defFunc Var = varDecDef
      defFunc Const = constDecDef
  getDecl (partition (flip member (Map.filter (cname ==) (eMap $ codeSpec g)) 
    . codeName) (constants $ csi $ codeSpec g)) (conStruct g)

initLogFileVar :: (RenderSym repr) => Logging -> [GS (repr (Statement repr))]
initLogFileVar LogVar = [varDec varLogFile]
initLogFileVar LogAll = [varDec varLogFile]
initLogFileVar _ = []

------- INPUT ----------

chooseInModule :: (RenderSym repr) => InputModule -> Reader DrasilState 
  [FS (repr (RenderFile repr))]
chooseInModule Combined = genInputModCombined
chooseInModule Separated = genInputModSeparated

genInputModSeparated :: (RenderSym repr) => 
  Reader DrasilState [FS (repr (RenderFile repr))]
genInputModSeparated = do
  ipDesc <- modDesc inputParametersDesc
  ifDesc <- modDesc (liftS inputFormatDesc)
  dvDesc <- modDesc (liftS derivedValuesDesc)
  icDesc <- modDesc (liftS inputConstraintsDesc)
  sequence 
    [genModule "InputParameters" ipDesc 
      Nothing (Just $ fmap maybeToList genInputClass),
    genModule "InputFormat" ifDesc
      (Just $ fmap maybeToList (genInputFormat Pub)) Nothing,
    genModule "DerivedValues" dvDesc
      (Just $ fmap maybeToList (genInputDerived Pub)) Nothing,
    genModule "InputConstraints" icDesc 
      (Just $ fmap maybeToList (genInputConstraints Pub)) Nothing]

genInputModCombined :: (RenderSym repr) => 
  Reader DrasilState [FS (repr (RenderFile repr))]
genInputModCombined = do
  ipDesc <- modDesc inputParametersDesc
  let cname = "InputParameters"
      genMod :: (RenderSym repr) => Maybe (FS (repr (Class repr))) ->
        Reader DrasilState (FS (repr (RenderFile repr)))
      genMod Nothing = genModule cname ipDesc (Just $ concat <$> mapM (fmap 
        maybeToList) [genInputFormat Pub, genInputDerived 
        Pub, genInputConstraints Pub]) 
        Nothing
      genMod _ = genModule cname ipDesc Nothing (Just $ fmap maybeToList genInputClass)
  ic <- genInputClass
  liftS $ genMod ic

constVarFunc :: (RenderSym repr) => ConstantRepr -> String ->
  (repr (Variable repr) -> repr (Value repr) -> GS (repr (StateVar repr)))
constVarFunc Var n = stateVarDef n public dynamic_
constVarFunc Const n = constVar n public

genInputClass :: (RenderSym repr) => 
  Reader DrasilState (Maybe (FS (repr (Class repr))))
genInputClass = do
  g <- ask
  let ins = inputs $ csi $ codeSpec g
      cs = constants $ csi $ codeSpec g
      cname = "InputParameters"
      filt :: (CodeIdea c) => [c] -> [c]
      filt = filter (flip member (Map.filter (cname ==) (eMap $ codeSpec g)) . 
        codeName)
      methods :: (RenderSym repr) => InputModule -> 
        Reader DrasilState [MS (repr (Method repr))]
      methods Separated = return []
      methods Combined = concat <$> mapM (fmap maybeToList) 
        [genInputConstructor, genInputFormat Priv, 
        genInputDerived Priv, 
        genInputConstraints Priv]
      genClass :: (RenderSym repr) => [CodeChunk] -> [CodeDefinition] -> 
        Reader DrasilState (Maybe (FS (repr (Class repr))))
      genClass [] [] = return Nothing
      genClass inps csts = do
        vals <- mapM (convExpr . codeEquat) csts
        let inputVars = map (\x -> pubMVar (var (codeName x) (convType $ 
              codeType x))) inps
            constVars = zipWith (\c vl -> constVarFunc (conRepr g) cname
              (var (codeName c) (convType $ codeType c)) vl) csts vals
        icDesc <- inputClassDesc
        c <- publicClass icDesc cname Nothing (inputVars ++ constVars) 
          (methods $ inMod g)
        return $ Just c
  genClass (filt ins) (filt cs)

genInputConstructor :: (RenderSym repr) => Reader DrasilState 
  (Maybe (MS (repr (Method repr))))
genInputConstructor = do
  g <- ask
  let dm = defMap $ codeSpec g
      genCtor False = return Nothing
      genCtor True = do 
        cdesc <- inputConstructorDesc
        cparams <- getInConstructorParams    
        ics <- getAllInputCalls
        ctor <- genConstructor "InputParameters" cdesc cparams 
          [block ics]
        return $ Just ctor
  genCtor $ any (`member` dm) ["get_input", "derived_values", 
    "input_constraints"]

genInputDerived :: (RenderSym repr) => ScopeTag -> 
  Reader DrasilState (Maybe (MS (repr (Method repr))))
genInputDerived s = do
  g <- ask
  let dvals = derivedInputs $ csi $ codeSpec g
      getFunc Pub = publicInOutFunc
      getFunc Priv = privateInOutMethod "InputParameters"
      genDerived :: (RenderSym repr) => Maybe String -> Reader DrasilState 
        (Maybe (MS (repr (Method repr))))
      genDerived Nothing = return Nothing
      genDerived (Just _) = do
        ins <- getDerivedIns
        outs <- getDerivedOuts
        bod <- mapM (\x -> genCalcBlock CalcAssign x (codeEquat x)) dvals
        desc <- dvFuncDesc
        mthd <- getFunc s "derived_values" desc ins outs bod
        return $ Just mthd
  genDerived $ Map.lookup "derived_values" (defMap $ codeSpec g)

genInputConstraints :: (RenderSym repr) => ScopeTag ->
  Reader DrasilState (Maybe (MS (repr (Method repr))))
genInputConstraints s = do
  g <- ask
  let cm = cMap $ csi $ codeSpec g
      getFunc Pub = publicFunc
      getFunc Priv = privateMethod "InputParameters"
      genConstraints :: (RenderSym repr) => Maybe String -> Reader DrasilState 
        (Maybe (MS (repr (Method repr))))
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
        mthd <- getFunc s "input_constraints" void desc parms 
          Nothing [block sf, block hw]
        return $ Just mthd
  genConstraints $ Map.lookup "input_constraints" (defMap $ codeSpec g)

sfwrCBody :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr) 
  => [(q,[Constraint])] -> Reader DrasilState [GS (repr (Statement repr))]
sfwrCBody cs = do
  g <- ask
  let cb = onSfwrC g
  chooseConstr cb cs

physCBody :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr) 
  => [(q,[Constraint])] -> Reader DrasilState [GS (repr (Statement repr))]
physCBody cs = do
  g <- ask
  let cb = onPhysC g
  chooseConstr cb cs

chooseConstr :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, 
  RenderSym repr) => ConstraintBehaviour -> [(q,[Constraint])] -> 
  Reader DrasilState [GS (repr (Statement repr))]
chooseConstr Warning   cs = do
  checks <- mapM constrWarn cs
  return $ concat checks
chooseConstr Exception cs = do
  checks <- mapM constrExc cs
  return $ concat checks

constrWarn :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr)
  => (q,[Constraint]) -> Reader DrasilState [GS (repr (Statement repr))]
constrWarn c = do
  let q = fst c
      cs = snd c
  conds <- mapM (convExpr . renderC q) cs
  msgs <- mapM (constraintViolatedMsg q "suggested") cs
  return $ zipWith (\cond m -> ifNoElse [((?!) cond, bodyStatements $
    printStr "Warning: " : m)]) conds msgs

constrExc :: (HasUID q, HasSymbol q, CodeIdea q, HasCodeType q, RenderSym repr) 
  => (q,[Constraint]) -> Reader DrasilState [GS (repr (Statement repr))]
constrExc c = do
  let q = fst c
      cs = snd c
  conds <- mapM (convExpr . renderC q) cs
  msgs <- mapM (constraintViolatedMsg q "expected") cs
  return $ zipWith (\cond m -> ifNoElse [((?!) cond, bodyStatements $ 
    m ++ [throw "InputError"])]) conds msgs

constraintViolatedMsg :: (CodeIdea q, HasUID q, HasCodeType q, RenderSym repr) 
  => q -> String -> Constraint -> Reader DrasilState 
  [GS (repr (Statement repr))]
constraintViolatedMsg q s c = do
  pc <- printConstraint c 
  v <- mkVal q
  return $ [printStr $ codeName q ++ " has value ",
    print v,
    printStr $ " but " ++ s ++ " to be "] ++ pc

printConstraint :: (RenderSym repr) => Constraint -> 
  Reader DrasilState [GS (repr (Statement repr))]
printConstraint c = do
  g <- ask
  let db = sysinfodb $ csi $ codeSpec g
      printConstraint' :: (RenderSym repr) => Constraint -> Reader DrasilState 
        [GS (repr (Statement repr))]
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

printExpr :: (RenderSym repr) => Expr -> ChunkDB -> [GS (repr (Statement repr))]
printExpr (Dbl _) _ = []
printExpr (Int _) _ = []
printExpr e db = [printStr $ " (" ++ render (exprDoc db Implementation Linear e)
  ++ ")"]

genInputFormat :: (RenderSym repr) => ScopeTag -> 
  Reader DrasilState (Maybe (MS (repr (Method repr))))
genInputFormat s = do
  g <- ask
  dd <- genDataDesc
  let getFunc Pub = publicInOutFunc
      getFunc Priv = privateInOutMethod "InputParameters"
      genInFormat :: (RenderSym repr) => Maybe String -> Reader DrasilState 
        (Maybe (MS (repr (Method repr))))
      genInFormat Nothing = return Nothing
      genInFormat (Just _) = do
        ins <- getInputFormatIns
        outs <- getInputFormatOuts
        bod <- readData dd
        desc <- inFmtFuncDesc
        mthd <- getFunc s "get_input" desc ins outs bod
        return $ Just mthd
  genInFormat $ Map.lookup "get_input" (defMap $ codeSpec g)

genDataDesc :: Reader DrasilState DataDesc
genDataDesc = do
  g <- ask
  return $ junkLine : 
    intersperse junkLine (map singleton (extInputs $ csi $ codeSpec g))

genSampleInput :: (AuxiliarySym repr) => Reader DrasilState [repr (Auxiliary repr)]
genSampleInput = do
  g <- ask
  dd <- genDataDesc
  return [sampleInput (sysinfodb $ csi $ codeSpec g) dd (sampleData g) | SampleInput `elem` 
    auxiliaries g]

----- CONSTANTS -----

genConstMod :: (RenderSym repr) => Reader DrasilState [FS (repr (RenderFile repr))]
genConstMod = do
  cDesc <- modDesc $ liftS constModDesc
  liftS $ genModule "Constants" cDesc Nothing (Just $ fmap maybeToList 
    genConstClass)

genConstClass :: (RenderSym repr) => 
  Reader DrasilState (Maybe (FS (repr (Class repr))))
genConstClass = do
  g <- ask
  let cs = constants $ csi $ codeSpec g
      cname = "Constants"
      genClass :: (RenderSym repr) => [CodeDefinition] -> Reader DrasilState (Maybe 
        (FS (repr (Class repr))))
      genClass [] = return Nothing 
      genClass vs = do
        vals <- mapM (convExpr . codeEquat) vs 
        let vars = map (\x -> var (codeName x) (convType $ codeType x)) vs
            constVars = zipWith (constVarFunc (conRepr g) cname) vars vals
        cDesc <- constClassDesc
        cls <- publicClass cDesc cname Nothing constVars (return [])
        return $ Just cls
  genClass $ filter (flip member (Map.filter (cname ==) (eMap $ codeSpec g)) . 
    codeName) cs

----- OUTPUT -------

genOutputMod :: (RenderSym repr) => Reader DrasilState [FS (repr (RenderFile repr))]
genOutputMod = do
  outformat <- genOutputFormat
  ofDesc <- modDesc $ liftS outputFormatDesc
  let outf = maybeToList outformat
  liftS $ genModule "OutputFormat" ofDesc
    (Just $ return outf) Nothing

genOutputFormat :: (RenderSym repr) => 
  Reader DrasilState (Maybe (MS (repr (Method repr))))
genOutputFormat = do
  g <- ask
  let genOutput :: (RenderSym repr) => Maybe String -> Reader DrasilState 
        (Maybe (MS (repr (Method repr))))
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
        mthd <- publicFunc "write_output" void desc parms Nothing 
          [block $ [
          varDec var_outfile,
          openFileW var_outfile (litString "output.txt") ] ++
          concat outp ++ [ closeFile v_outfile ]]
        return $ Just mthd
  genOutput $ Map.lookup "write_output" (eMap $ codeSpec g)
