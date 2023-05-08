module Language.Drasil.Code.Imperative.Modules (
  genMain, genMainFunc, chooseInModule, genInputClass, genInputDerived, 
  genInputConstraints, genInputFormat, genConstMod, genConstClass, genCalcMod, 
  genCalcFunc, genOutputMod, genOutputFormat, genSampleInput
) where

import Language.Drasil (Constraint(..), RealInterval(..),
  HasUID(uid), Stage(..))
import Database.Drasil (ChunkDB)
import Language.Drasil.CodeExpr.Development
import Language.Drasil.Code.Imperative.Comments (getComment)
import Language.Drasil.Code.Imperative.Descriptions (constClassDesc, 
  constModDesc, derivedValuesDesc, dvFuncDesc, inConsFuncDesc, inFmtFuncDesc, 
  inputClassDesc, inputConstraintsDesc, inputConstructorDesc, inputFormatDesc, 
  inputParametersDesc, modDesc, outputFormatDesc, woFuncDesc, calcModDesc)
import Language.Drasil.Code.Imperative.FunctionCalls (getCalcCall,
  getAllInputCalls, getOutputCall)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..), genModule, 
  genModuleWithImports, primaryClass, auxClass)
import Language.Drasil.Code.Imperative.Helpers (liftS)
import Language.Drasil.Code.Imperative.Import (codeType, convExpr, convStmt,
  genConstructor, mkVal, mkVar, privateInOutMethod, privateMethod, publicFunc, 
  publicInOutFunc, readData, renderC)
import Language.Drasil.Code.Imperative.Logging (maybeLog, varLogFile)
import Language.Drasil.Code.Imperative.Parameters (getConstraintParams, 
  getDerivedIns, getDerivedOuts, getInConstructorParams, getInputFormatIns, 
  getInputFormatOuts, getCalcParams, getOutputParams)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (AuxiliarySym(..))
import Language.Drasil.Chunk.Code (CodeIdea(codeName), CodeVarChunk, quantvar, 
  DefiningCodeExpr(..))
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, DefinitionType(..),
  defType)
import Language.Drasil.Chunk.ConstraintMap (physLookup, sfwrLookup)
import Language.Drasil.Chunk.Parameter (pcAuto)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.Code.DataDesc (DataDesc, junkLine, singleton)
import Language.Drasil.Code.ExtLibImport (defs, imports, steps)
import Language.Drasil.Choices (Comments(..), ConstantStructure(..), 
  ConstantRepr(..), ConstraintBehaviour(..), ImplementationType(..), 
  InputModule(..), Logging(..), Structure(..), hasSampleInput)
import Language.Drasil.CodeSpec (CodeSpec(..))
import Language.Drasil.Expr.Development (Completeness(..))
import Language.Drasil.Printers (Linearity(Linear), codeExprDoc)

import GOOL.Drasil (SFile, MSBody, MSBlock, SVariable, SValue, MSStatement, 
  SMethod, CSStateVar, SClass, OOProg, BodySym(..), bodyStatements, oneLiner, 
  BlockSym(..), PermanenceSym(..), TypeSym(..), VariableSym(..), Literal(..), 
  VariableValue(..), CommandLineArgs(..), BooleanExpression(..), 
  StatementSym(..), AssignStatement(..), DeclStatement(..), objDecNewNoParams, 
  extObjDecNewNoParams, IOStatement(..), ControlStatement(..), ifNoElse, 
  ScopeSym(..), MethodSym(..), StateVarSym(..), pubDVar, convType, ScopeTag(..))

import Prelude hiding (print)
import Data.List (intersperse, partition)
import Data.Map ((!), elems, member)
import qualified Data.Map as Map (lookup, filter)
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad (liftM2, zipWithM)
import Control.Monad.State (get, gets)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (render)

type ConstraintCE = Constraint CodeExpr

---- MAIN ---

-- | Generates a controller module.
genMain :: (OOProg r) => GenState (SFile r)
genMain = genModule "Control" "Controls the flow of the program" 
  [genMainFunc] []

-- | Generates a main function, to act as the controller for an SCS program.
-- The controller declares input and constant variables, then calls the 
-- functions for reading input values, calculating derived inputs, checking 
-- constraints, calculating outputs, and printing outputs.
-- Returns Nothing if the user chose to generate a library.
genMainFunc :: (OOProg r) => GenState (Maybe (SMethod r))
genMainFunc = do
    g <- get
    let mainFunc Library = return Nothing
        mainFunc Program = do
          v_filename <- mkVar $ quantvar inFileName
          logInFile <- maybeLog v_filename
          co <- initConsts
          ip <- getInputDecl
          ics <- getAllInputCalls
          varDef <- mapM getCalcCall (execOrder $ codeSpec g)
          wo <- getOutputCall
          return $ Just $ (if CommentFunc `elem` commented g then docMain else 
            mainFunction) $ bodyStatements $ initLogFileVar (logKind g) 
            ++ varDecDef v_filename (arg 0) 
            : logInFile 
            -- Constants must be declared before inputs because some derived 
            -- input definitions or input constraints may use the constants
            ++ catMaybes [co, ip] ++ ics ++ catMaybes (varDef ++ [wo])
    mainFunc $ implType g

-- | If there are no inputs, the 'inParams' object still needs to be declared 
-- if inputs are 'Bundled', constants are stored 'WithInputs', and constant 
-- representation is 'Var'.
-- If there are inputs and they are not exported by any module, then they are 
-- 'Unbundled' and are declared individually using 'varDec'.
-- If there are inputs and they are exported by a module, they are 'Bundled' in
-- the InputParameters class, so 'inParams' should be declared and constructed,
-- using 'objDecNew' if the inputs are exported by the current module, and 
-- 'extObjDecNew' if they are exported by a different module.
getInputDecl :: (OOProg r) => GenState (Maybe (MSStatement r))
getInputDecl = do
  g <- get
  v_params <- mkVar (quantvar inParams)
  constrParams <- getInConstructorParams 
  cps <- mapM mkVal constrParams
  let cname = "InputParameters"
      getDecl ([],[]) = constIns (partition (flip member (eMap g) . 
        codeName) (map quantvar $ constants $ codeSpec g)) (conRepr g) 
        (conStruct g)
      getDecl ([],ins) = do
        vars <- mapM mkVar ins
        return $ Just $ multi $ map varDec vars
      getDecl (i:_,[]) = return $ Just $ (if currentModule g == 
        eMap g ! codeName i then objDecNew 
        else extObjDecNew cname) v_params cps
      getDecl _ = error ("Inputs or constants are only partially contained in " 
        ++ "a class")
      constIns ([],[]) _ _ = return Nothing
      -- If Const is chosen, don't declare an object because constants are static and accessed through class
      constIns cs Var WithInputs = getDecl cs
      constIns _ _ _ = return Nothing 
  getDecl (partition (flip member (eMap g) . codeName) 
    (inputs $ codeSpec g))

-- | If constants are 'Unbundled', declare them individually using 'varDecDef' if 
-- representation is 'Var' and 'constDecDef' if representation is 'Const'.
-- If constants are 'Bundled' independently and representation is 'Var', declare 
-- the consts object. If representation is 'Const', no object needs to be 
-- declared because the constants will be accessed directly through the 
-- Constants class.
-- If constants are 'Bundled' 'WithInputs', do 'Nothing'; declaration of the 'inParams' 
-- object is handled by 'getInputDecl'.
-- If constants are 'Inlined', nothing needs to be declared.
initConsts :: (OOProg r) => GenState (Maybe (MSStatement r))
initConsts = do
  g <- get
  v_consts <- mkVar (quantvar consts)
  let cname = "Constants"
      cs = constants $ codeSpec g 
      getDecl (Store Unbundled) _ = declVars
      getDecl (Store Bundled) _ = gets (declObj cs . conRepr)
      getDecl WithInputs Unbundled = declVars
      getDecl WithInputs Bundled = return Nothing
      getDecl Inline _ = return Nothing
      declVars = do 
        vars <- mapM (mkVar . quantvar) cs
        vals <- mapM (convExpr . (^. codeExpr)) cs
        logs <- mapM maybeLog vars
        return $ Just $ multi $ zipWith (defFunc $ conRepr g) vars vals ++ 
          concat logs
      defFunc Var = varDecDef
      defFunc Const = constDecDef
      declObj [] _ = Nothing
      declObj (c:_) Var = Just $ (if currentModule g == eMap g ! codeName c 
        then objDecNewNoParams else extObjDecNewNoParams cname) v_consts
      declObj _ Const = Nothing
  getDecl (conStruct g) (inStruct g)

-- | Generates a statement to declare the variable representing the log file, 
-- if the user chose to turn on logs for variable assignments.
initLogFileVar :: (OOProg r) => [Logging] -> [MSStatement r]
initLogFileVar l = [varDec varLogFile | LogVar `elem` l]

------- INPUT ----------

-- | Generates either a single module containing all input-related components, or 
-- separate modules for each input-related component, depending on the user's 
-- modularity choice.
chooseInModule :: (OOProg r) => InputModule -> GenState [SFile r]
chooseInModule Combined = genInputModCombined
chooseInModule Separated = genInputModSeparated

-- | Generates separate modules for each input-related component.
genInputModSeparated :: (OOProg r) => GenState [SFile r]
genInputModSeparated = do
  ipDesc <- modDesc inputParametersDesc
  ifDesc <- modDesc (liftS inputFormatDesc)
  dvDesc <- modDesc (liftS derivedValuesDesc)
  icDesc <- modDesc (liftS inputConstraintsDesc)
  sequence 
    [genModule "InputParameters" ipDesc [] [genInputClass Primary],
    genModule "InputFormat" ifDesc [genInputFormat Pub] [],
    genModule "DerivedValues" dvDesc [genInputDerived Pub] [],
    genModule "InputConstraints" icDesc [genInputConstraints Pub] []]

-- | Generates a single module containing all input-related components.
genInputModCombined :: (OOProg r) => GenState [SFile r]
genInputModCombined = do
  ipDesc <- modDesc inputParametersDesc
  let cname = "InputParameters"
      genMod :: (OOProg r) => Maybe (SClass r) ->
        GenState (SFile r)
      genMod Nothing = genModule cname ipDesc [genInputFormat Pub, 
        genInputDerived Pub, genInputConstraints Pub] []
      genMod _ = genModule cname ipDesc [] [genInputClass Primary]
  ic <- genInputClass Primary
  liftS $ genMod ic

-- | Returns a function for generating a state variable for a constant.
-- Either generates a declare-define statement for a regular state variable
-- (if user chose 'Var'),
-- or a declare-define statement for a constant variable (if user chose 'Const').
constVarFunc :: (OOProg r) => ConstantRepr ->
  (SVariable r -> SValue r -> CSStateVar r)
constVarFunc Var = stateVarDef public dynamic
constVarFunc Const = constVar public

-- | Returns 'Nothing' if no inputs or constants are mapped to InputParameters in 
-- the class definition map.
-- If any inputs or constants are defined in InputParameters, this generates 
-- the InputParameters class containing the inputs and constants as state 
-- variables. If the InputParameters constructor is also exported, then the
-- generated class also contains the input-related functions as private methods.
genInputClass :: (OOProg r) => ClassType -> 
  GenState (Maybe (SClass r))
genInputClass scp = do
  g <- get
  let ins = inputs $ codeSpec g
      cs = constants $ codeSpec g
      filt :: (CodeIdea c) => [c] -> [c]
      filt = filter ((Just cname ==) . flip Map.lookup (clsMap g) . codeName)
      methods :: (OOProg r) => GenState [SMethod r]
      methods = if cname `elem` defList g 
        then concat <$> mapM (fmap maybeToList) [genInputConstructor, 
        genInputFormat Priv, genInputDerived Priv, genInputConstraints Priv] 
        else return []
      genClass :: (OOProg r) => [CodeVarChunk] -> [CodeDefinition] -> 
        GenState (Maybe (SClass r))
      genClass [] [] = return Nothing
      genClass inps csts = do
        vals <- mapM (convExpr . (^. codeExpr)) csts
        inputVars <- mapM (\x -> fmap (pubDVar . var (codeName x) . convType) 
          (codeType x)) inps
        constVars <- zipWithM (\c vl -> fmap (\t -> constVarFunc (conRepr g) 
          (var (codeName c) (convType t)) vl) (codeType c)) 
          csts vals
        let getFunc Primary = primaryClass
            getFunc Auxiliary = auxClass
            f = getFunc scp
        icDesc <- inputClassDesc
        c <- f cname Nothing icDesc (inputVars ++ constVars) methods
        return $ Just c
  genClass (filt ins) (filt cs)
  where cname = "InputParameters"

-- | Generates a constructor for the input class, where the constructor calls the 
-- input-related functions. Returns 'Nothing' if no input-related functions are
-- generated.
genInputConstructor :: (OOProg r) => GenState (Maybe (SMethod r))
genInputConstructor = do
  g <- get
  let dl = defList g
      genCtor False = return Nothing
      genCtor True = do 
        cdesc <- inputConstructorDesc
        cparams <- getInConstructorParams    
        ics <- getAllInputCalls
        ctor <- genConstructor "InputParameters" cdesc (map pcAuto cparams)
          [block ics]
        return $ Just ctor
  genCtor $ any (`elem` dl) ["get_input", "derived_values", 
    "input_constraints"]

-- | Generates a function for calculating derived inputs.
genInputDerived :: (OOProg r) => ScopeTag ->
  GenState (Maybe (SMethod r))
genInputDerived s = do
  g <- get
  let dvals = derivedInputs $ codeSpec g
      getFunc Pub = publicInOutFunc
      getFunc Priv = privateInOutMethod
      genDerived :: (OOProg r) => Bool -> GenState 
        (Maybe (SMethod r))
      genDerived False = return Nothing
      genDerived _ = do
        ins <- getDerivedIns
        outs <- getDerivedOuts
        bod <- mapM (\x -> genCalcBlock CalcAssign x (x ^. codeExpr)) dvals
        desc <- dvFuncDesc
        mthd <- getFunc s "derived_values" desc ins outs bod
        return $ Just mthd
  genDerived $ "derived_values" `elem` defList g

-- | Generates function that checks constraints on the input.
genInputConstraints :: (OOProg r) => ScopeTag ->
  GenState (Maybe (SMethod r))
genInputConstraints s = do
  g <- get
  let cm = cMap $ codeSpec g
      getFunc Pub = publicFunc
      getFunc Priv = privateMethod
      genConstraints :: (OOProg r) => Bool -> GenState 
        (Maybe (SMethod r))
      genConstraints False = return Nothing
      genConstraints _ = do
        parms <- getConstraintParams
        let varsList = filter (\i -> member (i ^. uid) cm) (inputs $ codeSpec g)
            sfwrCs   = map (sfwrLookup cm) varsList
            physCs   = map (physLookup cm) varsList
        sf <- sfwrCBody sfwrCs
        ph <- physCBody physCs
        desc <- inConsFuncDesc
        mthd <- getFunc s "input_constraints" void desc (map pcAuto parms) 
          Nothing [block sf, block ph]
        return $ Just mthd
  genConstraints $ "input_constraints" `elem` defList g

-- | Generates input constraints code block for checking software constraints.
sfwrCBody :: (OOProg r) => [(CodeVarChunk, [ConstraintCE])] -> 
  GenState [MSStatement r]
sfwrCBody cs = do
  g <- get
  let cb = onSfwrC g
  chooseConstr cb cs

-- | Generates input constraints code block for checking physical constraints.
physCBody :: (OOProg r) => [(CodeVarChunk, [ConstraintCE])] -> 
  GenState [MSStatement r]
physCBody cs = do
  g <- get
  let cb = onPhysC g
  chooseConstr cb cs

-- | Generates conditional statements for checking constraints, where the 
-- bodies depend on user's choice of constraint violation behaviour.
chooseConstr :: (OOProg r) => ConstraintBehaviour -> 
  [(CodeVarChunk, [ConstraintCE])] -> GenState [MSStatement r]
chooseConstr cb cs = do
  conds <- mapM (\(q,cns) -> mapM (convExpr . renderC q) cns) cs
  bods <- mapM (chooseCB cb) cs
  return $ concat $ zipWith (zipWith (\cond bod -> ifNoElse [((?!) cond, bod)]))
    conds bods
  where chooseCB Warning = constrWarn
        chooseCB Exception = constrExc 

-- | Generates body defining constraint violation behaviour if Warning chosen from 'chooseConstr'.
-- Prints a \"Warning\" message followed by a message that says
-- what value was \"suggested\".
constrWarn :: (OOProg r) => (CodeVarChunk, [ConstraintCE]) -> 
  GenState [MSBody r]
constrWarn c = do
  let q = fst c
      cs = snd c
  msgs <- mapM (constraintViolatedMsg q "suggested") cs
  return $ map (bodyStatements . (printStr "Warning: " :)) msgs

-- | Generates body defining constraint violation behaviour if Exception chosen from 'chooseConstr'.
-- Prints a message that says what value was \"expected\",
-- followed by throwing an exception.
constrExc :: (OOProg r) => (CodeVarChunk, [ConstraintCE]) -> 
  GenState [MSBody r]
constrExc c = do
  let q = fst c
      cs = snd c
  msgs <- mapM (constraintViolatedMsg q "expected") cs
  return $ map (bodyStatements . (++ [throw "InputError"])) msgs

-- | Generates statements that print a message for when a constraint is violated.
-- Message includes the name of the cosntraint quantity, its value, and a
-- description of the constraint that is violated.
constraintViolatedMsg :: (OOProg r) => CodeVarChunk -> String -> 
  ConstraintCE -> GenState [MSStatement r]
constraintViolatedMsg q s c = do
  pc <- printConstraint c 
  v <- mkVal (quantvar q)
  return $ [printStr $ codeName q ++ " has value ",
    print v,
    printStr $ ", but is " ++ s ++ " to be "] ++ pc

-- | Generates statements to print descriptions of constraints, using words and 
-- the constrained values. Constrained values are followed by printing the 
-- expression they originated from, using printExpr. 
printConstraint :: (OOProg r) => ConstraintCE ->
  GenState [MSStatement r]
printConstraint c = do
  g <- get
  let db = sysinfodb $ codeSpec g
      printConstraint' :: (OOProg r) => ConstraintCE -> GenState 
        [MSStatement r]
      printConstraint' (Range _ (Bounded (_, e1) (_, e2))) = do
        lb <- convExpr e1
        ub <- convExpr e2
        return $ [printStr "between ", print lb] ++ printExpr e1 db ++
          [printStr " and ", print ub] ++ printExpr e2 db ++ [printStrLn "."]
      printConstraint' (Range _ (UpTo (_, e))) = do
        ub <- convExpr e
        return $ [printStr "below ", print ub] ++ printExpr e db ++ 
          [printStrLn "."]
      printConstraint' (Range _ (UpFrom (_, e))) = do
        lb <- convExpr e
        return $ [printStr "above ", print lb] ++ printExpr e db ++ [printStrLn "."]
  printConstraint' c

-- | Don't print expressions that are just literals, because that would be 
-- redundant (the values are already printed by printConstraint).
-- If expression is more than just a literal, print it in parentheses.
printExpr :: (OOProg r) => CodeExpr -> ChunkDB -> [MSStatement r]
printExpr Lit{} _  = []
printExpr e     db = [printStr $ " (" ++ render (codeExprDoc db Implementation Linear e) ++ ")"]

-- | | Generates a function for reading inputs from a file.
genInputFormat :: (OOProg r) => ScopeTag -> 
  GenState (Maybe (SMethod r))
genInputFormat s = do
  g <- get
  dd <- genDataDesc
  let getFunc Pub = publicInOutFunc
      getFunc Priv = privateInOutMethod
      genInFormat :: (OOProg r) => Bool -> GenState 
        (Maybe (SMethod r))
      genInFormat False = return Nothing
      genInFormat _ = do
        ins <- getInputFormatIns
        outs <- getInputFormatOuts
        bod <- readData dd
        desc <- inFmtFuncDesc
        mthd <- getFunc s "get_input" desc ins outs bod
        return $ Just mthd
  genInFormat $ "get_input" `elem` defList g

-- | Defines the 'DataDesc' for the format we require for input files. When we make
-- input format a design variability, this will read the user's design choices 
-- instead of returning a fixed 'DataDesc'.
genDataDesc :: GenState DataDesc
genDataDesc = do
  g <- get
  return $ junkLine : 
    intersperse junkLine (map singleton (extInputs $ codeSpec g))

-- | Generates a sample input file compatible with the generated program, 
-- if the user chose to.
genSampleInput :: (AuxiliarySym r) => GenState (Maybe (r (Auxiliary r)))
genSampleInput = do
  g <- get
  dd <- genDataDesc
  if hasSampleInput (auxiliaries g) then (return . Just) $ sampleInput 
    (sysinfodb $ codeSpec g) dd (sampleData g) else return Nothing

----- CONSTANTS -----

-- | Generates a module containing the class where constants are stored. 
genConstMod :: (OOProg r) => GenState [SFile r]
genConstMod = do
  cDesc <- modDesc $ liftS constModDesc
  liftS $ genModule "Constants" cDesc [] [genConstClass Primary]

-- | Generates a class to store constants, if constants are mapped to the 
-- Constants class in the class definition map, otherwise returns Nothing.
genConstClass :: (OOProg r) => ClassType ->
  GenState (Maybe (SClass r))
genConstClass scp = do
  g <- get
  let cs = constants $ codeSpec g
      genClass :: (OOProg r) => [CodeDefinition] -> GenState 
        (Maybe (SClass r))
      genClass [] = return Nothing 
      genClass vs = do
        vals <- mapM (convExpr . (^. codeExpr)) vs 
        vars <- mapM (\x -> fmap (var (codeName x) . convType) (codeType x)) vs
        let constVars = zipWith (constVarFunc (conRepr g)) vars vals
            getFunc Primary = primaryClass
            getFunc Auxiliary = auxClass
            f = getFunc scp
        cDesc <- constClassDesc
        cls <- f cname Nothing cDesc constVars (return [])
        return $ Just cls
  genClass $ filter (flip member (Map.filter (cname ==) (clsMap g)) 
    . codeName) cs
  where cname = "Constants"

------- CALC ----------

-- | Generates a module containing calculation functions.
genCalcMod :: (OOProg r) => GenState (SFile r)
genCalcMod = do
  g <- get
  let elmap = extLibMap g
  genModuleWithImports "Calculations" calcModDesc (concatMap (^. imports) $ 
    elems elmap) (map (fmap Just . genCalcFunc) (execOrder $ codeSpec g)) []

-- | Generates a calculation function corresponding to the 'CodeDefinition'.
-- For solving ODEs, the 'ExtLibState' containing the information needed to 
-- generate code is found by looking it up in the external library map.
genCalcFunc :: (OOProg r) => CodeDefinition -> 
  GenState (SMethod r)
genCalcFunc cdef = do
  g <- get
  parms <- getCalcParams cdef
  let nm = codeName cdef
  tp <- codeType cdef
  v <- mkVar (quantvar cdef)
  blcks <- case cdef ^. defType 
            of Definition -> liftS $ genCalcBlock CalcReturn cdef 
                 (cdef ^. codeExpr)
               ODE -> maybe (error $ nm ++ " missing from ExtLibMap") 
                 (\el -> do
                   defStmts <- mapM convStmt (el ^. defs)
                   stepStmts <- mapM convStmt (el ^. steps)
                   return [block (varDec v : defStmts), 
                     block stepStmts,
                     block [returnStmt $ valueOf v]])
                 (Map.lookup nm (extLibMap g))
  desc <- getComment cdef
  publicFunc
    nm
    (convType tp)
    ("Calculates " ++ desc)
    (map pcAuto parms)
    (Just desc)
    blcks

-- | Calculations may be assigned to a variable or asked for a result.
data CalcType = CalcAssign | CalcReturn deriving Eq

-- | Generates a calculation block for the given 'CodeDefinition', and assigns the 
-- result to a variable (if 'CalcAssign') or returns the result (if 'CalcReturn').
genCalcBlock :: (OOProg r) => CalcType -> CodeDefinition -> CodeExpr ->
  GenState (MSBlock r)
genCalcBlock t v (Case c e) = genCaseBlock t v c e
genCalcBlock CalcAssign v e = do
  vv <- mkVar (quantvar v)
  ee <- convExpr e
  l <- maybeLog vv
  return $ block $ assign vv ee : l
genCalcBlock CalcReturn _ e = block <$> liftS (returnStmt <$> convExpr e)

-- | Generates a calculation block for a value defined by cases. 
-- If the function is defined for every case, the final case is captured by an 
-- else clause, otherwise an error-throwing else-clause is generated.
genCaseBlock :: (OOProg r) => CalcType -> CodeDefinition -> Completeness 
  -> [(CodeExpr, CodeExpr)] -> GenState (MSBlock r)
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

-- | Generates a module containing the function for printing outputs.
genOutputMod :: (OOProg r) => GenState [SFile r]
genOutputMod = do
  ofDesc <- modDesc $ liftS outputFormatDesc
  liftS $ genModule "OutputFormat" ofDesc [genOutputFormat] []

-- | Generates a function for printing output values.
genOutputFormat :: (OOProg r) => GenState (Maybe (SMethod r))
genOutputFormat = do
  g <- get
  let genOutput :: (OOProg r) => Maybe String -> GenState 
        (Maybe (SMethod r))
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
                 ] ) (outputs $ codeSpec g)
        desc <- woFuncDesc
        mthd <- publicFunc "write_output" void desc (map pcAuto parms) Nothing 
          [block $ [
          varDec var_outfile,
          openFileW var_outfile (litString "output.txt") ] ++
          concat outp ++ [ closeFile v_outfile ]]
        return $ Just mthd
  genOutput $ Map.lookup "write_output" (eMap g)
