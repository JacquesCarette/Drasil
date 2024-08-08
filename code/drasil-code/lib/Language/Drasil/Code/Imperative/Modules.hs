{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Language.Drasil.Code.Imperative.Modules (
  genMain, genMainProc, genMainFunc, genMainFuncProc, genInputClass,
  genInputDerived, genInputDerivedProc, genInputMod, genInputModProc,
  genInputConstraints, genInputConstraintsProc, genInputFormat,
  genInputFormatProc, genConstMod, checkConstClass, genConstClass, genCalcMod,
  genCalcModProc, genCalcFunc, genCalcFuncProc, genOutputMod, genOutputModProc,
  genOutputFormat, genOutputFormatProc, genSampleInput
) where
import Language.Drasil (Constraint(..), RealInterval(..),
  HasUID(uid), Stage(..))
import Database.Drasil (ChunkDB)
import Language.Drasil.CodeExpr.Development
import Language.Drasil.Code.Imperative.Comments (getComment)
import Language.Drasil.Code.Imperative.Descriptions (constClassDesc,
  constModDesc, dvFuncDesc, inConsFuncDesc, inFmtFuncDesc, inputClassDesc,
  inputConstructorDesc, inputParametersDesc, modDesc, outputFormatDesc,
  woFuncDesc, calcModDesc)
import Language.Drasil.Code.Imperative.FunctionCalls (genCalcCall,
  genCalcCallProc, genAllInputCalls, genAllInputCallsProc, genOutputCall,
  genOutputCallProc)
import Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..), genModule,
  genModuleProc, genModuleWithImports, genModuleWithImportsProc, primaryClass,
  auxClass)
import Language.Drasil.Code.Imperative.Helpers (liftS, convScope)
import Language.Drasil.Code.Imperative.Import (codeType, convExpr, convExprSet, convExprProc,
  convStmt, convStmtProc, genConstructor, mkVal, mkValProc, mkVar, mkVarProc,
  privateInOutMethod, privateMethod, privateFuncProc, publicFunc,
  publicFuncProc, publicInOutFunc, publicInOutFuncProc, privateInOutFuncProc,
  readData, readDataProc, renderC)
import Language.Drasil.Code.Imperative.Logging (maybeLog, varLogFile)
import Language.Drasil.Code.Imperative.Parameters (getConstraintParams,
  getDerivedIns, getDerivedOuts, getInConstructorParams, getInputFormatIns,
  getInputFormatOuts, getCalcParams, getOutputParams)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..),
  ScopeType(..), genICName)
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
  Logging(..), Structure(..), hasSampleInput, InternalConcept(..))
import Language.Drasil.CodeSpec (CodeSpec(..))
import Language.Drasil.Expr.Development (Completeness(..))
import Language.Drasil.Printers (SingleLine(OneLine), codeExprDoc)

import Drasil.GOOL (MSBody, MSBlock, SVariable, SValue, MSStatement,
  SMethod, CSStateVar, SClass, SharedProg, OOProg, BodySym(..), bodyStatements,
  oneLiner, BlockSym(..), PermanenceSym(..), TypeSym(..), VariableSym(..),
  ScopeSym(..), Literal(..), VariableValue(..), CommandLineArgs(..),
  BooleanExpression(..), StatementSym(..), AssignStatement(..),
  DeclStatement(..), OODeclStatement(..), objDecNewNoParams,
  extObjDecNewNoParams, IOStatement(..), ControlStatement(..), ifNoElse,
  VisibilitySym(..), MethodSym(..), StateVarSym(..), pubDVar, convType,
    convTypeOO, VisibilityTag(..))

import qualified Drasil.GOOL as OO (SFile)
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as Proc (SFile)

import Prelude hiding (print)
import Data.List (intersperse, partition)
import Data.Map ((!), elems, member)
import qualified Data.Map as Map (lookup, filter)
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad (liftM2, zipWithM)
import Control.Monad.State (get, gets, modify)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (render)

type ConstraintCE = Constraint CodeExpr

---- MAIN ---

-- | Generates a controller module.
genMain :: (OOProg r) => GenState (OO.SFile r)
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
          modify (\st -> st {currentScope = MainFn})
          v_filename <- mkVar (quantvar inFileName)
          logInFile <- maybeLog v_filename
          co <- initConsts
          ip <- getInputDecl
          ics <- genAllInputCalls
          varDef <- mapM genCalcCall (execOrder $ codeSpec g)
          wo <- genOutputCall
          return $ Just $ (if CommentFunc `elem` commented g then docMain else
            mainFunction) $ bodyStatements $ initLogFileVar (logKind g) mainFn
            ++ varDecDef v_filename mainFn (arg 0)
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
  let scp = convScope $ currentScope g
  v_params <- mkVar (quantvar inParams)
  constrParams <- getInConstructorParams
  cps <- mapM mkVal constrParams
  cname <- genICName InputParameters
  let getDecl ([],[]) = constIns (partition (flip member (eMap g) .
        codeName) (map quantvar $ constants $ codeSpec g)) (conRepr g)
        (conStruct g)
      getDecl ([],ins) = do
        vars <- mapM mkVar ins
        return $ Just $ multi $ map (`varDec` scp) vars
      getDecl (i:_,[]) = return $ Just $ (if currentModule g ==
        eMap g ! codeName i then objDecNew
        else extObjDecNew cname) v_params scp cps
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
  let scp = convScope $ currentScope g
  v_consts <- mkVar (quantvar consts)
  cname <- genICName Constants
  let cs = constants $ codeSpec g
      getDecl (Store Unbundled) _ = declVars
      getDecl (Store Bundled) _ = gets (declObj cs . conRepr)
      getDecl WithInputs Unbundled = declVars
      getDecl WithInputs Bundled = return Nothing
      getDecl Inline _ = return Nothing
      declVars = do
        vars <- mapM (mkVar . quantvar) cs
        vals <- mapM (convExpr . (^. codeExpr)) cs
        logs <- mapM maybeLog vars
        return $ Just $ multi $
          zipWith (\vr -> defFunc (conRepr g) vr scp) vars vals ++ concat logs
      defFunc Var = varDecDef
      defFunc Const = constDecDef
      declObj [] _ = Nothing
      declObj (c:_) Var = Just $ (if currentModule g == eMap g ! codeName c
        then objDecNewNoParams else extObjDecNewNoParams cname) v_consts scp
      declObj _ Const = Nothing
  getDecl (conStruct g) (inStruct g)

-- | Generates a statement to declare the variable representing the log file,
-- if the user chose to turn on logs for variable assignments.
initLogFileVar :: (SharedProg r) => [Logging] -> r (Scope r) -> [MSStatement r]
initLogFileVar l scp = [varDec varLogFile scp | LogVar `elem` l]

------- INPUT ----------

-- | Generates a single module containing all input-related components.
genInputMod :: (OOProg r) => GenState [OO.SFile r]
genInputMod = do
  ipDesc <- modDesc inputParametersDesc
  cname <- genICName InputParameters
  let genMod :: (OOProg r) => Maybe (SClass r) ->
        GenState (OO.SFile r)
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
  modify (\st -> st {currentScope = Local})
  cname <- genICName InputParameters
  let ins = inputs $ codeSpec g
      cs = constants $ codeSpec g
      filt :: (CodeIdea c) => [c] -> [c]
      filt = filter ((Just cname ==) . flip Map.lookup (clsMap g) . codeName)
      constructors :: (OOProg r) => GenState [SMethod r]
      constructors = if cname `elem` defSet g
        then concat <$> mapM (fmap maybeToList) [genInputConstructor]
        else return []
      methods :: (OOProg r) => GenState [SMethod r]
      methods = if cname `elem` defSet g
        then concat <$> mapM (fmap maybeToList) [genInputFormat Priv,
        genInputDerived Priv, genInputConstraints Priv]
        else return []
      genClass :: (OOProg r) => [CodeVarChunk] -> [CodeDefinition] ->
        GenState (Maybe (SClass r))
      genClass [] [] = return Nothing
      genClass inps csts = do
        vals <- mapM (convExpr . (^. codeExpr)) csts
        inputVars <- mapM (\x -> fmap (pubDVar .
          var (codeName x) . convTypeOO) (codeType x)) inps
        constVars <- zipWithM (\c vl -> fmap (\t -> constVarFunc (conRepr g)
          (var (codeName c) (convTypeOO t)) vl) (codeType c))
          csts vals
        let getFunc Primary = primaryClass
            getFunc Auxiliary = auxClass
            f = getFunc scp
        icDesc <- inputClassDesc
        c <- f cname Nothing icDesc (inputVars ++ constVars) constructors methods
        return $ Just c
  genClass (filt ins) (filt cs)

-- | Generates a constructor for the input class, where the constructor calls the
-- input-related functions. Returns 'Nothing' if no input-related functions are
-- generated.
genInputConstructor :: (OOProg r) => GenState (Maybe (SMethod r))
genInputConstructor = do
  g <- get
  ipName <- genICName InputParameters
  giName <- genICName GetInput
  dvName <- genICName DerivedValuesFn
  icName <- genICName InputConstraintsFn
  let ds = defSet g
      genCtor False = return Nothing
      genCtor True = do
        cdesc <- inputConstructorDesc
        cparams <- getInConstructorParams
        ics <- genAllInputCalls
        ctor <- genConstructor ipName cdesc (map pcAuto cparams)
          [block ics]
        return $ Just ctor
  genCtor $ any (`elem` ds) [giName,
    dvName, icName]

-- | Generates a function for calculating derived inputs.
genInputDerived :: (OOProg r) => VisibilityTag ->
  GenState (Maybe (SMethod r))
genInputDerived s = do
  g <- get
  modify (\st -> st {currentScope = Local})
  dvName <- genICName DerivedValuesFn
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
        mthd <- getFunc s dvName desc ins outs bod
        return $ Just mthd
  genDerived $ dvName `elem` defSet g

-- | Generates function that checks constraints on the input.
genInputConstraints :: (OOProg r) => VisibilityTag ->
  GenState (Maybe (SMethod r))
genInputConstraints s = do
  g <- get
  modify (\st -> st {currentScope = Local})
  icName <- genICName InputConstraintsFn
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
        mthd <- getFunc s icName void desc (map pcAuto parms)
          Nothing [block sf, block ph]
        return $ Just mthd
  genConstraints $ icName `elem` defSet g

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

transform :: [(CodeVarChunk, [ConstraintCE])] -> [(CodeVarChunk, ConstraintCE)]
transform xs = [(s, n) | (s, ns) <- xs, n <- ns]

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | Generates conditional statements for checking constraints, where the
-- bodies depend on user's choice of constraint violation behaviour.
chooseConstr :: (OOProg r) => ConstraintBehaviour ->
  [(CodeVarChunk, [ConstraintCE])] -> GenState [MSStatement r]
chooseConstr cb cs = do
  let ch = transform cs
  varDecs <- mapM exc ch
  let varDefs = concat varDecs
  conds <- mapM (\(q,cns) -> mapM (convExpr . renderC q) cns) cs
  bods <- mapM (chooseCB cb) cs
  let bodies = concat $ zipWith (zipWith (\cond bod -> ifNoElse [((?!) cond, bod)])) conds bods
  return $ interleave varDefs bodies
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

exc :: (OOProg r) => (CodeVarChunk, ConstraintCE) ->
  GenState [MSStatement r]
exc (v, Elem _ e) = do
  lb <- convExprSet e
  t <- codeType v
  let value = var "set" (setType $ convTypeOO t) local
  return [setDecDef value lb]
exc _ = return [emptyStmt]

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
      printConstraint' (Elem _ e) = do
        lb <- convExpr e
        return $ [printStr "an element of the set ", print lb] ++ [printStrLn "."]
  printConstraint' c


-- | Don't print expressions that are just literals, because that would be
-- redundant (the values are already printed by printConstraint).
-- If expression is more than just a literal, print it in parentheses.
printExpr :: (SharedProg r) => CodeExpr -> ChunkDB -> [MSStatement r]
printExpr Lit{} _  = []
printExpr e     db = [printStr $ " (" ++ render (codeExprDoc db Implementation OneLine e) ++ ")"]

-- | | Generates a function for reading inputs from a file.
genInputFormat :: (OOProg r) => VisibilityTag ->
  GenState (Maybe (SMethod r))
genInputFormat s = do
  g <- get
  modify (\st -> st {currentScope = Local})
  dd <- genDataDesc
  giName <- genICName GetInput
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
        mthd <- getFunc s giName desc ins outs bod
        return $ Just mthd
  genInFormat $ giName `elem` defSet g

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
  if hasSampleInput (auxiliaries g) then return . Just $ sampleInput
    (sysinfodb $ codeSpec g) dd (sampleData g) else return Nothing

----- CONSTANTS -----

-- | Generates a module containing the class where constants are stored.
genConstMod :: (OOProg r) => GenState [OO.SFile r]
genConstMod = do
  cDesc <- modDesc $ liftS constModDesc
  cName <- genICName Constants
  liftS $ genModule cName cDesc [] [genConstClass Primary]

-- | Generates a class to store constants, if constants are mapped to the
-- Constants class in the class definition map, otherwise returns Nothing.
genConstClass :: (OOProg r) => ClassType ->
  GenState (Maybe (SClass r))
genConstClass scp = do
  g <- get
  modify (\st -> st {currentScope = Local})
  cname <- genICName Constants
  let cs = constants $ codeSpec g
      genClass :: (OOProg r) => [CodeDefinition] -> GenState
        (Maybe (SClass r))
      genClass [] = return Nothing
      genClass vs = do
        vals <- mapM (convExpr . (^. codeExpr)) vs
        vars <- mapM (\x -> fmap (var (codeName x) . convTypeOO)
          (codeType x)) vs
        let constVars = zipWith (constVarFunc (conRepr g)) vars vals
            getFunc Primary = primaryClass
            getFunc Auxiliary = auxClass
            f = getFunc scp
        cDesc <- constClassDesc
        cls <- f cname Nothing cDesc constVars (return []) (return [])
        return $ Just cls
  genClass $ filter (flip member (Map.filter (cname ==) (clsMap g))
    . codeName) cs

------- CALC ----------

-- | Generates a module containing calculation functions.
genCalcMod :: (OOProg r) => GenState (OO.SFile r)
genCalcMod = do
  g <- get
  cName <- genICName Calculations
  let elmap = extLibMap g
  genModuleWithImports cName calcModDesc (concatMap (^. imports) $
    elems elmap) (map (fmap Just . genCalcFunc) (execOrder $ codeSpec g)) []

-- | Generates a calculation function corresponding to the 'CodeDefinition'.
-- For solving ODEs, the 'ExtLibState' containing the information needed to
-- generate code is found by looking it up in the external library map.
genCalcFunc :: (OOProg r) => CodeDefinition ->
  GenState (SMethod r)
genCalcFunc cdef = do
  g <- get
  modify (\st -> st {currentScope = Local})
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
                   return [block (varDec v local : defStmts),
                     block stepStmts,
                     block [returnStmt $ valueOf v]])
                 (Map.lookup nm (extLibMap g))
  desc <- getComment cdef
  publicFunc
    nm
    (convTypeOO tp)
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
genOutputMod :: (OOProg r) => GenState [OO.SFile r]
genOutputMod = do
  ofName <- genICName OutputFormat
  ofDesc <- modDesc $ liftS outputFormatDesc
  liftS $ genModule ofName ofDesc [genOutputFormat] []

-- | Generates a function for printing output values.
genOutputFormat :: (OOProg r) => GenState (Maybe (SMethod r))
genOutputFormat = do
  g <- get
  modify (\st -> st {currentScope = Local})
  woName <- genICName WriteOutput
  let genOutput :: (OOProg r) => Maybe String -> GenState (Maybe (SMethod r))
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
        mthd <- publicFunc woName void desc (map pcAuto parms) Nothing
          [block $ [
          varDec var_outfile local,
          openFileW var_outfile (litString "output.txt") ] ++
          concat outp ++ [ closeFile v_outfile ]]
        return $ Just mthd
  genOutput $ Map.lookup woName (eMap g)

-- Procedural Versions --

-- | Generates a controller module.
genMainProc :: (ProcProg r) => GenState (Proc.SFile r)
genMainProc = genModuleProc "Control" "Controls the flow of the program"
  [genMainFuncProc]

-- | Generates a main function, to act as the controller for an SCS program.
-- The controller declares input and constant variables, then calls the
-- functions for reading input values, calculating derived inputs, checking
-- constraints, calculating outputs, and printing outputs.
-- Returns Nothing if the user chose to generate a library.
genMainFuncProc :: (SharedProg r) => GenState (Maybe (SMethod r))
genMainFuncProc = do
    g <- get
    let mainFunc Library = return Nothing
        mainFunc Program = do
          modify (\st -> st {currentScope = MainFn})
          v_filename <- mkVarProc (quantvar inFileName)
          logInFile <- maybeLog v_filename
          co <- initConstsProc
          ip <- getInputDeclProc
          ics <- genAllInputCallsProc
          varDef <- mapM genCalcCallProc (execOrder $ codeSpec g)
          wo <- genOutputCallProc
          return $ Just $ (if CommentFunc `elem` commented g then docMain else
            mainFunction) $ bodyStatements $ initLogFileVar (logKind g) mainFn
            ++ varDecDef v_filename mainFn (arg 0)
            : logInFile
            -- Constants must be declared before inputs because some derived
            -- input definitions or input constraints may use the constants
            ++ catMaybes [co, ip] ++ ics ++ catMaybes (varDef ++ [wo])
    mainFunc $ implType g

-- | If constants are 'Unbundled', declare them individually using 'varDecDef' if
-- representation is 'Var' and 'constDecDef' if representation is 'Const'.
-- If constants are 'Bundled' independently and representation is 'Var', throw
-- an error. If representation is 'Const', no object needs to be
-- declared because the constants will be accessed directly through the
-- Constants class.
-- If constants are 'Bundled' 'WithInputs', do 'Nothing'; declaration of the 'inParams'
-- object is handled by 'getInputDecl'.
-- If constants are 'Inlined', nothing needs to be declared.
initConstsProc :: (SharedProg r) => GenState (Maybe (MSStatement r))
initConstsProc = do
  g <- get
  let scp = convScope $ currentScope g
      cs = constants $ codeSpec g
      getDecl (Store Unbundled) _ = declVars
      getDecl (Store Bundled) _ = error "initConstsProc: Procedural renderers do not support bundled constants."
      getDecl WithInputs Unbundled = declVars
      getDecl WithInputs Bundled = return Nothing
      getDecl Inline _ = return Nothing
      declVars = do
        vars <- mapM (mkVarProc . quantvar) cs
        vals <- mapM (convExprProc . (^. codeExpr)) cs
        logs <- mapM maybeLog vars
        return $ Just $ multi $
          zipWith (\vr -> defFunc (conRepr g) vr scp) vars vals ++ concat logs
      defFunc Var = varDecDef
      defFunc Const = constDecDef
  getDecl (conStruct g) (inStruct g)

-- | Checks if a class is needed to store constants, i.e. if constants are
-- mapped to the constants class in the class definition map.
checkConstClass :: GenState Bool
checkConstClass = do
  g <- get
  cName <- genICName Constants
  let cs = constants $ codeSpec g
      checkClass :: [CodeDefinition] -> GenState Bool
      checkClass [] = return False
      checkClass _ = return True
  checkClass $ filter (flip member (Map.filter (cName ==) (clsMap g))
    . codeName) cs

-- | Generates a single module containing all input-related components.
genInputModProc :: (ProcProg r) => GenState [Proc.SFile r]
genInputModProc = do
  ipDesc <- modDesc inputParametersDesc
  cname <- genICName InputParameters
  let genMod :: (ProcProg r) => Bool ->
        GenState (Proc.SFile r)
      genMod False = genModuleProc cname ipDesc [genInputFormatProc Pub,
        genInputDerivedProc Pub, genInputConstraintsProc Pub]
      genMod True = error "genInputModProc: Procedural renderers do not support bundled inputs"
  ic <- checkInputClass
  liftS $ genMod ic

-- | Returns 'False' if no inputs or constants are mapped to InputParameters in
-- the class definition map.
-- Returns 'True' If any inputs or constants are defined in InputParameters
checkInputClass :: GenState Bool
checkInputClass = do
  g <- get
  cname <- genICName InputParameters
  let ins = inputs $ codeSpec g
      cs = constants $ codeSpec g
      filt :: (CodeIdea c) => [c] -> [c]
      filt = filter ((Just cname ==) . flip Map.lookup (clsMap g) . codeName)
      checkClass :: [CodeVarChunk] -> [CodeDefinition] -> GenState Bool
      checkClass [] [] = return False
      checkClass _ _ = return True
  checkClass (filt ins) (filt cs)

-- | If there are no inputs, return nothing.
-- If there are inputs and they are not exported by any module, then they are
-- 'Unbundled' and are declared individually using 'varDec'.
-- If there are inputs and they are exported by a module, they are 'Bundled' in
-- the InputParameters class, so 'inParams' should be declared and constructed,
-- using 'objDecNew' if the inputs are exported by the current module, and
-- 'extObjDecNew' if they are exported by a different module.
getInputDeclProc :: (SharedProg r) => GenState (Maybe (MSStatement r))
getInputDeclProc = do
  g <- get
  let scp = convScope $ currentScope g
      getDecl ([],[]) = return Nothing
      getDecl ([],ins) = do
        vars <- mapM mkVarProc ins
        return $ Just $ multi $ map (`varDec` scp) vars
      getDecl _ = error "getInputDeclProc: Procedural renderers do not support bundled inputs"
  getDecl (partition (flip member (eMap g) . codeName)
    (inputs $ codeSpec g))

-- | Generates a module containing calculation functions.
genCalcModProc :: (ProcProg r) => GenState (Proc.SFile r)
genCalcModProc = do
  g <- get
  cName <- genICName Calculations
  let elmap = extLibMap g
  genModuleWithImportsProc cName calcModDesc (concatMap (^. imports) $
    elems elmap) (map (fmap Just . genCalcFuncProc) (execOrder $ codeSpec g))

-- | Generates a calculation function corresponding to the 'CodeDefinition'.
-- For solving ODEs, the 'ExtLibState' containing the information needed to
-- generate code is found by looking it up in the external library map.
genCalcFuncProc :: (SharedProg r) => CodeDefinition ->
  GenState (SMethod r)
genCalcFuncProc cdef = do
  g <- get
  modify (\st -> st {currentScope = Local})
  parms <- getCalcParams cdef
  let nm = codeName cdef
  tp <- codeType cdef
  v <- mkVarProc (quantvar cdef)
  blcks <- case cdef ^. defType
            of Definition -> liftS $ genCalcBlockProc CalcReturn cdef
                 (cdef ^. codeExpr)
               ODE -> maybe (error $ nm ++ " missing from ExtLibMap")
                 (\el -> do
                   defStmts <- mapM convStmtProc (el ^. defs)
                   stepStmts <- mapM convStmtProc (el ^. steps)
                   return [block (varDec v local : defStmts),
                     block stepStmts,
                     block [returnStmt $ valueOf v]])
                 (Map.lookup nm (extLibMap g))
  desc <- getComment cdef
  publicFuncProc
    nm
    (convType tp)
    ("Calculates " ++ desc)
    (map pcAuto parms)
    (Just desc)
    blcks

-- | Generates a calculation block for the given 'CodeDefinition', and assigns the
-- result to a variable (if 'CalcAssign') or returns the result (if 'CalcReturn').
genCalcBlockProc :: (SharedProg r) => CalcType -> CodeDefinition -> CodeExpr ->
  GenState (MSBlock r)
genCalcBlockProc t v (Case c e) = genCaseBlockProc t v c e
genCalcBlockProc CalcAssign v e = do
  vv <- mkVarProc (quantvar v)
  ee <- convExprProc e
  l <- maybeLog vv
  return $ block $ assign vv ee : l
genCalcBlockProc CalcReturn _ e = block <$> liftS (returnStmt <$> convExprProc e)

-- | Generates a calculation block for a value defined by cases.
-- If the function is defined for every case, the final case is captured by an
-- else clause, otherwise an error-throwing else-clause is generated.
genCaseBlockProc :: (SharedProg r) => CalcType -> CodeDefinition -> Completeness
  -> [(CodeExpr, CodeExpr)] -> GenState (MSBlock r)
genCaseBlockProc _ _ _ [] = error $ "Case expression with no cases encountered" ++
  " in code generator"
genCaseBlockProc t v c cs = do
  ifs <- mapM (\(e,r) -> liftM2 (,) (convExprProc r) (calcBody e)) (ifEs c)
  els <- elseE c
  return $ block [ifCond ifs els]
  where calcBody e = fmap body $ liftS $ genCalcBlockProc t v e
        ifEs Complete = init cs
        ifEs Incomplete = cs
        elseE Complete = calcBody $ fst $ last cs
        elseE Incomplete = return $ oneLiner $ throw $
          "Undefined case encountered in function " ++ codeName v

-- | | Generates a function for reading inputs from a file.
genInputFormatProc :: (SharedProg r) => VisibilityTag ->
  GenState (Maybe (SMethod r))
genInputFormatProc s = do
  g <- get
  modify (\st -> st {currentScope = Local})
  dd <- genDataDesc
  giName <- genICName GetInput
  let getFunc Pub = publicInOutFuncProc
      getFunc Priv = privateInOutFuncProc
      genInFormat :: (SharedProg r) => Bool -> GenState
        (Maybe (SMethod r))
      genInFormat False = return Nothing
      genInFormat _ = do
        ins <- getInputFormatIns
        outs <- getInputFormatOuts
        bod <- readDataProc dd
        desc <- inFmtFuncDesc
        mthd <- getFunc s giName desc ins outs bod
        return $ Just mthd
  genInFormat $ giName `elem` defSet g

-- | Generates a function for calculating derived inputs.
genInputDerivedProc :: (SharedProg r) => VisibilityTag ->
  GenState (Maybe (SMethod r))
genInputDerivedProc s = do
  g <- get
  modify (\st -> st {currentScope = Local})
  dvName <- genICName DerivedValuesFn
  let dvals = derivedInputs $ codeSpec g
      getFunc Pub = publicInOutFuncProc
      getFunc Priv = privateInOutFuncProc
      genDerived :: (SharedProg r) => Bool -> GenState
        (Maybe (SMethod r))
      genDerived False = return Nothing
      genDerived _ = do
        ins <- getDerivedIns
        outs <- getDerivedOuts
        bod <- mapM (\x -> genCalcBlockProc CalcAssign x (x ^. codeExpr)) dvals
        desc <- dvFuncDesc
        mthd <- getFunc s dvName desc ins outs bod
        return $ Just mthd
  genDerived $ dvName `elem` defSet g

-- | Generates function that checks constraints on the input.
genInputConstraintsProc :: (SharedProg r) => VisibilityTag ->
  GenState (Maybe (SMethod r))
genInputConstraintsProc s = do
  g <- get
  modify (\st -> st {currentScope = Local})
  icName <- genICName InputConstraintsFn
  let cm = cMap $ codeSpec g
      getFunc Pub = publicFuncProc
      getFunc Priv = privateFuncProc
      genConstraints :: (SharedProg r) => Bool -> GenState
        (Maybe (SMethod r))
      genConstraints False = return Nothing
      genConstraints _ = do
        parms <- getConstraintParams
        let varsList = filter (\i -> member (i ^. uid) cm) (inputs $ codeSpec g)
            sfwrCs   = map (sfwrLookup cm) varsList
            physCs   = map (physLookup cm) varsList
        sf <- sfwrCBodyProc sfwrCs
        ph <- physCBodyProc physCs
        desc <- inConsFuncDesc
        mthd <- getFunc s icName void desc (map pcAuto parms)
          Nothing [block sf, block ph]
        return $ Just mthd
  genConstraints $ icName `elem` defSet g

-- | Generates input constraints code block for checking software constraints.
sfwrCBodyProc :: (SharedProg r) => [(CodeVarChunk, [ConstraintCE])] ->
  GenState [MSStatement r]
sfwrCBodyProc cs = do
  g <- get
  let cb = onSfwrC g
  chooseConstrProc cb cs

-- | Generates input constraints code block for checking physical constraints.
physCBodyProc :: (SharedProg r) => [(CodeVarChunk, [ConstraintCE])] ->
  GenState [MSStatement r]
physCBodyProc cs = do
  g <- get
  let cb = onPhysC g
  chooseConstrProc cb cs

-- | Generates conditional statements for checking constraints, where the
-- bodies depend on user's choice of constraint violation behaviour.
chooseConstrProc :: (SharedProg r) => ConstraintBehaviour ->
  [(CodeVarChunk, [ConstraintCE])] -> GenState [MSStatement r]
chooseConstrProc cb cs = do
  conds <- mapM (\(q,cns) -> mapM (convExprProc . renderC q) cns) cs
  bods <- mapM (chooseCB cb) cs
  return $ concat $ zipWith (zipWith (\cond bod -> ifNoElse [((?!) cond, bod)]))
    conds bods
  where chooseCB Warning = constrWarnProc
        chooseCB Exception = constrExcProc

-- | Generates body defining constraint violation behaviour if Warning chosen from 'chooseConstr'.
-- Prints a \"Warning\" message followed by a message that says
-- what value was \"suggested\".
constrWarnProc :: (SharedProg r) => (CodeVarChunk, [ConstraintCE]) ->
  GenState [MSBody r]
constrWarnProc c = do
  let q = fst c
      cs = snd c
  msgs <- mapM (constraintViolatedMsgProc q "suggested") cs
  return $ map (bodyStatements . (printStr "Warning: " :)) msgs

-- | Generates body defining constraint violation behaviour if Exception chosen from 'chooseConstr'.
-- Prints a message that says what value was \"expected\",
-- followed by throwing an exception.
constrExcProc :: (SharedProg r) => (CodeVarChunk, [ConstraintCE]) ->
  GenState [MSBody r]
constrExcProc c = do
  let q = fst c
      cs = snd c
  msgs <- mapM (constraintViolatedMsgProc q "expected") cs
  return $ map (bodyStatements . (++ [throw "InputError"])) msgs

-- | Generates statements that print a message for when a constraint is violated.
-- Message includes the name of the cosntraint quantity, its value, and a
-- description of the constraint that is violated.
constraintViolatedMsgProc :: (SharedProg r) => CodeVarChunk -> String ->
  ConstraintCE -> GenState [MSStatement r]
constraintViolatedMsgProc q s c = do
  pc <- printConstraintProc c
  v <- mkValProc (quantvar q)
  return $ [printStr $ codeName q ++ " has value ",
    print v,
    printStr $ ", but is " ++ s ++ " to be "] ++ pc

-- | Generates statements to print descriptions of constraints, using words and
-- the constrained values. Constrained values are followed by printing the
-- expression they originated from, using printExpr.
printConstraintProc :: (SharedProg r) => ConstraintCE ->
  GenState [MSStatement r]
printConstraintProc c = do
  g <- get
  let db = sysinfodb $ codeSpec g
      printConstraint' :: (SharedProg r) => ConstraintCE -> GenState
        [MSStatement r]
      printConstraint' (Range _ (Bounded (_, e1) (_, e2))) = do
        lb <- convExprProc e1
        ub <- convExprProc e2
        return $ [printStr "between ", print lb] ++ printExpr e1 db ++
          [printStr " and ", print ub] ++ printExpr e2 db ++ [printStrLn "."]
      printConstraint' (Range _ (UpTo (_, e))) = do
        ub <- convExprProc e
        return $ [printStr "below ", print ub] ++ printExpr e db ++
          [printStrLn "."]
      printConstraint' (Range _ (UpFrom (_, e))) = do
        lb <- convExprProc e
        return $ [printStr "above ", print lb] ++ printExpr e db ++ [printStrLn "."]
      printConstraint' (Elem _ e) = do
        lb <- convExprProc e
        return $ [printStr "an element of the set ", print lb] ++ [printStrLn "."]
  printConstraint' c

-- | Generates a module containing the function for printing outputs.
genOutputModProc :: (ProcProg r) => GenState [Proc.SFile r]
genOutputModProc = do
  ofName <- genICName OutputFormat
  ofDesc <- modDesc $ liftS outputFormatDesc
  liftS $ genModuleProc ofName ofDesc [genOutputFormatProc]

-- | Generates a function for printing output values.
genOutputFormatProc :: (SharedProg r) => GenState (Maybe (SMethod r))
genOutputFormatProc = do
  g <- get
  modify (\st -> st {currentScope = Local})
  woName <- genICName WriteOutput
  let genOutput :: (SharedProg r) => Maybe String -> GenState (Maybe (SMethod r))
      genOutput Nothing = return Nothing
      genOutput (Just _) = do
        let l_outfile = "outputfile"
            var_outfile = var l_outfile outfile
            v_outfile = valueOf var_outfile
        parms <- getOutputParams
        outp <- mapM (\x -> do
          v <- mkValProc x
          return [ printFileStr v_outfile (codeName x ++ " = "),
                   printFileLn v_outfile v
                 ] ) (outputs $ codeSpec g)
        desc <- woFuncDesc
        mthd <- publicFuncProc woName void desc (map pcAuto parms) Nothing
          [block $ [
          varDec var_outfile local,
          openFileW var_outfile (litString "output.txt") ] ++
          concat outp ++ [ closeFile v_outfile ]]
        return $ Just mthd
  genOutput $ Map.lookup woName (eMap g)
