module Language.Drasil.Code.Imperative.Import(generator, generateCode) where

import Language.Drasil hiding (int, getLabel, Label)
import Language.Drasil.Code.Code as C (CodeType(List, File, Char, Float, Object, 
  String, Boolean, Integer))
-- import Language.Drasil.Code.Imperative.AST as I hiding ((&=), State, assign, return, 
--   Not, Tan, Cos, Sin, Exp, Abs, Log, Ln, And, Or)
-- import qualified Language.Drasil.Code.Imperative.AST as I (assign, return)
import qualified Language.Drasil.Code.Imperative.New as I (
  RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), Selector(..), FunctionSym(..), SelectorFunction(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.LanguageRenderer (Options(..))
import Language.Drasil.Code.Imperative.Parsers.ConfigParser (pythonLabel, cppLabel, cSharpLabel, javaLabel)
-- import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Code.Imperative.NewLanguageRenderer (createCodeFiles, makeCode) -- potentially move to CodeGeneration.hs
import Language.Drasil.Chunk.Code (CodeChunk, CodeDefinition, codeName, codeType, 
  codevar, codefunc, codeEquat, funcPrefix, physLookup, sfwrLookup, programName)
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))
import qualified Language.Drasil.CodeSpec as CS (Mod(..))
import Language.Drasil.Code.DataDesc (Ind(WithPattern, WithLine, Explicit), 
  Entry(JunkEntry, ListEntry, Entry), LinePattern(Repeat, Straight), 
  Data(Line, Lines, JunkData, Singleton), DataDesc, getInputs)

import Prelude hiding (log, exp, const)
import Data.List (intersperse, (\\), stripPrefix)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Data.Map (member)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe)
import Control.Lens ((^.))
import Control.Monad (when,liftM2,liftM3,zipWithM)
import Control.Monad.Reader (Reader, ask, runReader, withReader)

-- Private State, used to push these options around the generator
data State repr = State {
  codeSpec :: CodeSpec,
  inStruct :: Structure,
  logName :: String,
  logKind :: Logging,
  commented :: Comments,
  currentModule :: String,

  sfwrCBody :: (I.RenderSym repr) => Expr -> (repr (I.Body repr)),
  physCBody :: (I.RenderSym repr) => Expr -> (repr (I.Body repr))
}

-- function to choose how to deal with
-- 1. constraints
-- 2. how to structure the input "module"
-- 3. logging assignments
chooseConstr :: ConstraintBehaviour -> Expr -> Body
chooseConstr Warning   = constrWarn
chooseConstr Exception = constrExc

chooseInStructure :: (I.RenderSym repr) => Structure -> Reader State [(repr (I.Module repr))]
chooseInStructure Loose   = genInputModNoClass
chooseInStructure AsClass = genInputModClass

chooseLogging :: (I.RenderSym repr) => Logging -> ((repr (I.Value repr)) -> (repr (I.Value repr)) -> Reader State (repr (I.Statement repr)))
chooseLogging LogVar = loggedAssign
chooseLogging LogAll = loggedAssign
chooseLogging _      = (\x y -> return $ I.assign x y)

generator :: Choices -> CodeSpec -> State
generator chs spec = State {
  -- constants
  codeSpec = spec,
  inStruct = inputStructure chs,
  logKind  = logging chs,
  commented = comments chs,
  -- state
  currentModule = "",

  -- next depend on chs
  logName = logFile chs,
  sfwrCBody = chooseConstr $ onSfwrConstraint chs,
  physCBody = chooseConstr $ onPhysConstraint chs
}

assign :: (I.RenderSym repr) => (repr (I.Value repr)) -> (repr (I.Value repr)) -> Reader State (repr (I.Statement repr))
assign x y = do
  g <- ask
  chooseLogging (logKind g) x y

publicMethod :: (I.RenderSym repr) => (repr (I.MethodType repr)) -> Label -> [repr (I.Parameter repr)] -> [repr (I.StateType repr)] -> [Label] -> Reader State (repr (I.Body repr)) -> Reader State (repr (I.Method repr))
publicMethod mt l pl st v u = do
  g <- ask
  genMethodCall public static (commented g) (logKind g) mt l pl st v u

generateCode :: Choices -> State -> IO ()
generateCode chs g =
  do workingDir <- getCurrentDirectory
     mapM_ (\x -> do
          createDirectoryIfMissing False (getDir x)
          setCurrentDirectory (getDir x)
          when (x == Java) $ createDirectoryIfMissing False prog
          when (x == Java) $ setCurrentDirectory prog
          createCodeFiles $ makeCode (map (getUnRepr x) files) (getExt x)
          setCurrentDirectory workingDir) (lang $ chs)
  where prog = case codeSpec g of { CodeSpec {program = pp} -> programName pp }
        files = runReader genFiles g

genFiles :: Reader State [(repr (I.RenderFile repr))]
genFiles = do
  g <- ask
  return $ map fileDoc $ genModules g

genModules :: Reader State [(repr (I.Module repr))]
genModules = do
  g <- ask
  let s = codeSpec g
  mn     <- genMain
  inp    <- chooseInStructure $ inStruct g
  out    <- genOutputMod $ outputs s
  moddef <- sequence $ fmap genModDef (mods s) -- hack ?
  return $ (mn : inp ++ out ++ moddef)

-- private utilities used in generateCode
getLabel, getDir :: Lang -> String
getLabel Cpp = cppLabel
getLabel CSharp = cSharpLabel
getLabel Java = javaLabel
getLabel Python = pythonLabel
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"

getExt :: Lang -> [Label]
getExt Java = [".java"]
getExt Python = [".py"]

getUnRepr :: (I.RenderSym repr) => Lang -> (repr (I.RenderFile repr) -> Doc)
getUnRepr Java = unJC
getUnRepr Python = unPC

liftS :: Reader a b -> Reader a [b]
liftS = fmap (\x -> [x])

------- INPUT ----------

genInputModClass :: (I.RenderSym repr) => Reader State [(repr (I.Module repr))]
genInputModClass =
  sequence $ [ genModule "InputParameters" Nothing (Just $ liftS genInputClass),
               genModule "DerivedValues" (Just $ liftS genInputDerived) Nothing,
               genModule "InputConstraints" (Just $ liftS genInputConstraints) Nothing
             ]

genInputModNoClass :: (I.RenderSym repr) => Reader State [(repr (I.Module repr))]
genInputModNoClass = do
  g <- ask
  let ins = inputs $ codeSpec g
  inpDer    <- genInputDerived
  inpConstr <- genInputConstraints
  return $ [ buildModule "InputParameters" []
             (map (\x -> varDecDef (codeName x) (convType $ codeType x) (getDefaultValue $ codeType x)) ins)
             [inpDer , inpConstr]
             []
           ]

genInputClass :: (I.RenderSym repr) => Reader State (repr (I.Class repr))
genInputClass = do
  g <- ask
  let ins          = inputs $ codeSpec g
      inputVars    =
          map (\x -> pubMVar 0 (codeName x) (convType $ codeType x)) ins
      varsList     = map (\x -> self $-> var (codeName x)) ins
      vals         = map (getDefaultValue . codeType) ins
  asgs <- zipWithM assign varsList vals
  return $ pubClass "InputParameters" Nothing inputVars
    [ constructor "InputParameters" [] (body [block asgs]) ]

genInputConstraints :: (I.RenderSym repr) => Reader State (repr (I.Method repr))
genInputConstraints = do
  g <- ask
  let varsList = inputs $ codeSpec g
      cm       = cMap $ codeSpec g
      sfwrCs   = concatMap (renderC . sfwrLookup cm) varsList
      physCs   = concatMap (renderC . physLookup cm) varsList
  parms <- getParams varsList
  types <- getParamTypes varsList
  names <- getParamNames varsList
  sf <- mapM (\x -> do { e <- convExpr x; return $ ifNoElse [((?!) e, sfwrCBody g x)]}) sfwrCs
  hw <- mapM (\x -> do { e <- convExpr x; return $ ifNoElse [((?!) e, physCBody g x)]}) physCs
  publicMethod void "input_constraints" parms types names
      (return $ body [ sf ++ hw ])

genInputDerived :: (I.RenderSym repr) => Reader State (repr (I.Method repr))
genInputDerived = do
  g <- ask
  let dvals = derivedInputs $ codeSpec g
  parms <- getParams $ map codevar dvals
  types <- getParamTypes $ map codevar dvals
  names <- getParamNames $ map codevar dvals
  inps <- mapM (\x -> genCalcBlock CalcAssign (codeName x) (codeEquat x)) dvals
  publicMethod void "derived_values" parms types names
      (return $ concat inps)

-- need Expr -> String to print constraint
constrWarn :: Expr -> Body
constrWarn _ = oneLiner $ printStrLn "Warning: constraint violated"

constrExc :: Expr -> Body
constrExc _ = oneLiner $ throw "InputError"

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
genCalcFunc :: CodeDefinition -> Reader State Method
genCalcFunc cdef = do
  g <- ask
  parms <- getParams (codevars' (codeEquat cdef) $ sysinfodb $ codeSpec g)
  publicMethod
    (methodType $ convType (codeType cdef))
    (codeName cdef)
    parms
    (genCalcBlock CalcReturn (codeName cdef) (codeEquat cdef))

data CalcType = CalcAssign | CalcReturn deriving Eq

genCalcBlock :: (I.RenderSym repr) => CalcType -> String -> Expr -> Reader State (repr (I.Body repr))
genCalcBlock t' v' e' = do
  doit t' v' e'
    where
    doit :: (I.RenderSym repr) => CalcType -> String -> Expr -> Reader State (repr (I.Body repr))
    doit t v (Case e)    = genCaseBlock t v e
    doit t v e
      | t == CalcAssign  = fmap oneLiner $ do { vv <- variable v; ee <- convExpr e; assign vv ee}
      | otherwise        = fmap (oneLiner . I.returnState) $ convExpr e

genCaseBlock :: (I.RenderSym repr) => CalcType -> String -> [(Expr,Relation)] -> Reader State (repr (I.Body repr))
genCaseBlock t v cs = do
  ifs <- mapM (\(e,r) -> liftM2 (,) (convExpr e) (genCalcBlock t v r)) cs
  return $ body $ [ifNoElse ifs]

----- OUTPUT -------

genOutputMod :: (I.RenderSym repr) => [CodeChunk] -> Reader State [(repr (I.Module repr))]
genOutputMod outs = liftS $ genModule "OutputFormat" (Just $ liftS $ genOutputFormat outs) Nothing

genOutputFormat :: (I.RenderSym repr) => [CodeChunk] -> Reader State (repr (I.Method repr))
genOutputFormat outs =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    parms <- getParams outs
    types <- getParamTypes outs
    names <- getParamNames outs
    outp <- mapM (\x -> do
        v <- variable $ codeName x
        return [ printFileStr v_outfile ((codeName x) ++ " = "),
                 printFileLn v_outfile (convType $ codeType x) v
               ] ) outs
    publicMethod void "write_output" parms types names (return bodyStatements $
      [
      varDec l_outfile outfile,
      openFileW v_outfile (litString "output.txt") ] ++
      concat outp ++ [ closeFile v_outfile ])

-----

genMethodCall :: (I.RenderSym repr) => (repr (I.Scope repr)) -> (repr (I.Permanence repr)) -> Comments -> Logging -> (repr (I.MethodType repr)) -> Label -> [repr (I.Parameter repr)] -> [repr (I.StateType repr)] -> [Label]
                  -> Reader State (repr (I.Body repr)) -> Reader State (repr I.Method repr))
genMethodCall s pr doComments doLog t n p st l b = do
  let loggedBody LogFunc = loggedMethod n st l b
      loggedBody LogAll  = loggedMethod n st l b
      loggedBody _       = b
      commBody CommentFunc = commMethod n l
      commBody _           = id
  bod <- commBody doComments (loggedBody doLog)
  return $ method n s pr t p bod

commMethod :: (I.RenderSym repr) => Label -> [Label] -> Reader State (repr (I.Body repr)) -> Reader State (repr (I.Body repr))
commMethod n l b = do
  g <- ask
  rest <- b
  return $ (
    body $ [ block [
      comment $ "function '" ++ n ++ "': " ++ (funcTerm n (fMap $ codeSpec g)),
      multi $ map
        (\x -> comment $ "parameter '" ++ x ++ "': " ++ (varTerm x (vMap $ codeSpec g))) l
    ]]) : rest 

loggedMethod :: (I.RenderSym repr) => Label -> [repr (I.StateType repr)] -> [Label] -> Reader State (repr (I.Body repr)) -> Reader State (repr I.Body repr))
loggedMethod n st l b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    g <- ask
    rest <- b
    return $ ( block [
      varDec l_outfile outfile,
      openFileW v_outfile (litString $ logName g),
      printFileStr v_outfile ("function " ++ n ++ "("),
      printParams st v v_outfile,
      printFileStrLn v_outfile ") called",
      closeFile v_outfile ] )
      : rest
  where
    printParams st v v_outfile = multi $
      intersperse (printFileStr v_outfile ", ") $
      map (\x y -> printFile v_outfile x (var y)) st l

---- MAIN ---

genModule :: (I.RenderSym repr) => Name
               -> Maybe (Reader State [(repr (I.Method repr))])
               -> Maybe (Reader State [(repr (I.Class repr))])
               -> Reader State (repr (I.Module repr))
genModule n maybeMs maybeCs = do
  g <- ask
  let ls = maybe [] id (Map.lookup n (dMap $ codeSpec g))
      updateState = withReader (\s -> s { currentModule = n })
  cs <- maybe (return []) updateState maybeCs
  ms <- maybe (return []) updateState maybeMs
  return $ buildModule n ls [] ms cs


genMain :: (I.RenderSym repr) => Reader State (repr (I.Module repr))
genMain = genModule "Control" (Just $ liftS $ genMainFunc) Nothing

genMainFunc :: (I.RenderSym repr) => Reader State (repr (I.Method repr))
genMainFunc =
  let l_filename = "inputfile"
      v_filename = var l_filename
      l_params = "inParams"
      v_params = var l_params
  in do
    g <- ask
    let args1 x = getArgs $ codevars' (codeEquat x) $ sysinfodb $ codeSpec g
    args2 <- getArgs $ outputs $ codeSpec g
    gi <- fApp (funcPrefix ++ "get_input") [v_filename, v_params]
    dv <- fApp "derived_values" [v_params]
    ic <- fApp "input_constraints" [v_params]
    varDef <- mapM (\x -> do
      args <- args1 x
      cnargs <- fApp (codeName x) args
      return $ varDecDef (nopfx $ codeName x) (convType $ codeType x) cnargs) (execOrder $ codeSpec g)
    wo <- fApp "write_output" args2
    return $ mainMethod $ bodyStatements $ [
      varDecDef l_filename string $ arg 0 ,
      extObjDecNewVoid l_params "InputParameters" (obj "InputParameters") ,
      valState gi,
      valState dv,
      valState ic
      ] ++ varDef ++ [ valState wo ]

-----

loggedAssign :: (I.RenderSym repr) => (repr (I.Value repr)) -> (repr (I.Value repr)) -> Reader State (repr (I.Statement repr))
loggedAssign a b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    g <- ask
    return $ multi [
      I.assign a b,
      varDec l_outfile outfile,
      openFileW v_outfile (litString $ logName g),
      printFileStr v_outfile ("var '" ++ (valName a) ++ "' assigned to "),
      printFile v_outfile (convType $ varType (valName b) (vMap $ codeSpec g)) b,
      printFileStrLn v_outfile (" in module " ++ currentModule g),
      closeFile v_outfile ]

-- helpers

nopfx :: String -> String
nopfx s = maybe s id (stripPrefix funcPrefix s)

variable :: (I.RenderSym repr) => String -> Reader State (repr (I.Value repr))
variable s' = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
      doit :: (I.RenderSym repr) => String -> Reader State (repr (I.Value repr))
      doit s | member s mm =
        maybe (error "impossible") ((convExpr) . codeEquat) (Map.lookup s mm) --extvar "Constants" s
             | s `elem` (map codeName $ inputs cs) = return $ (var "inParams")$->(var s)
             | otherwise                        = return $ var s
  doit s'
  
fApp :: (I.RenderSym repr) => String -> [(repr (I.Value repr))] -> Reader State (repr (I.Value repr))
fApp s' vl' = do
  g <- ask
  let doit :: String -> [Value] -> Value
      doit s vl | member s (eMap $ codeSpec g) =
        maybe (error "impossible")
          (\x -> if x /= currentModule g then extFuncApp x s vl else funcApp s vl)
          (Map.lookup s (eMap $ codeSpec g))
                | otherwise = funcApp s vl
  return $ doit s' vl'

getParams :: (I.RenderSym repr) => [CodeChunk] -> Reader State [(repr (I.Parameter repr))]
getParams cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      ps = map (\y -> param (codeName y) (convType $ codeType y))
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g)) csSubIns)
  return $ if length csSubIns < length cs
           then (param "inParams" (obj "InputParameters")):ps  -- todo:  make general
           else ps

getParamTypes :: (I.RenderSym repr) => [CodeChunk] -> Reader State [(repr (I.StateType repr))]
getParamTypes cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      pts = map (\y -> convType $ codeType y)
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g)) csSubIns)
  return $ if length csSubIns < length cs
          then (obj "InputParameters"):pts  -- todo:  make general
          else pts

getParamNames :: [CodeChunk] -> Reader State [Label]
getParamNames cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      pvs = map (\y -> codeName y)
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g)) csSubIns)
  return $ if length csSubIns < length cs
          then ("inParams"):pvs  -- todo:  make general
          else pvs

getArgs :: (I.RenderSym repr) => [CodeChunk] -> Reader State [(repr (I.Value repr))]
getArgs cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      args = map (var . codeName)
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g)) csSubIns)
  return $ if length csSubIns < length cs
           then (var "inParams"):args  -- todo:  make general
           else args

paramName :: Parameter -> String
paramName (StateParam l _) = l
paramName (FuncParam _ _ _) = error "Function param not implemented"

valName :: Value -> String
valName (Lit (LitBool b)) = show b
valName (Lit (LitInt i)) = show i
valName (Lit (LitFloat f)) = show f
valName (Lit (LitChar c)) = [c]
valName (Lit (LitStr s)) = s
valName (Var _ n) = n
valName (ObjVar o v) = valName o ++ "." ++ valName v
valName (ObjAccess o (ListAccess v)) = valName o ++ "[" ++ valName v ++ "]"
valName _ = error "Value has no name"

getDefaultValue :: (I.RenderSym repr) => C.CodeType -> (repr (I.Value repr))
getDefaultValue C.Boolean = defaultBool
getDefaultValue C.Integer = defaultInt
getDefaultValue C.Float = defaultFloat
getDefaultValue C.Char = defaultChar
getDefaultValue C.String = defaultString
getDefaultValue _ = error "No default value for the given type"

convType :: (I.RenderSym repr) => C.CodeType -> (repr (I.StateType repr))
convType C.Boolean = bool
convType C.Integer = int
convType C.Float = float
convType C.Char = char
convType C.String = string
convType (C.List t) = listType dynamic $ convType t -- new GOOL has different functions for intListType, floatListType, etc... Probably need to add these to CodeType
convType (C.Object n) = obj n
convType (C.File) = error "convType: File ?"

convExpr :: Expr -> Reader State (repr (I.Value repr))
convExpr (Dbl d)      = return $ litFloat d
convExpr (Int i)      = return $ litInt i
convExpr (Str s)      = return $ litString s
convExpr (AssocA Add l)  = fmap (foldr1 (#+)) $ sequence $ map convExpr l
convExpr (AssocA Mul l)  = fmap (foldr1 (#*)) $ sequence $ map convExpr l
convExpr (AssocB And l)  = fmap (foldr1 (?&&)) $ sequence $ map convExpr l
convExpr (AssocB Or l)  = fmap (foldr1 (?||)) $ sequence $ map convExpr l
convExpr (Deriv _ _ _) = return $ litString "**convExpr :: Deriv unimplemented**"
convExpr (C c)         = do
  g <- ask
  variable $ codeName $ codevar (symbLookup c ((sysinfodb $ codeSpec g) ^. symbolTable))
convExpr  (FCall (C c) x)  = do
  g <- ask
  let info = sysinfodb $ codeSpec g
  args <- mapM convExpr x
  fApp (codeName (codefunc (symbLookup c (info ^. symbolTable)))) args
convExpr (FCall _ _)   = return $ litString "**convExpr :: FCall unimplemented**"
convExpr (UnaryOp o u) = fmap (unop o) (convExpr u)
convExpr (BinaryOp Frac (Int a) (Int b)) =
  return $ (litFloat $ fromIntegral a) #/ (litFloat $ fromIntegral b) -- hack to deal with integer division
convExpr  (BinaryOp o a b)  = liftM2 (bfunc o) (convExpr a) (convExpr b)
convExpr  (Case l)      = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen"
    doit [(e,_)] = convExpr e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExpr cond) (convExpr e) (convExpr (Case xs))
convExpr (Matrix _)    = error "convExpr: Matrix"
convExpr (Operator _ _ _) = error "convExpr: Operator"
convExpr (IsIn _ _)    = error "convExpr: IsIn"
convExpr (RealI c ri)  = do
  g <- ask
  convExpr $ renderRealInt (lookupC (sysinfodb $ codeSpec g) c) ri

lookupC :: HasSymbolTable s => s -> UID -> QuantityDict
lookupC sm c = symbLookup c $ sm^.symbolTable

renderC :: (HasUID c, HasSymbol c) => (c, [Constraint]) -> [Expr]
renderC (u, l) = map (renderC' u) l

renderC' :: (HasUID c, HasSymbol c) => c -> Constraint -> Expr
renderC' s (Range _ rr)          = renderRealInt s rr
renderC' s (EnumeratedReal _ rr) = IsIn (sy s) (DiscreteD rr)
renderC' s (EnumeratedStr _ rr)  = IsIn (sy s) (DiscreteS rr)

renderRealInt :: (HasUID c, HasSymbol c) => c -> RealInterval Expr Expr -> Expr
renderRealInt s (Bounded (Inc,a) (Inc,b)) = (a $<= sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Inc,a) (Exc,b)) = (a $<= sy s) $&& (sy s $<  b)
renderRealInt s (Bounded (Exc,a) (Inc,b)) = (a $<  sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Exc,a) (Exc,b)) = (a $<  sy s) $&& (sy s $<  b)
renderRealInt s (UpTo (Inc,a))    = sy s $<= a
renderRealInt s (UpTo (Exc,a))    = sy s $< a
renderRealInt s (UpFrom (Inc,a))  = sy s $>= a
renderRealInt s (UpFrom (Exc,a))  = sy s $>  a

unop :: UFunc -> ((repr (I.Value repr)) -> (repr (I.Value repr)))
unop Sqrt = (#/^)
unop Log  = I.log
unop Ln   = I.ln
unop Abs  = (#|)
unop Exp  = I.exp
unop Sin  = I.sin
unop Cos  = I.cos
unop Tan  = I.tan
unop Csc  = I.csc
unop Sec  = I.sec
unop Cot  = I.cot
unop Dim  = (I.$.listSize)
unop Norm = error "unop: Norm not implemented"
unop Not  = (?!)
unop Neg  = (#~)

bfunc :: (I.RenderSym repr) => BinOp -> ((repr (I.Value repr)) -> (repr (I.Value repr)) -> (repr (I.Value repr)))
bfunc Eq    = (?==)
bfunc NEq   = (?!=)
bfunc Gt   = (?>)
bfunc Lt      = (?<)
bfunc LEq    = (?<=)
bfunc GEq = (?>=)
bfunc Cross      = error "bfunc: Cross not implemented"
bfunc Pow    = (#^)
bfunc Subt   = (#-)
bfunc Impl    = error "convExpr :=>"
bfunc Iff        = error "convExpr :<=>"
bfunc Dot = error "convExpr DotProduct"
bfunc Frac = (#/)
bfunc Index      = (\x y -> x I.$.(listAccess y))

-- medium hacks --
genModDef :: (I.RenderSym repr) => CS.Mod -> Reader State (repr (I.Module repr))
genModDef (CS.Mod n fs) = genModule n (Just $ sequence $ map genFunc fs) Nothing

genFunc :: (I.RenderSym repr) => Func -> Reader State (repr (I.Method repr))
genFunc (FDef (FuncDef n i o s)) = do
  g <- ask
  parms <- getParams i
  types <- getParamTypes i
  names <- getParamNames i
  blocks <- mapM convBlock s
  publicMethod (mState $ convType o) n parms types names
    (return body $ (block
        (map (\x -> varDec (codeName x) (convType $ codeType x))
          ((((fstdecl (sysinfodb (codeSpec g)))) s) \\ i)))
        ++ blocks
    ])
genFunc (FData (FuncData n dd)) = genDataFunc n dd
genFunc (FCD cd) = genCalcFunc cd

convBlock :: (I.RenderSym repr) => FuncStmt -> Reader State (repr (I.Block 
  repr))
convBlock (FAsg v e) = do
  e' <- convExpr e
  return $ block $ [assign (var $ codeName v) e']
convBlock (FFor v e st) = do
  blcks <- mapM convBlock st
  e' <- convExpr e
  return $ for (varDecDef (codeName v) int (litInt 0)) e' ((&++) (var (codeName v)))
               (body blcks)
convBlock (FWhile e st) = do
  blcks <- mapM convBlock st
  e' <- convExpr e
  return $ while e' (body blcks)
convBlock (FCond e tSt []) = do
  blcks <- mapM convBlock tSt
  e' <- convExpr e
  return $ ifNoElse [(e', (body blcks))]
convBlock (FCond e tSt eSt) = do
  blck1 <- mapM convBlock tSt
  blck2 <- mapM convBlock eSt
  e' <- convExpr e
  return $ ifCond [(e', (body blck1)] (body blck2)
convBlock (FRet e) = do
  e' <- convExpr e
  return $ block $ [I.returnState e']
convBlock (FThrow s) = return $ block [throw s]
convBlock (FTry t c) = do
  blck1 <- mapM convBlock t
  blck2 <- mapM convBlock c
  return $ tryCatch (body blck1) (body blck2)
convBlock (FContinue) = return $ block [continue]
convBlock (FDec v (C.List t)) = return $ listDec' (codeName v) 0 (listType
  dynamic $ convType t)
-- Again, above won't work for intListType, etc. Better solution needed.
convBlock (FDec v t) = return $ varDec (codeName v) (convType t)
convBlock (FProcCall n l) = do
  e' <- convExpr (FCall (asExpr n) l)
  return $ block [valState e']
convBlock (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ block [valState $ liftM2 (\x y -> x I.$.(listAppend y)) a' b']

-- this is really ugly!!
genDataFunc :: Name -> DataDesc -> Reader State Method
genDataFunc nameTitle dd = do
    parms <- getParams $ getInputs dd
    inD <- mapM inData dd
    publicMethod methodTypeVoid nameTitle (p_filename : parms) $
      return $ body $ [
      varDec l_infile infile,
      varDec l_line string,
      listDec' l_lines string 0,
      listDec' l_linetokens string 0,
      openFileR v_infile v_filename ] ++
      (concat inD) ++ [
      closeFile v_infile ]
  where inData :: Data -> Reader State [Statement]
        inData (Singleton v) = do
          vv <- variable $ codeName v
          return [getFileInput v_infile (convType $ codeType v) vv]
        inData JunkData = return [discardFileLine v_infile]
        inData (Line lp d) = do
          lnI <- lineData lp (litInt 0)
          return $ [ getFileInputLine v_infile v_line, stringSplit v_linetokens v_line d ] ++ lnI
        inData (Lines lp Nothing d) = do
          lnV <- lineData lp v_i
          return $ [ getFileInputAll v_infile v_lines,
              for (varDecDef l_i int (litInt 0)) (v_i ?< v_lines I.$.listSize) ((&++) v_i)
                ( body ( [ stringSplit v_linetokens (v_lines I.$.(listAccess v_i)) d ] ++ lnV))
            ]
        inData (Lines lp (Just numLines) d) = do
          lnV <- lineData lp v_i
          return $ [ for (varDecDef l_i int (litInt 0)) (v_i ?< (litInt numLines)) ((&++) v_i)
              ( body
                ( [ getFileInputLine v_infile v_line,
                    stringSplit v_linetokens v_line d
                  ] ++ lnV
                )
              )
            ]
        ---------------
        lineData :: LinePattern -> Value -> Reader State [Statement]
        lineData (Straight p) lineNo = patternData p lineNo (litInt 0)
        lineData (Repeat p Nothing) lineNo = do
          pat <- patternData p lineNo v_j
          return [ for (varDecDef l_j int (litInt 0)) (v_j ?< (v_linetokens I.$.listSize #/ (litInt $ toInteger $ length p))I.$.(cast int float)) ((&++) v_j)
              ( body pat )
            ]
        lineData (Repeat p (Just numPat)) lineNo = do
          pat <- patternData p lineNo v_j
          return [ for (varDecDef l_j int (litInt 0)) (v_j ?< (litInt numPat)) ((&++) v_j)
              ( body pat )
            ]
        ---------------
        patternData :: [Entry] -> Value -> Value -> Reader State [Statement]
        patternData d lineNo patNo = do
          let l = toInteger $ length d
          ent <- mapM (\(x,y) -> entryData x lineNo patNo y) $ zip (map (\z -> (patNo #* (litInt l)) #+ (litInt z)) [0..l-1]) d
          return $ concat ent
        ---------------
        entryData :: Value -> Value -> Value -> Entry -> Reader State [Statement]
        entryData tokIndex _ _ (Entry v) = do
          vv <- variable $ codeName v
          a <- assign vv $ (v_linetokens I.$.(listAccess tokIndex))I.$. (cast (convType $ codeType v) string)
          return [a]
        entryData tokIndex lineNo patNo (ListEntry indx v) = do
          vv <- variable $ codeName v
          a <- assign (indexData indx lineNo patNo vv) $
                (v_linetokens I.$.(listAccess tokIndex))I.$.(cast (listType (codeType v) (toInteger $ length indx)) string)
          return $ checkIndex indx lineNo patNo vv (codeType v) ++ [ a ]
        entryData _ _ _ JunkEntry = return []
        ---------------
        indexData :: [Ind] -> Value -> Value -> Value -> Value
        indexData [] _ _ v = v
        indexData ((Explicit i):is) l p v = indexData is l p (ObjAccess v (listAccess $ litInt i))
        indexData (WithLine:is) l p v = indexData is l p (ObjAccess v (listAccess l))
        indexData (WithPattern:is) l p v = indexData is l p (ObjAccess v (listAccess p))
        ---------------
        checkIndex :: [Ind] -> Value -> Value -> Value -> C.CodeType -> [Statement]
        checkIndex indx l p v s = checkIndex' indx len l p v (listBase s)
          where len = toInteger $ length indx
        checkIndex' [] _ _ _ _ _ = []
        checkIndex' ((Explicit i):is) n l p v s =
          [ while (v I.$.listSize ?<= (litInt i)) ( body [ valStmt $ v I.$.(listExtend $ listType' s n) ] ) ]
          ++ checkIndex' is (n-1) l p (v I.$.(listAccess $ litInt i)) s
        checkIndex' ((WithLine):is) n l p v s =
          [ while (v I.$.listSize ?<= l) ( body [ valStmt $ v I.$.(listExtend $ listType' s n ) ] ) ]
          ++ checkIndex' is (n-1) l p (v I.$.(listAccess l)) s
        checkIndex' ((WithPattern):is) n l p v s =
          [ while (v I.$.listSize ?<= p) ( body [ valStmt $ v I.$.(listExtend $ listType' s n ) ] ) ]
          ++ checkIndex' is (n-1) l p (v I.$.(listAccess p)) s
        ---------------
        listType :: C.CodeType -> Integer -> I.StateType
        listType _ 0 = error "No index given"
        listType (C.List t) 1 = convType t
        listType (C.List t) n = listType t (n-1)
        listType _ _ = error "Not a list type"
        ---------------
        listBase :: C.CodeType -> C.CodeType
        listBase (C.List t) = listBase t
        listBase t = t
        ---------------
        listType' :: C.CodeType -> Integer -> I.StateType
        listType' _ 0 = error "No index given"
        listType' t 1 = convType t
        listType' t n = listT $ listType' t (n-1)
        ---------------
        l_line = "line"
        v_line = var l_line
        l_lines = "lines"
        v_lines = var l_lines
        l_linetokens = "linetokens"
        v_linetokens = var l_linetokens
        l_infile = "infile"
        v_infile = var l_infile
        l_filename = "filename"
        p_filename = param l_filename string
        v_filename = var l_filename
        l_i = "i"
        v_i = var l_i
        l_j = "j"
        v_j = var l_j
