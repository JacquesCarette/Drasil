{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import(generator, generateCode) where

import Language.Drasil hiding (int, Label, Block, ($.), log, ln, exp,
  sin, cos, tan, csc, sec, cot)
import Language.Drasil.Code.Code as C (CodeType(List, File, Char, Float, 
  Object, String, Boolean, Integer))
import Language.Drasil.Code.Imperative.New (Label,
  RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), Selector(..), FunctionSym(..), SelectorFunction(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code (CodeChunk, CodeDefinition, codeName,
  codeType, codevar, codefunc, codeEquat, funcPrefix, physLookup, sfwrLookup,
  programName)
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))
import qualified Language.Drasil.CodeSpec as CS (Mod(..))
import Language.Drasil.Code.DataDesc (Ind(WithPattern, WithLine, Explicit), 
  Entry(JunkEntry, ListEntry, Entry), LinePattern(Repeat, Straight), 
  Data(Line, Lines, JunkData, Singleton), DataDesc, getInputs)

import Prelude hiding (sin, cos, tan, log, exp, const)
import Data.List (intersperse, (\\), stripPrefix)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing,
  getCurrentDirectory)
import Data.Map (member)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe)
import Control.Lens ((^.))
import Control.Monad (when,liftM2,liftM3,zipWithM)
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Text.PrettyPrint.HughesPJ (Doc)

-- Private State, used to push these options around the generator
data State repr = State {
  codeSpec :: CodeSpec,
  inStruct :: Structure,
  logName :: String,
  logKind :: Logging,
  commented :: Comments,
  currentModule :: String,

  sfwrCBody :: (RenderSym repr) => Expr -> (repr (Body repr)),
  physCBody :: (RenderSym repr) => Expr -> (repr (Body repr))
}

-- function to choose how to deal with
-- 1. constraints
-- 2. how to structure the input "module"
-- 3. logging assignments
chooseConstr :: (RenderSym repr) => ConstraintBehaviour -> Expr -> (repr
  (Body repr))
chooseConstr Warning   = constrWarn
chooseConstr Exception = constrExc

chooseInStructure :: (RenderSym repr) => Structure -> Reader (State repr) 
  [(repr (Module repr))]
chooseInStructure Loose   = genInputModNoClass
chooseInStructure AsClass = genInputModClass

chooseLogging :: (RenderSym repr) => Logging -> ((repr (Value repr)) -> 
  (repr (Value repr)) -> Reader (State repr) (repr (Statement repr)))
chooseLogging LogVar = loggedAssign
chooseLogging LogAll = loggedAssign
chooseLogging _      = (\x y -> return $ assign x y)

generator :: (RenderSym repr) => Choices -> CodeSpec -> (State repr)
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

assign' :: (RenderSym repr) => (repr (Value repr)) -> (repr (Value repr)) ->
  Reader (State repr) (repr (Statement repr))
assign' x y = do
  g <- ask
  chooseLogging (logKind g) x y

publicMethod :: (RenderSym repr) => (repr (MethodType repr)) -> Label -> [repr
  (Parameter repr)] -> [repr (StateType repr)] -> [Label] -> Reader (State repr)
  [(repr (Block repr))] -> Reader (State repr) (repr (Method repr))
publicMethod mt l pl st v u = do
  g <- ask
  genMethodCall public static (commented g) (logKind g) mt l pl st v u

generateCode :: (RenderSym repr) => Lang -> ((repr (RenderFile repr)) -> 
  (Doc, Label)) -> (State repr) -> IO ()
generateCode l unRepr g =
  do workingDir <- getCurrentDirectory
     createDirectoryIfMissing False (getDir l)
     setCurrentDirectory (getDir l)
     when (l == Java) $ createDirectoryIfMissing False prog
     when (l == Java) $ setCurrentDirectory prog
     createCodeFiles $ makeCode (map unRepr files) (getExt l)
     setCurrentDirectory workingDir
  where prog = case codeSpec g of { CodeSpec {program = pp} -> programName pp }
        files = runReader genFiles g

genFiles :: (RenderSym repr) => Reader (State repr) [(repr (RenderFile repr))]
genFiles = do
  ms <- genModules
  return $ map fileDoc ms

genModules :: (RenderSym repr) => Reader (State repr) [(repr (Module repr))]
genModules = do
  g <- ask
  let s = codeSpec g
  mn     <- genMain
  inp    <- chooseInStructure $ inStruct g
  out    <- genOutputMod $ outputs s
  moddef <- sequence $ fmap genModDef (mods s) -- hack ?
  return $ (mn : inp ++ out ++ moddef)

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"

getExt :: Lang -> [Label]
getExt Java = [".java"]
getExt Python = [".py"]
getExt _ = error "Language not yet implemented"

liftS :: Reader a b -> Reader a [b]
liftS = fmap (\x -> [x])

------- INPUT ----------

genInputModClass :: (RenderSym repr) => Reader (State repr) 
  [(repr (Module repr))]
genInputModClass =
  sequence $ [ genModule "InputParameters" Nothing (Just $ liftS genInputClass),
               genModule "DerivedValues" (Just $ liftS genInputDerived) Nothing,
               genModule "InputConstraints" (Just $ liftS genInputConstraints)
                 Nothing
             ]

genInputModNoClass :: (RenderSym repr) => Reader (State repr)
  [(repr (Module repr))]
genInputModNoClass = do
  g <- ask
  let ins = inputs $ codeSpec g
  inpDer    <- genInputDerived
  inpConstr <- genInputConstraints
  return $ [ buildModule "InputParameters" []
             (map (\x -> varDecDef (codeName x) (convType $ codeType x)
               (getDefaultValue $ codeType x)) ins)
             [inpDer , inpConstr]
             []
           ]

genInputClass :: (RenderSym repr) => Reader (State repr) (repr (Class repr))
genInputClass = do
  g <- ask
  let ins          = inputs $ codeSpec g
      inputVars    =
          map (\x -> pubMVar 0 (codeName x) (convType $ codeType x)) ins
      varsList     = map (\x -> self $-> var (codeName x)) ins
      vals         = map (getDefaultValue . codeType) ins
  asgs <- zipWithM assign' varsList vals
  return $ pubClass "InputParameters" Nothing inputVars
    [ constructor "InputParameters" [] (body [block asgs]) ]

genInputConstraints :: (RenderSym repr) => Reader (State repr) 
  (repr (Method repr))
genInputConstraints = do
  g <- ask
  let varsList = inputs $ codeSpec g
      cm       = cMap $ codeSpec g
      sfwrCs   = concatMap (renderC . sfwrLookup cm) varsList
      physCs   = concatMap (renderC . physLookup cm) varsList
  parms <- getParams varsList
  ptypes <- getParamTypes varsList
  pnames <- getParamNames varsList
  sf <- mapM (\x -> do { e <- convExpr x; return $ ifNoElse [((?!) e, 
    sfwrCBody g x)]}) sfwrCs
  hw <- mapM (\x -> do { e <- convExpr x; return $ ifNoElse [((?!) e, 
    physCBody g x)]}) physCs
  publicMethod void "input_constraints" parms ptypes pnames
      (return $ sf ++ hw)

genInputDerived :: (RenderSym repr) => Reader (State repr) (repr (Method repr))
genInputDerived = do
  g <- ask
  let dvals = derivedInputs $ codeSpec g
  parms <- getParams $ map codevar dvals
  ptypes <- getParamTypes $ map codevar dvals
  pnames <- getParamNames $ map codevar dvals
  inps <- mapM (\x -> genCalcBlock CalcAssign (codeName x) (codeEquat x)) dvals
  publicMethod void "derived_values" parms ptypes pnames
      (return $ inps)

-- need Expr -> String to print constraint
constrWarn :: (RenderSym repr) => Expr -> (repr (Body repr))
constrWarn _ = oneLiner $ printStrLn "Warning: constraint violated"

constrExc :: (RenderSym repr) => Expr -> (repr (Body repr))
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
genCalcFunc :: (RenderSym repr) => CodeDefinition -> Reader (State repr) (repr
  (Method repr))
genCalcFunc cdef = do
  g <- ask
  parms <- getParams $ codecs g
  ptypes <- getParamTypes $ codecs g
  pnames <- getParamNames $ codecs g
  blck <- genCalcBlock CalcReturn (codeName cdef) (codeEquat cdef)
  publicMethod
    (mState $ convType (codeType cdef))
    (codeName cdef)
    parms ptypes pnames
    (return $ [blck])
  where codecs g = codevars' (codeEquat cdef) $ sysinfodb $ codeSpec g

data CalcType = CalcAssign | CalcReturn deriving Eq

genCalcBlock :: (RenderSym repr) => CalcType -> String -> Expr -> Reader (State
  repr) (repr (Block repr))
genCalcBlock t' v' e' = do
  doit t' v' e'
    where
    doit :: (RenderSym repr) => CalcType -> String -> Expr -> Reader (State
      repr) (repr (Block repr))
    doit t v (Case e)    = genCaseBlock t v e
    doit t v e
      | t == CalcAssign  = fmap block $ liftS $ do { vv <- variable v; ee <-
        convExpr e; assign' vv ee}
      | otherwise        = fmap block $ liftS $ fmap returnState $ convExpr e

genCaseBlock :: (RenderSym repr) => CalcType -> String -> [(Expr,Relation)] ->
  Reader (State repr) (repr (Block repr))
genCaseBlock t v cs = do
  ifs <- mapM (\(e,r) -> liftM2 (,) (convExpr r) (fmap body $ liftS $
    genCalcBlock t v e)) cs
  return $ ifNoElse ifs

----- OUTPUT -------

genOutputMod :: (RenderSym repr) => [CodeChunk] -> Reader (State repr) [(repr
  (Module repr))]
genOutputMod outs = liftS $ genModule "OutputFormat" (Just $ liftS $ 
  genOutputFormat outs) Nothing

genOutputFormat :: (RenderSym repr) => [CodeChunk] -> Reader (State repr) (repr 
  (Method repr))
genOutputFormat outs =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    parms <- getParams outs
    ptypes <- getParamTypes outs
    pnames <- getParamNames outs
    outp <- mapM (\x -> do
        v <- variable $ codeName x
        return [ printFileStr v_outfile ((codeName x) ++ " = "),
                 printFileLn v_outfile (convType $ codeType x) v
               ] ) outs
    publicMethod void "write_output" parms ptypes pnames (return $ [block $
      [
      varDec l_outfile outfile,
      openFileW v_outfile (litString "output.txt") ] ++
      concat outp ++ [ closeFile v_outfile ]])

-----

genMethodCall :: (RenderSym repr) => (repr (Scope repr)) -> (repr
  (Permanence repr)) -> Comments -> Logging -> (repr (MethodType repr)) ->
  Label -> [repr (Parameter repr)] -> [repr (StateType repr)] -> [Label]
  -> Reader (State repr) [(repr (Block repr))] -> Reader (State repr) (repr
  (Method repr))
genMethodCall s pr doComments doLog t n p st l b = do
  let loggedBody LogFunc = loggedMethod n st l b
      loggedBody LogAll  = loggedMethod n st l b
      loggedBody _       = b
      commBody CommentFunc = commMethod n l
      commBody _           = id
  bod <- commBody doComments (loggedBody doLog)
  return $ method n s pr t p (body bod)

commMethod :: (RenderSym repr) => Label -> [Label] -> Reader (State repr) 
  [(repr (Block repr))] -> Reader (State repr) [(repr (Block repr))]
commMethod n l b = do
  g <- ask
  rest <- b
  return $ (block [
      comment $ "function '" ++ n ++ "': " ++ (funcTerm n (fMap $ codeSpec g)),
      multi $ map
        (\x -> comment $ "parameter '" ++ x ++ "': " ++ (varTerm x (vMap $ 
          codeSpec g))) l
    ]) : rest 

loggedMethod :: (RenderSym repr) => Label -> [repr (StateType repr)] ->
  [Label] -> Reader (State repr) [(repr (Block repr))] -> Reader (State repr)
  [(repr (Block repr))]
loggedMethod n st l b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    g <- ask
    rest <- b
    return $ (block [
      varDec l_outfile outfile,
      openFileW v_outfile (litString $ logName g),
      printFileStr v_outfile ("function " ++ n ++ "("),
      printParams st l v_outfile,
      printFileStrLn v_outfile ") called",
      closeFile v_outfile ] )
      : rest
  where
    printParams sts ls v_outfile = multi $
      intersperse (printFileStr v_outfile ", ") $
      map (\(x,y) -> printFile v_outfile x (var y)) (zip sts ls)

---- MAIN ---

genModule :: (RenderSym repr) => Name
               -> Maybe (Reader (State repr) [(repr (Method repr))])
               -> Maybe (Reader (State repr) [(repr (Class repr))])
               -> Reader (State repr) (repr (Module repr))
genModule n maybeMs maybeCs = do
  g <- ask
  let ls = maybe [] id (Map.lookup n (dMap $ codeSpec g))
      updateState = withReader (\s -> s { currentModule = n })
  cs <- maybe (return []) updateState maybeCs
  ms <- maybe (return []) updateState maybeMs
  return $ buildModule n ls [] ms cs


genMain :: (RenderSym repr) => Reader (State repr) (repr (Module repr))
genMain = genModule "Control" (Just $ liftS $ genMainFunc) Nothing

genMainFunc :: (RenderSym repr) => Reader (State repr) (repr (Method repr))
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
      return $ varDecDef (nopfx $ codeName x) (convType $ codeType x) cnargs)
        (execOrder $ codeSpec g)
    wo <- fApp "write_output" args2
    return $ mainMethod $ bodyStatements $ [
      varDecDef l_filename string $ arg 0 ,
      extObjDecNewVoid l_params "InputParameters" (obj "InputParameters") ,
      valState gi,
      valState dv,
      valState ic
      ] ++ varDef ++ [ valState wo ]

-----

loggedAssign :: (RenderSym repr) => (repr (Value repr)) -> 
  (repr (Value repr)) -> Reader (State repr) (repr (Statement repr))
loggedAssign a b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    g <- ask
    return $ multi [
      assign a b,
      varDec l_outfile outfile,
      openFileW v_outfile (litString $ logName g),
      printFileStr v_outfile ("var '" ++ (valName a) ++ "' assigned to "),
      printFile v_outfile (convType $ varType (valName b) (vMap $ codeSpec g))
        b,
      printFileStrLn v_outfile (" in module " ++ currentModule g),
      closeFile v_outfile ]

-- helpers

nopfx :: String -> String
nopfx s = maybe s id (stripPrefix funcPrefix s)

variable :: (RenderSym repr) => String -> Reader (State repr) 
  (repr (Value repr))
variable s' = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
      doit :: (RenderSym repr) => String -> Reader (State repr) 
        (repr (Value repr))
      doit s | member s mm =
        maybe (error "impossible") ((convExpr) . codeEquat) (Map.lookup s mm) --extvar "Constants" s
             | s `elem` (map codeName $ inputs cs) = return $
               (var "inParams")$->(var s)
             | otherwise                        = return $ var s
  doit s'
  
fApp :: (RenderSym repr) => String -> [(repr (Value repr))] -> Reader 
  (State repr) (repr (Value repr))
fApp s' vl' = do
  g <- ask
  let doit :: (RenderSym repr) => String -> [(repr (Value repr))] -> (repr
        (Value repr))
      doit s vl | member s (eMap $ codeSpec g) =
        maybe (error "impossible")
          (\x -> if x /= currentModule g then extFuncApp x s vl else 
            funcApp s vl)
          (Map.lookup s (eMap $ codeSpec g))
                | otherwise = funcApp s vl
  return $ doit s' vl'

getParams :: (RenderSym repr) => [CodeChunk] -> Reader (State repr) 
  [(repr (Parameter repr))]
getParams cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      ps = map (\y -> stateParam (codeName y) (convType $ codeType y))
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g))
              csSubIns)
  return $ if length csSubIns < length cs
           then (stateParam "inParams" (obj "InputParameters")):ps  -- todo:  make general
           else ps

getParamTypes :: (RenderSym repr) => [CodeChunk] -> Reader (State repr) 
  [(repr (StateType repr))]
getParamTypes cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      pts = map (\y -> convType $ codeType y)
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g))
              csSubIns)
  return $ if length csSubIns < length cs
          then (obj "InputParameters"):pts  -- todo:  make general
          else pts

getParamNames :: [CodeChunk] -> Reader (State repr) [Label]
getParamNames cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      pvs = map (\y -> codeName y)
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g))
              csSubIns)
  return $ if length csSubIns < length cs
          then ("inParams"):pvs  -- todo:  make general
          else pvs

getArgs :: (RenderSym repr) => [CodeChunk] -> Reader (State repr) 
  [(repr (Value repr))]
getArgs cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      args = map (var . codeName)
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g))
              csSubIns)
  return $ if length csSubIns < length cs
           then (var "inParams"):args  -- todo:  make general
           else args

getDefaultValue :: (RenderSym repr) => C.CodeType -> (repr (Value repr))
getDefaultValue C.Boolean = defaultBool
getDefaultValue C.Integer = defaultInt
getDefaultValue C.Float = defaultFloat
getDefaultValue C.Char = defaultChar
getDefaultValue C.String = defaultString
getDefaultValue _ = error "No default value for the given type"

convType :: (RenderSym repr) => C.CodeType -> (repr (StateType repr))
convType C.Boolean = bool
convType C.Integer = int
convType C.Float = float
convType C.Char = char
convType C.String = string
convType (C.List t) = getListTypeFunc t dynamic
convType (C.Object n) = obj n
convType (C.File) = error "convType: File ?"

convExpr :: (RenderSym repr) => Expr -> Reader (State repr) (repr (Value repr))
convExpr (Dbl d)      = return $ litFloat d
convExpr (Int i)      = return $ litInt i
convExpr (Str s)      = return $ litString s
convExpr (AssocA Add l)  = fmap (foldr1 (#+)) $ sequence $ map convExpr l
convExpr (AssocA Mul l)  = fmap (foldr1 (#*)) $ sequence $ map convExpr l
convExpr (AssocB And l)  = fmap (foldr1 (?&&)) $ sequence $ map convExpr l
convExpr (AssocB Or l)  = fmap (foldr1 (?||)) $ sequence $ map convExpr l
convExpr (Deriv _ _ _) = return $ 
  litString "**convExpr :: Deriv unimplemented**"
convExpr (C c)         = do
  g <- ask
  variable $ codeName $ codevar (symbLookup c ((sysinfodb $ codeSpec g) ^.
    symbolTable))
convExpr  (FCall (C c) x)  = do
  g <- ask
  let info = sysinfodb $ codeSpec g
  args <- mapM convExpr x
  fApp (codeName (codefunc (symbLookup c (info ^. symbolTable)))) args
convExpr (FCall _ _)   = return $ 
  litString "**convExpr :: FCall unimplemented**"
convExpr (UnaryOp o u) = fmap (unop o) (convExpr u)
convExpr (BinaryOp Frac (Int a) (Int b)) =
  return $ (litFloat $ fromIntegral a) #/ (litFloat $ fromIntegral b) -- hack to deal with integer division
convExpr  (BinaryOp o a b)  = liftM2 (bfunc o) (convExpr a) (convExpr b)
convExpr  (Case l)      = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen"
    doit [(e,_)] = convExpr e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExpr cond) (convExpr e) 
      (convExpr (Case xs))
convExpr (Matrix _)    = error "convExpr: Matrix"
convExpr (Operator _ _ _) = error "convExpr: Operator"
convExpr (IsIn _ _)    = error "convExpr: IsIn"
convExpr (RealI c ri)  = do
  g <- ask
  convExpr $ renderRealInt (lookupC (sysinfodb $ codeSpec g) c) ri

getUpperBound :: Expr -> Expr
getUpperBound (BinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

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

unop :: (RenderSym repr) => UFunc -> ((repr (Value repr)) -> 
  (repr (Value repr)))
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
unop Dim  = listSizeAccess
unop Norm = error "unop: Norm not implemented"
unop Not  = (?!)
unop Neg  = (#~)

bfunc :: (RenderSym repr) => BinOp -> ((repr (Value repr)) -> (repr
  (Value repr)) -> (repr (Value repr)))
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
bfunc Index      = (\x y -> x $.(listAccess y))

-- medium hacks --
genModDef :: (RenderSym repr) => CS.Mod -> Reader (State repr) 
  (repr (Module repr))
genModDef (CS.Mod n fs) = genModule n (Just $ sequence $ map genFunc fs) Nothing

genFunc :: (RenderSym repr) => Func -> Reader (State repr) (repr (Method repr))
genFunc (FDef (FuncDef n i o s)) = do
  g <- ask
  parms <- getParams i
  ptypes <- getParamTypes i
  pnames <- getParamNames i
  blocks <- mapM convBlock s
  publicMethod (mState $ convType o) n parms ptypes pnames
    (return $ [block
        (map (\x -> varDec (codeName x) (convType $ codeType x))
          ((((fstdecl (sysinfodb (codeSpec g)))) s) \\ i))]
        ++ blocks
    )
genFunc (FData (FuncData n dd)) = genDataFunc n dd
genFunc (FCD cd) = genCalcFunc cd

convBlock :: (RenderSym repr) => FuncStmt -> Reader (State repr) (repr (Block 
  repr))
convBlock (FAsg v e) = do
  e' <- convExpr e
  fmap block $ liftS $ assign' (var $ codeName v) e'
convBlock (FFor v e st) = do
  blcks <- mapM convBlock st
  e' <- convExpr $ getUpperBound e
  return $ forRange (codeName v) (litInt 0) e' (litInt 1) (body blcks)
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
  return $ ifCond [(e', (body blck1))] (body blck2)
convBlock (FRet e) = do
  e' <- convExpr e
  return $ block $ [returnState e']
convBlock (FThrow s) = return $ block [throw s]
convBlock (FTry t c) = do
  blck1 <- mapM convBlock t
  blck2 <- mapM convBlock c
  return $ tryCatch (body blck1) (body blck2)
convBlock (FContinue) = return $ block [continue]
convBlock (FDec v (C.List t)) = return $ block [listDec (codeName v) 0 
  (getListTypeFunc t dynamic)]
convBlock (FDec v t) = return $ block [varDec (codeName v) (convType t)]
convBlock (FProcCall n l) = do
  e' <- convExpr (FCall (asExpr n) l)
  return $ block [valState e']
convBlock (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ block [valState $ a' $.(listAppend b')]

getListTypeFunc :: (RenderSym repr) => C.CodeType -> (repr (Permanence repr)) ->
  (repr (StateType repr))
getListTypeFunc C.Integer p = intListType p
getListTypeFunc C.Float p = floatListType p
getListTypeFunc C.Boolean _ = boolListType
getListTypeFunc (C.List t) p = listType p $ getListTypeFunc t p
getListTypeFunc t p = listType p (convType t)

-- this is really ugly!!
genDataFunc :: (RenderSym repr) => Name -> DataDesc -> Reader (State repr)
  (repr (Method repr))
genDataFunc nameTitle dd = do
    parms <- getParams $ getInputs dd
    ptypes <- getParamTypes $ getInputs dd
    pnames <- getParamNames $ getInputs dd
    inD <- mapM inData dd
    publicMethod void nameTitle (p_filename : parms) (string : ptypes)
      (l_filename : pnames) $
      return $ [ (block [
      varDec l_infile infile,
      varDec l_line string,
      listDec l_lines 0 (listType dynamic string),
      listDec l_linetokens 0 (listType dynamic string),
      openFileR v_infile v_filename ])] ++
      (concat inD) ++ [(block [
      closeFile v_infile ])]
  where inData :: (RenderSym repr) => Data -> Reader (State repr) [repr (Block
          repr)]
        inData (Singleton v) = do
            vv <- variable $ codeName v
            return $ [block [(getFileInput (codeType v)) v_infile vv]]
        inData JunkData = return $ [block [discardFileLine v_infile]]
        inData (Line lp d) = do
          lnI <- lineData lp (litInt 0)
          return $ [block [ getFileInputLine v_infile v_line, 
            stringSplit d v_linetokens v_line ]] ++ lnI
        inData (Lines lp Nothing d) = do
          lnV <- lineData lp v_i
          return $ [ getFileInputAll v_infile v_lines,
            forRange l_i (litInt 0) (listSizeAccess v_lines) (litInt 1)
              ( body $ [(block [ stringSplit d v_linetokens (v_lines $.
                (listAccess v_i)) ])] ++ lnV)
            ]
        inData (Lines lp (Just numLines) d) = do
          lnV <- lineData lp v_i
          return $ [ forRange l_i (litInt 0) (litInt numLines) (litInt 1)
            ( body $
              [ (block [ getFileInputLine v_infile v_line,
                  stringSplit d v_linetokens v_line
                ])
              ] ++ lnV
            )
            ]
        ---------------
        lineData :: (RenderSym repr) => LinePattern -> (repr (Value repr))
          -> Reader (State repr) [(repr (Block repr))]
        lineData (Straight p) lineNo = patternData p lineNo (litInt 0)
        lineData (Repeat p Nothing) lineNo = do
          pat <- patternData p lineNo v_j
          return $ [forRange l_j (litInt 0) (castObj (cast int float)
            (listSizeAccess v_linetokens #/ (litInt $ toInteger $ length p))) (litInt 1)
            ( body pat )]
        lineData (Repeat p (Just numPat)) lineNo = do
          pat <- patternData p lineNo v_j
          return $ [forRange l_j (litInt 0) (litInt numPat) (litInt 1) 
            ( body pat )]
        ---------------
        patternData :: (RenderSym repr) => [Entry] -> (repr (Value repr))
          -> (repr (Value repr)) -> Reader (State repr) [(repr (Block repr))]
        patternData d lineNo patNo = do
          let l = toInteger $ length d
          ent <- mapM (\(x,y) -> entryData x lineNo patNo y) $ 
            zip (map (\z -> (patNo #* (litInt l)) #+ (litInt z)) [0..l-1]) d
          return $ concat ent
        ---------------
        entryData :: (RenderSym repr) => (repr (Value repr)) -> (repr 
          (Value repr)) -> (repr (Value repr)) -> Entry -> Reader (State repr)
          [(repr (Block repr))]
        entryData tokIndex _ _ (Entry v) = do
          vv <- variable $ codeName v
          a <- assign' vv $ getCastFunc (codeType v)
            (v_linetokens $.(listAccess tokIndex))
          return [block [a]]
        entryData tokIndex lineNo patNo (ListEntry indx v) = do
          vv <- variable $ codeName v
          return $ checkIndex indx 1 lineNo patNo vv (codeType v) ++ [block [
            valState $ (indexData indx lineNo patNo vv) $. 
            (listSet (getIndex indx lineNo patNo) $ 
            getCastFunc (getListType (codeType v) (toInteger $ length indx))
            (v_linetokens $.(listAccess tokIndex)))]]
        entryData _ _ _ JunkEntry = return []
        ---------------
        indexData :: (RenderSym repr) => [Ind] -> (repr (Value repr)) ->
          (repr (Value repr)) -> (repr (Value repr)) -> (repr (Value repr))
        indexData [_] _ _ v = v
        indexData ((Explicit i):is) l p v = indexData is l p (objAccess v 
          (listAccess $ litInt i))
        indexData (WithLine:is) l p v = indexData is l p (objAccess v 
          (listAccess l))
        indexData (WithPattern:is) l p v = indexData is l p (objAccess v
          (listAccess p))
        ------------------------------
        getIndex :: (RenderSym repr) => [Ind] -> (repr (Value repr)) ->
          (repr (Value repr)) -> (repr (Value repr))
        getIndex [(Explicit i)] _ _ = litInt i
        getIndex [WithLine] l _ = l
        getIndex [WithPattern] _ p = p
        getIndex (x:xs) l p = getIndex xs l p
        ---------------
        checkIndex :: (RenderSym repr) => [Ind] -> Integer-> (repr (Value repr)) ->
          (repr (Value repr)) -> (repr (Value repr)) -> C.CodeType ->
          [repr (Block repr)]
        checkIndex [] _ _ _ _ _ = []
        checkIndex ((Explicit i):is) n l p v s =
          [ while (listSizeAccess v ?<= (litInt i)) ( bodyStatements [ 
            valState $
            v $.(getListExtend (getListType s n)) ] ) ]
            ++ checkIndex is (n+1) l p (v $.(listAccess $ litInt i)) s
        checkIndex ((WithLine):is) n l p v s =
          [ while (listSizeAccess  v ?<= l) ( bodyStatements [ valState $
          v $.(getListExtend (getListType s n)) ] ) ]
          ++ checkIndex is (n+1) l p (v $.(listAccess l)) s
        checkIndex ((WithPattern):is) n l p v s =
          [ while (listSizeAccess v ?<= p) ( bodyStatements [ valState $ 
          v $.(getListExtend (getListType s n)) ] ) ]
          ++ checkIndex is (n+1) l p (v $.(listAccess p)) s
        ---------------------------
        l_line, l_lines, l_linetokens, l_infile, l_filename, l_i, l_j :: Label
        v_line, v_lines, v_linetokens, v_infile, v_filename, v_i, v_j ::
          (RenderSym repr) => (repr (Value repr))
        p_filename :: (RenderSym repr) => (repr (Parameter repr))
        l_line = "line"
        v_line = var l_line
        l_lines = "lines"
        v_lines = var l_lines
        l_linetokens = "linetokens"
        v_linetokens = var l_linetokens
        l_infile = "infile"
        v_infile = var l_infile
        l_filename = "filename"
        p_filename = stateParam l_filename string
        v_filename = var l_filename
        l_i = "i"
        v_i = var l_i
        l_j = "j"
        v_j = var l_j

getFileInput :: (RenderSym repr) => C.CodeType -> ((repr (Value repr)) -> (repr
  (Value repr)) -> (repr (Statement repr)))
getFileInput C.Boolean = getBoolFileInput
getFileInput C.Integer = getIntFileInput
getFileInput C.Float = getFloatFileInput
getFileInput C.Char = getCharFileInput
getFileInput C.String = getStringFileInput
getFileInput _ = error "No getFileInput function for the given type"

getListExtend :: (RenderSym repr) => C.CodeType -> (repr (Function repr))
getListExtend C.Boolean = listExtendBool
getListExtend C.Integer = listExtendInt
getListExtend C.Float = listExtendFloat
getListExtend C.Char = listExtendChar
getListExtend C.String = listExtendString
getListExtend t@(C.List _) = listExtendList (convType t)
getListExtend _ = error "No listExtend function for the given type"

getCastFunc :: (RenderSym repr) => C.CodeType -> (repr (Value repr)) ->
   (repr (Value repr))
getCastFunc C.Float = castStrToFloat
getCastFunc t = castObj (cast (convType t) string)

getListType :: C.CodeType -> Integer -> C.CodeType
getListType _ 0 = error "No index given"
getListType (C.List t) 1 = t
getListType (C.List t) n = getListType t (n-1)
getListType _ _ = error "Not a list type"

listBase :: C.CodeType -> C.CodeType
listBase (C.List t) = listBase t
listBase t = t
