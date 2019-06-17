{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import(generator, generateCode) where

import Language.Drasil hiding (int, ($.), log, ln, exp,
  sin, cos, tan, csc, sec, cot, arcsin, arccos, arctan)
import Database.Drasil(ChunkDB, symbLookup, symbolTable)
import Language.Drasil.Code.Code as C (Code(..), CodeType(List, File, Char, 
  Float, Object, String, Boolean, Integer))
import Language.Drasil.Code.Imperative.Symantics (Label,
  PackageSym(..), RenderSym(..), PermanenceSym(..), BodySym(..), BlockSym(..), 
  StateTypeSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), Selector(..), FunctionSym(..), SelectorFunction(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.Build.AST (asFragment, buildAll,    
  BuildConfig, buildSingle, cppCompiler, inCodePackage, interp, interpMM, 
  mainModule, mainModuleFile, nativeBinary, osClassDefault, Runnable, withExt)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.Helpers (ModData(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer 
  (jNameOpts)
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
import Data.List (nub, intersperse, (\\), stripPrefix)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Data.Map (member)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe, maybe, maybeToList, catMaybes, mapMaybe)
import Control.Applicative ((<$>))
import Control.Monad (when,liftM2,liftM3,zipWithM)
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Control.Lens ((^.))
import qualified Prelude as P ((<>))

-- Private State, used to push these options around the generator
data State repr = State {
  codeSpec :: CodeSpec,
  inStruct :: Structure,
  logName :: String,
  logKind :: Logging,
  commented :: Comments,
  currentModule :: String,

  sfwrCBody :: (RenderSym repr) => Expr -> repr (Body repr),
  physCBody :: (RenderSym repr) => Expr -> repr (Body repr)
}

-- function to choose how to deal with
-- 1. constraints
-- 2. how to structure the input "module"
-- 3. logging assignments
chooseConstr :: (RenderSym repr) => ConstraintBehaviour -> Expr -> repr
  (Body repr)
chooseConstr Warning   = constrWarn
chooseConstr Exception = constrExc

chooseInStructure :: (RenderSym repr) => Structure -> Reader (State repr) 
  [repr (Module repr)]
chooseInStructure Loose   = genInputModNoClass
chooseInStructure AsClass = genInputModClass

chooseLogging :: (RenderSym repr) => Logging -> (repr (Value repr) -> 
  repr (Value repr) -> Reader (State repr) (repr (Statement repr)))
chooseLogging LogVar = loggedAssign
chooseLogging LogAll = loggedAssign
chooseLogging _      = \x y -> return $ assign x y

initLogFileVar :: (RenderSym repr) => Logging -> [repr (Statement repr)]
initLogFileVar LogVar = [varDec "outfile" outfile]
initLogFileVar LogAll = [varDec "outfile" outfile]
initLogFileVar _ = []


generator :: (RenderSym repr) => Choices -> CodeSpec -> State repr
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

assign' :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) ->
  Reader (State repr) (repr (Statement repr))
assign' x y = do
  g <- ask
  chooseLogging (logKind g) x y

publicMethod :: (RenderSym repr) => repr (MethodType repr) -> Label -> 
  [ParamData repr] -> Reader (State repr) [repr (Block repr)] -> 
  Reader (State repr) (repr (Method repr))
publicMethod mt l pl u = do
  g <- ask
  genMethodCall public static_ (commented g) (logKind g) mt l pl u

generateCode :: (PackageSym repr) => Lang -> [repr (Package repr) -> 
  ([ModData], Label)] -> State repr -> IO ()
generateCode l unRepr g =
  do workingDir <- getCurrentDirectory
     createDirectoryIfMissing False (getDir l)
     setCurrentDirectory (getDir l)
     when (l == Java) $ createDirectoryIfMissing False prog
     createCodeFiles $ makeBuild (last unRepr pckg) (getBuildConfig l) (getRunnable l) (getExt l) $ C.Code $
            map (if l == Java then \(c,d) -> (prog ++ "/" ++ c, d) else id) $
            C.unCode $ makeCode (map (fst . ($ pckg)) unRepr) (getExt l)
     setCurrentDirectory workingDir
  where prog = case codeSpec g of { CodeSpec {program = pp} -> programName pp }
        pckg = runReader (genPackage prog) g

genPackage :: (PackageSym repr) => String -> Reader (State repr) 
  (repr (Package repr))
genPackage n = packMods n <$> genFiles

genFiles :: (RenderSym repr) => Reader (State repr) [repr (RenderFile repr)]
genFiles = map fileDoc <$> genModules

genModules :: (RenderSym repr) => Reader (State repr) [repr (Module repr)]
genModules = do
  g <- ask
  let s = codeSpec g
  mn     <- genMain
  inp    <- chooseInStructure $ inStruct g
  out    <- genOutputMod
  moddef <- traverse genModDef (mods s) -- hack ?
  return $ mn : inp ++ out ++ moddef

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"

getExt :: Lang -> [Label]
getExt Java = [".java"]
getExt Python = [".py"]
getExt CSharp = [".cs"]
getExt Cpp = [".hpp", ".cpp"]

getRunnable :: Lang -> Runnable
getRunnable Java = interp (flip withExt ".class" $ inCodePackage mainModule) 
  jNameOpts "java"
getRunnable Python = interpMM "python"
getRunnable CSharp = nativeBinary
getRunnable Cpp = nativeBinary

getBuildConfig :: Lang -> Maybe BuildConfig
getBuildConfig Java = buildSingle (\i _ -> asFragment "javac" : i) $
  inCodePackage mainModuleFile
getBuildConfig Python = Nothing
getBuildConfig CSharp = buildAll $ \i o -> [osClassDefault "CSC" "csc" "mcs", 
  asFragment "-out:" P.<> o] ++ i
getBuildConfig Cpp = buildAll $ \i o -> cppCompiler : i ++ map asFragment
  ["--std=c++11", "-o"] ++ [o]

liftS :: Reader a b -> Reader a [b]
liftS = fmap (: [])

------- INPUT ----------

genInputModClass :: (RenderSym repr) => Reader (State repr) [repr (Module repr)]
genInputModClass = do
  inputClass <- genInputClass
  derived <- genInputDerived
  constrs <- genInputConstraints
  let ic = maybeToList inputClass
      dl = maybeToList derived
      cl = maybeToList constrs
  sequence [ genModule "InputParameters" Nothing (Just $ return ic),
             genModule "DerivedValues" (Just $ return dl) Nothing,
             genModule "InputConstraints" (Just $ return cl) Nothing
           ]

genInputModNoClass :: (RenderSym repr) => Reader (State repr)
  [repr (Module repr)]
genInputModNoClass = do
  inpDer    <- genInputDerived
  inpConstr <- genInputConstraints
  return [ buildModule "InputParameters" [] []
           (catMaybes [inpDer, inpConstr])
           []
         ]

genInputClass :: (RenderSym repr) => Reader (State repr) (Maybe (repr (Class 
  repr)))
genInputClass = do
  g <- ask
  let ins       = inputs $ codeSpec g
      genClass :: (RenderSym repr) => [String] -> Reader (State repr) (Maybe 
        (repr (Class repr)))
      genClass [] = return Nothing 
      genClass _ = do
        let inputVars = map (\x -> pubMVar 0 (codeName x) (convType $ 
              codeType x)) ins
            varsList  = map (objVarSelf . codeName) ins
            vals      = map (getDefaultValue . codeType) ins
        asgs <- zipWithM assign' varsList vals
        return $ Just $ pubClass "InputParameters" Nothing inputVars
          [ constructor "InputParameters" [] (body [block (initLogFileVar 
          (logKind g) ++ asgs)]) ]
  genClass $ mapMaybe (\x -> Map.lookup (codeName x) (eMap $ codeSpec g)) ins

genInputConstraints :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Method repr)))
genInputConstraints = do
  g <- ask
  let cm       = cMap $ codeSpec g
      genConstraints :: (RenderSym repr) => Maybe String -> Reader (State repr) 
        (Maybe (repr (Method repr)))
      genConstraints Nothing = return Nothing
      genConstraints (Just _) = do
        h <- ask
        parms <- getConstraintParams
        let varsList = filter (\i -> member (i ^. uid) cm) (inputs $ codeSpec h)
            sfwrCs   = concatMap (renderC . sfwrLookup cm) varsList
            physCs   = concatMap (renderC . physLookup cm) varsList
        sf <- mapM (\x -> do { e <- convExpr x; return $ ifNoElse [((?!) e, 
          sfwrCBody h x)]}) sfwrCs
        hw <- mapM (\x -> do { e <- convExpr x; return $ ifNoElse [((?!) e, 
          physCBody h x)]}) physCs
        mthd <- publicMethod void "input_constraints" parms (return [block sf, 
          block hw])
        return $ Just mthd
  genConstraints $ Map.lookup "input_constraints" (eMap $ codeSpec g)

genInputDerived :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Method repr)))
genInputDerived = do
  g <- ask
  let dvals = derivedInputs $ codeSpec g
      genDerived :: (RenderSym repr) => Maybe String -> Reader (State repr) 
        (Maybe (repr (Method repr)))
      genDerived Nothing = return Nothing
      genDerived (Just _) = do
        parms <- getDerivedParams
        inps <- mapM (\x -> genCalcBlock CalcAssign (codeName x) (codeEquat x)) dvals
        mthd <- publicMethod void "derived_values" parms (return inps)
        return $ Just mthd
  genDerived $ Map.lookup "derived_values" (eMap $ codeSpec g)

-- need Expr -> String to print constraint
constrWarn :: (RenderSym repr) => Expr -> repr (Body repr)
constrWarn _ = oneLiner $ printStrLn "Warning: constraint violated"

constrExc :: (RenderSym repr) => Expr -> repr (Body repr)
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
  parms <- getCalcParams cdef
  blck <- genCalcBlock CalcReturn (codeName cdef) (codeEquat cdef)
  publicMethod
    (mState $ convType (codeType cdef))
    (codeName cdef)
    parms
    (return [blck])

data CalcType = CalcAssign | CalcReturn deriving Eq

genCalcBlock :: (RenderSym repr) => CalcType -> String -> Expr -> Reader (State
  repr) (repr (Block repr))
genCalcBlock t v (Case e) = genCaseBlock t v e
genCalcBlock t v e
    | t == CalcAssign  = fmap block $ liftS $ do { vv <- variable v; ee <-
      convExpr e; assign' vv ee}
    | otherwise        = block <$> liftS (returnState <$> convExpr e)

genCaseBlock :: (RenderSym repr) => CalcType -> String -> [(Expr,Relation)] ->
  Reader (State repr) (repr (Block repr))
genCaseBlock t v cs = do
  ifs <- mapM (\(e,r) -> liftM2 (,) (convExpr r) (fmap body $ liftS $
    genCalcBlock t v e)) cs
  return $ block [ifNoElse ifs]

----- OUTPUT -------

genOutputMod :: (RenderSym repr) => Reader (State repr) [repr
  (Module repr)]
genOutputMod = do
  outformat <- genOutputFormat
  let outf = maybeToList outformat
  liftS $ genModule "OutputFormat" (Just $ return outf) Nothing

genOutputFormat :: (RenderSym repr) => Reader (State repr) (Maybe (repr 
  (Method repr)))
genOutputFormat = do
  g <- ask
  let genOutput :: (RenderSym repr) => Maybe String -> Reader (State repr) 
        (Maybe (repr (Method repr)))
      genOutput Nothing = return Nothing
      genOutput (Just _) = do
        let l_outfile = "outputfile"
            v_outfile = var l_outfile
        parms <- getOutputParams
        outp <- mapM (\x -> do
          v <- variable $ codeName x
          return [ printFileStr v_outfile (codeName x ++ " = "),
                   printFileLn v_outfile (convType $ codeType x) v
                 ] ) (outputs $ codeSpec g)
        mthd <- publicMethod void "write_output" parms (return [block $
          [
          varDec l_outfile outfile,
          openFileW v_outfile (litString "output.txt") ] ++
          concat outp ++ [ closeFile v_outfile ]])
        return $ Just mthd
  genOutput $ Map.lookup "write_output" (eMap $ codeSpec g)

-----

genMethodCall :: (RenderSym repr) => repr (Scope repr) -> repr
  (Permanence repr) -> Comments -> Logging -> repr (MethodType repr) ->
  Label -> [ParamData repr] -> Reader (State repr) [repr (Block repr)] -> 
  Reader (State repr) (repr (Method repr))
genMethodCall s pr doComments doLog t n p b = do
  let loggedBody LogFunc = loggedMethod n pTypes pNames b
      loggedBody LogAll  = loggedMethod n pTypes pNames b
      loggedBody _       = b
      commBody CommentFunc = commMethod n pNames
      commBody _           = id
      pTypes = map paramType p
      pNames = map paramName p
  bod <- commBody doComments (loggedBody doLog)
  return $ function n s pr t (map param p) (body bod)

commMethod :: (RenderSym repr) => Label -> [Label] -> Reader (State repr) 
  [repr (Block repr)] -> Reader (State repr) [repr (Block repr)]
commMethod n l b = do
  g <- ask
  rest <- b
  return $ block [
      comment $ "function '" ++ n ++ "': " ++ funcTerm n (fMap $ codeSpec g),
      multi $ map
        (\x -> comment $ "parameter '" ++ x ++ "': " ++ varTerm x (vMap $ 
          codeSpec g)) l
    ] : rest 

loggedMethod :: (RenderSym repr) => Label -> [repr (StateType repr)] ->
  [Label] -> Reader (State repr) [repr (Block repr)] -> Reader (State repr)
  [repr (Block repr)]
loggedMethod n st l b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    g <- ask
    rest <- b
    return $ block [
      varDec l_outfile outfile,
      openFileA v_outfile (litString $ logName g),
      printFileStr v_outfile ("function " ++ n ++ "("),
      printParams st l v_outfile,
      printFileStrLn v_outfile ") called",
      closeFile v_outfile ]
      : rest
  where
    printParams sts ls v_outfile = multi $
      intersperse (printFileStr v_outfile ", ") $
      map (\(x,y) -> printFile v_outfile x (var y)) (zip sts ls)

---- MAIN ---

genModule :: (RenderSym repr) => Name
               -> Maybe (Reader (State repr) [repr (Method repr)])
               -> Maybe (Reader (State repr) [repr (Class repr)])
               -> Reader (State repr) (repr (Module repr))
genModule n maybeMs maybeCs = do
  g <- ask
  let ls = fromMaybe [] (Map.lookup n (dMap $ codeSpec g))
      updateState = withReader (\s -> s { currentModule = n })
  cs <- maybe (return []) updateState maybeCs
  ms <- maybe (return []) updateState maybeMs
  return $ buildModule n ls [] ms cs


genMain :: (RenderSym repr) => Reader (State repr) (repr (Module repr))
genMain = genModule "Control" (Just $ liftS genMainFunc) Nothing

genMainFunc :: (RenderSym repr) => Reader (State repr) (repr (Method repr))
genMainFunc =
  let l_filename = "inputfile"
  in do
    g <- ask
    ip <- getInputDecl
    gi <- getInputCall
    dv <- getDerivedCall
    ic <- getConstraintCall
    varDef <- mapM getCalcCall (execOrder $ codeSpec g)
    wo <- getOutputCall
    return $ mainMethod "" $ bodyStatements $
      varDecDef l_filename string (arg 0) :
      catMaybes ([ip, gi, dv, ic] ++ varDef ++ [wo])

getInputDecl :: (RenderSym repr) => Reader (State repr) (Maybe (repr (
  Statement repr)))
getInputDecl = do
  g <- ask
  let l_params = "inParams"
      getDecl :: (RenderSym repr) => Structure -> [CodeChunk] -> Maybe (repr 
        (Statement repr))
      getDecl _ [] = Nothing
      getDecl Loose ins = Just $ multi $ map (\x -> varDecDef (codeName x) 
        (convType $ codeType x) (getDefaultValue $ codeType x)) ins
      getDecl AsClass _ = Just $ extObjDecNewVoid l_params "InputParameters" 
        (obj "InputParameters") 
  return $ getDecl (inStruct g) (inputs $ codeSpec g)

getFuncCall :: (RenderSym repr) => String -> Reader (State repr) 
  [ParamData repr] -> Reader (State repr) (Maybe (repr (Value repr)))
getFuncCall n funcPs = do
  g <- ask
  let getCall Nothing = return Nothing
      getCall (Just m) = do
        ps <- funcPs
        let pvals = getArgs ps
        val <- fApp m n pvals
        return $ Just val
  getCall $ Map.lookup n (eMap $ codeSpec g)

getInputCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getInputCall = do
  val <- getFuncCall (funcPrefix ++ "get_input") getInputFormatParams
  return $ fmap valState val

getDerivedCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getDerivedCall = do
  val <- getFuncCall "derived_values" getDerivedParams
  return $ fmap valState val

getConstraintCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getConstraintCall = do
  val <- getFuncCall "input_constraints" getConstraintParams
  return $ fmap valState val

getCalcCall :: (RenderSym repr) => CodeDefinition -> Reader (State repr) 
  (Maybe (repr (Statement repr)))
getCalcCall c = do
  val <- getFuncCall (codeName c) (getCalcParams c)
  return $ fmap (varDecDef (nopfx $ codeName c) (convType $ codeType c)) val

getOutputCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getOutputCall = do
  val <- getFuncCall "write_output" getOutputParams
  return $ fmap valState val

getInputFormatParams :: (RenderSym repr) => Reader (State repr) [ParamData repr]
getInputFormatParams = do 
  g <- ask
  let ins = extInputs $ codeSpec g
      l_filename = "inputfile"
  ps <- getParams ins
  return $ PD (stateParam l_filename infile) infile l_filename : ps

getDerivedParams :: (RenderSym repr) => Reader (State repr) [ParamData repr]
getDerivedParams = do
  g <- ask
  let dvals = derivedInputs $ codeSpec g
      reqdVals = concatMap (flip codevars (sysinfodb $ codeSpec g) . codeEquat) 
        dvals
  getParams reqdVals

getConstraintParams :: (RenderSym repr) => Reader (State repr) [ParamData repr]
getConstraintParams = do 
  g <- ask
  let cm = cMap $ codeSpec g
      mem = eMap $ codeSpec g
      db = sysinfodb $ codeSpec g
      varsList = filter (\i -> member (i ^. uid) cm) (inputs $ codeSpec g)
      reqdVals = nub $ varsList ++ concatMap (\v -> constraintvarsandfuncs v db 
        mem) (getConstraints cm varsList)
  getParams reqdVals

getCalcParams :: (RenderSym repr) => CodeDefinition -> Reader (State repr) 
  [ParamData repr]
getCalcParams c = do
  g <- ask
  getParams $ codevars' (codeEquat c) $ sysinfodb $ codeSpec g

getOutputParams :: (RenderSym repr) => Reader (State repr) [ParamData repr]
getOutputParams = do
  g <- ask
  getParams $ outputs $ codeSpec g

-----

loggedAssign :: (RenderSym repr) => repr (Value repr) -> 
  repr (Value repr) -> Reader (State repr) (repr (Statement repr))
loggedAssign a b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in do
    g <- ask
    return $ multi [
      assign a b,
      openFileA v_outfile (litString $ logName g),
      printFileStr v_outfile ("var '" ++ valName a ++ "' assigned to "),
      printFile v_outfile (convType $ varType (valName a) (vMap $ codeSpec g))
        a,
      printFileStrLn v_outfile (" in module " ++ currentModule g),
      closeFile v_outfile ]

-- helpers

nopfx :: String -> String
nopfx s = fromMaybe s (stripPrefix funcPrefix s)

variable :: (RenderSym repr) => String -> Reader (State repr) 
  (repr (Value repr))
variable s' = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
      doit :: (RenderSym repr) => String -> Reader (State repr) 
        (repr (Value repr))
      doit s | member s mm =
        maybe (error "impossible") (convExpr . codeEquat) (Map.lookup s mm) --extvar "Constants" s
             | s `elem` map codeName (inputs cs) = return $ var "inParams" $-> 
               var s
             | otherwise                         = return $ var s
  doit s'
  
fApp :: (RenderSym repr) => String -> String -> [repr (Value repr)] -> 
  Reader (State repr) (repr (Value repr))
fApp m s vl = do
  g <- ask
  return $ if m /= currentModule g then extFuncApp m s vl else funcApp s vl

data ParamData repr = PD {
  param :: (ParameterSym repr) => repr (Parameter repr),
  paramType :: (StateTypeSym repr) => repr (StateType repr),
  paramName :: Label
}

getParams :: (RenderSym repr) => [CodeChunk] -> Reader (State repr) 
  [ParamData repr]
getParams cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      consts = map codevar $ constants $ codeSpec g
      inpParams = filter (`elem` ins) cs
      inPs = getInputParams (inStruct g) inpParams
      conParams = filter (`elem` consts) cs
      conPs = getConstParams conParams
      csSubIns = cs \\ (ins ++ consts)
      ps = map mkParam csSubIns
  return $ inPs ++ conPs ++ ps

mkParam :: (RenderSym repr) => CodeChunk -> ParamData repr
mkParam p = PD ((paramFunc $ codeType p) pName pType) pType pName
  where paramFunc (C.List _) = pointerParam
        paramFunc _ = stateParam
        pName = codeName p
        pType = convType $ codeType p

getInputParams :: (RenderSym repr) => Structure -> [CodeChunk] ->
  [ParamData repr]
getInputParams _ [] = []
getInputParams Loose cs = map mkParam cs
getInputParams AsClass _ = [PD (pointerParam pName pType) pType pName]
  where pName = "inParams"
        pType = obj "InputParameters"

-- Right now, we always inline constants. In the future, this will be captured by a choice and this function should be updated to read that choice
getConstParams :: [CodeChunk] -> [ParamData repr]
getConstParams _ = []

getArgs :: (RenderSym repr) => [ParamData repr] -> [repr (Value repr)]
getArgs = map (var . paramName)

getDefaultValue :: (RenderSym repr) => C.CodeType -> repr (Value repr)
getDefaultValue C.Boolean = defaultBool
getDefaultValue C.Integer = defaultInt
getDefaultValue C.Float = defaultFloat
getDefaultValue C.Char = defaultChar
getDefaultValue C.String = defaultString
getDefaultValue _ = error "No default value for the given type"

convType :: (RenderSym repr) => C.CodeType -> repr (StateType repr)
convType C.Boolean = bool
convType C.Integer = int
convType C.Float = float
convType C.Char = char
convType C.String = string
convType (C.List t) = getListTypeFunc t dynamic_
convType (C.Object n) = obj n
convType C.File = error "convType: File ?"

convExpr :: (RenderSym repr) => Expr -> Reader (State repr) (repr (Value repr))
convExpr (Dbl d) = return $ litFloat d
convExpr (Int i) = return $ litInt i
convExpr (Str s) = return $ litString s
convExpr Perc{}  = error "convExpr: Perc"
convExpr (AssocA Add l) = foldr1 (#+)  <$> mapM convExpr l
convExpr (AssocA Mul l) = foldr1 (#*)  <$> mapM convExpr l
convExpr (AssocB And l) = foldr1 (?&&) <$> mapM convExpr l
convExpr (AssocB Or l)  = foldr1 (?||) <$> mapM convExpr l
convExpr Deriv{} = return $ litString "**convExpr :: Deriv unimplemented**"
convExpr (C c)   = do
  g <- ask
  variable $ codeName $ codevar (symbLookup c (symbolTable $ sysinfodb $ 
    codeSpec g))
convExpr (FCall (C c) x) = do
  g <- ask
  let info = sysinfodb $ codeSpec g
      mem = eMap $ codeSpec g
      funcNm = codeName (codefunc (symbLookup c (symbolTable info)))
  args <- mapM convExpr x
  maybe (error $ "Call to non-existent function" ++ funcNm) 
    (\f -> fApp f funcNm args) (Map.lookup funcNm mem)
convExpr FCall{}   = return $ litString "**convExpr :: FCall unimplemented**"
convExpr (UnaryOp o u) = fmap (unop o) (convExpr u)
convExpr (BinaryOp Frac (Int a) (Int b)) =
  return $ litFloat (fromIntegral a) #/ litFloat (fromIntegral b) -- hack to deal with integer division
convExpr (BinaryOp Eq a b@(Str _)) = liftM2 stringEqual (convExpr a) 
  (convExpr b) -- hack to deal with string equality
convExpr (BinaryOp o a b)  = liftM2 (bfunc o) (convExpr a) (convExpr b)
convExpr (Case l)      = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen"
    doit [(e,_)] = convExpr e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExpr cond) (convExpr e) 
      (convExpr (Case xs))
convExpr Matrix{}    = error "convExpr: Matrix"
convExpr Operator{} = error "convExpr: Operator"
convExpr IsIn{}    = error "convExpr: IsIn"
convExpr (RealI c ri)  = do
  g <- ask
  convExpr $ renderRealInt (lookupC (sysinfodb $ codeSpec g) c) ri

getUpperBound :: Expr -> Expr
getUpperBound (BinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

lookupC :: ChunkDB -> UID -> QuantityDict
lookupC sm c = symbLookup c $ symbolTable sm

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
unop Dim  = listSizeAccess
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
bfunc Index = \x y -> x $. listAccess y

-- medium hacks --
genModDef :: (RenderSym repr) => CS.Mod -> Reader (State repr) 
  (repr (Module repr))
genModDef (CS.Mod n fs) = genModule n (Just $ mapM genFunc fs) Nothing

genFunc :: (RenderSym repr) => Func -> Reader (State repr) (repr (Method repr))
genFunc (FDef (FuncDef n i o s)) = do
  g <- ask
  parms <- getParams i
  stmts <- mapM convStmt s
  publicMethod (mState $ convType o) n parms
    (return [block $
        map (\x -> varDec (codeName x) (convType $ codeType x))
          (fstdecl (sysinfodb (codeSpec g)) s \\ i)
        ++ stmts
    ])
genFunc (FData (FuncData n ddef)) = genDataFunc n ddef
genFunc (FCD cd) = genCalcFunc cd

convStmt :: (RenderSym repr) => FuncStmt -> Reader (State repr) 
  (repr (Statement repr))
convStmt (FAsg v e) = convExpr e >>= assign' (var $ codeName v)
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
convStmt (FDec v (C.List t)) = return $ listDec (codeName v) 0 
  (getListTypeFunc t dynamic_)
convStmt (FDec v t) = return $ varDec (codeName v) (convType t)
convStmt (FProcCall n l) = do
  e' <- convExpr (FCall (asExpr n) l)
  return $ valState e'
convStmt (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ valState $ a' $. listAppend b'

getListTypeFunc :: (RenderSym repr) => C.CodeType -> repr (Permanence repr) ->
  repr (StateType repr)
getListTypeFunc C.Integer p = intListType p
getListTypeFunc C.Float p = floatListType p
getListTypeFunc C.Boolean _ = boolListType
getListTypeFunc (C.List t) p = listType p $ getListTypeFunc t p
getListTypeFunc t p = listType p (convType t)

-- this is really ugly!!
genDataFunc :: (RenderSym repr) => Name -> DataDesc -> Reader (State repr)
  (repr (Method repr))
genDataFunc nameTitle ddef = do
    parms <- getParams $ getInputs ddef
    inD <- mapM inData ddef
    publicMethod void nameTitle (PD p_filename string l_filename : parms) $
      return [block $ [
      varDec l_infile infile,
      varDec l_line string,
      listDec l_lines 0 (listType dynamic_ string),
      listDec l_linetokens 0 (listType dynamic_ string),
      openFileR v_infile v_filename ] ++
      concat inD ++ [
      closeFile v_infile ]]
  where inData :: (RenderSym repr) => Data -> Reader (State repr) [repr (Statement repr)]
        inData (Singleton v) = do
            vv <- variable $ codeName v
            return [getFileInput (codeType v) v_infile vv]
        inData JunkData = return [discardFileLine v_infile]
        inData (Line lp d) = do
          lnI <- lineData lp (litInt 0)
          return $ [getFileInputLine v_infile v_line, 
            stringSplit d v_linetokens v_line] ++ lnI
        inData (Lines lp Nothing d) = do
          lnV <- lineData lp v_i
          return [ getFileInputAll v_infile v_lines,
            forRange l_i (litInt 0) (listSizeAccess v_lines) (litInt 1)
              (bodyStatements $ stringSplit d v_linetokens (v_lines $.
                listAccess v_i) : lnV)
            ]
        inData (Lines lp (Just numLines) d) = do
          lnV <- lineData lp v_i
          return [ forRange l_i (litInt 0) (litInt numLines) (litInt 1)
            ( bodyStatements $
              [getFileInputLine v_infile v_line,
               stringSplit d v_linetokens v_line
              ] ++ lnV)
            ]
        ---------------
        lineData :: (RenderSym repr) => LinePattern -> repr (Value repr)
          -> Reader (State repr) [repr (Statement repr)]
        lineData (Straight p) lineNo = patternData p lineNo (litInt 0)
        lineData (Repeat p Nothing) lineNo = do
          pat <- patternData p lineNo v_j
          return [forRange l_j (litInt 0) (castObj (cast int float)
            (listSizeAccess v_linetokens #/ litInt (toInteger $ length p))) 
            (litInt 1) ( bodyStatements pat )]
        lineData (Repeat p (Just numPat)) lineNo = do
          pat <- patternData p lineNo v_j
          return [forRange l_j (litInt 0) (litInt numPat) (litInt 1) 
            ( bodyStatements pat )]
        ---------------
        patternData :: (RenderSym repr) => [Entry] -> repr (Value repr)
          -> repr (Value repr) -> Reader (State repr) [repr (Statement repr)]
        patternData d lineNo patNo = do
          let l = toInteger $ length d
          ent <- mapM (\(x,y) -> entryData x lineNo patNo y) $ 
            zip (map (\z -> (patNo #* litInt l) #+ litInt z) [0..l-1]) d
          return $ concat ent
        ---------------
        entryData :: (RenderSym repr) => repr (Value repr) -> repr 
          (Value repr) -> repr (Value repr) -> Entry -> Reader (State repr)
          [repr (Statement repr)]
        entryData tokIndex _ _ (Entry v) = do
          vv <- variable $ codeName v
          a <- assign' vv $ getCastFunc (codeType v)
            (v_linetokens $. listAccess tokIndex)
          return [a]
        entryData tokIndex lineNo patNo (ListEntry indx v) = do
          vv <- variable $ codeName v
          return $ checkIndex indx 1 lineNo patNo vv (codeType v) ++ [
            valState $ indexData indx lineNo patNo vv $. 
            listSet (getIndex indx lineNo patNo)
            (getCastFunc (getListType (codeType v) (toInteger $ length indx))
            (v_linetokens $. listAccess tokIndex))]
        entryData _ _ _ JunkEntry = return []
        ---------------
        indexData :: (RenderSym repr) => [Ind] -> repr (Value repr) ->
          repr (Value repr) -> repr (Value repr) -> repr (Value repr)
        indexData [_] _ _ v = v
        indexData (Explicit i : is) l p v = indexData is l p (objAccess v 
          (listAccess $ litInt i))
        indexData (WithLine : is) l p v = indexData is l p (objAccess v 
          (listAccess l))
        indexData (WithPattern : is) l p v = indexData is l p (objAccess v
          (listAccess p))
        indexData [] _ _ _ = error "indexData called with empty index list"
        ------------------------------
        getIndex :: (RenderSym repr) => [Ind] -> repr (Value repr) ->
          repr (Value repr) -> repr (Value repr)
        getIndex [Explicit i] _ _ = litInt i
        getIndex [WithLine] l _ = l
        getIndex [WithPattern] _ p = p
        getIndex (_:xs) l p = getIndex xs l p
        getIndex [] _ _ = error "getIndex called with empty index list"
        ---------------
        checkIndex :: (RenderSym repr) => [Ind] -> Integer-> 
          repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> 
          C.CodeType -> [repr (Statement repr)]
        checkIndex [] _ _ _ _ _ = []
        checkIndex (Explicit i : is) n l p v s =
          while (listSizeAccess v ?<= litInt i) (bodyStatements [ 
            valState $
            v $. getListExtend (getListType s n) ] )
            : checkIndex is (n+1) l p (v $. listAccess (litInt i)) s
        checkIndex (WithLine : is) n l p v s =
          while (listSizeAccess  v ?<= l) (bodyStatements [ valState $
          v $. getListExtend (getListType s n) ])
          : checkIndex is (n+1) l p (v $. listAccess l) s
        checkIndex (WithPattern : is) n l p v s =
          while (listSizeAccess v ?<= p) (bodyStatements [ valState $ 
          v $. getListExtend (getListType s n) ] )
          : checkIndex is (n+1) l p (v $. listAccess p) s
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

getFileInput :: (RenderSym repr) => C.CodeType -> (repr (Value repr) -> repr
  (Value repr) -> repr (Statement repr))
getFileInput C.Boolean = getBoolFileInput
getFileInput C.Integer = getIntFileInput
getFileInput C.Float = getFloatFileInput
getFileInput C.Char = getCharFileInput
getFileInput C.String = getStringFileInput
getFileInput _ = error "No getFileInput function for the given type"

getListExtend :: (RenderSym repr) => C.CodeType -> repr (Function repr)
getListExtend C.Boolean = listExtendBool
getListExtend C.Integer = listExtendInt
getListExtend C.Float = listExtendFloat
getListExtend C.Char = listExtendChar
getListExtend C.String = listExtendString
getListExtend t@(C.List _) = listExtendList (getNestDegree t) (convType t)
getListExtend _ = error "No listExtend function for the given type"

getNestDegree :: C.CodeType -> Integer
getNestDegree (C.List t) = 1 + getNestDegree t
getNestDegree _ = 0

getCastFunc :: (RenderSym repr) => C.CodeType -> repr (Value repr) ->
   repr (Value repr)
getCastFunc C.Float = castStrToFloat
getCastFunc t = castObj (cast (convType t) string)

getListType :: C.CodeType -> Integer -> C.CodeType
getListType _ 0 = error "No index given"
getListType (C.List t) 1 = t
getListType (C.List t) n = getListType t (n-1)
getListType _ _ = error "Not a list type"
