{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import(generator, generateCode) where

import Language.Drasil hiding (int, ($.), log, ln, exp,
  sin, cos, tan, csc, sec, cot, arcsin, arccos, arctan)
import Database.Drasil(ChunkDB, symbLookup, symbolTable)
import Language.Drasil.Code.Code as C (Code(..), CodeType(List))
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
import Language.Drasil.Code.Imperative.Helpers (ModData(..), convType)
import Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer 
  (cppExts)
import Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer 
  (csExts)
import Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer 
  (jExts, jNameOpts)
import Language.Drasil.Code.Imperative.LanguageRenderer.PythonRenderer 
  (pyExts)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code (CodeChunk, CodeDefinition, codeName,
  codeType, codevar, codefunc, codeEquat, funcPrefix, physLookup, sfwrLookup,
  programName)
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))
import qualified Language.Drasil.CodeSpec as CS (Mod(..))
import Language.Drasil.Code.DataDesc (Entry(JunkEntry, ListEntry, Entry),
  LinePattern(Repeat, Straight), Data(Line, Lines, JunkData, Singleton), 
  DataDesc, getInputs, getPatternInputs, junkLine, singleton)

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
chooseInStructure Unbundled   = genInputModNoClass
chooseInStructure Bundled = genInputModClass

chooseLogging :: (RenderSym repr) => Logging -> (repr (Value repr) -> 
  Reader (State repr) (Maybe (repr (Statement repr))))
chooseLogging LogVar v = Just <$> loggedVar v
chooseLogging LogAll v = Just <$> loggedVar v
chooseLogging _      _ = return Nothing

initLogFileVar :: (RenderSym repr) => Logging -> [repr (Statement repr)]
initLogFileVar LogVar = [varDec $ var "outfile" outfile]
initLogFileVar LogAll = [varDec $ var "outfile" outfile]
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

maybeLog :: (RenderSym repr) => repr (Value repr) ->
  Reader (State repr) [repr (Statement repr)]
maybeLog v = do
  g <- ask
  l <- chooseLogging (logKind g) v
  return $ maybeToList l

publicMethod :: (RenderSym repr) => repr (MethodType repr) -> Label -> 
  [repr (Parameter repr)] -> Reader (State repr) [repr (Block repr)] -> 
  Reader (State repr) (repr (Method repr))
publicMethod mt l pl u = do
  g <- ask
  genMethodCall public static_ (commented g) (logKind g) mt l pl u

publicInOutFunc :: (RenderSym repr) => Label -> [repr (Value repr)] -> 
  [repr (Value repr)] -> Reader (State repr) [repr (Block repr)] -> 
  Reader (State repr) (repr (Method repr))
publicInOutFunc l ins outs u = do
  g <- ask
  genInOutFunc public static_ (commented g) (logKind g) l ins outs u

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
  let s = csi $ codeSpec g
  mn     <- genMain
  inp    <- chooseInStructure $ inStruct g
  inpf   <- genInputFormatMod
  out    <- genOutputMod
  moddef <- traverse genModDef (mods s) -- hack ?
  return $ mn : inp ++ inpf ++ out ++ moddef

-- private utilities used in generateCode
getDir :: Lang -> String
getDir Cpp = "cpp"
getDir CSharp = "csharp"
getDir Java = "java"
getDir Python = "python"

getExt :: Lang -> [Label]
getExt Java = jExts
getExt Python = pyExts
getExt CSharp = csExts
getExt Cpp = cppExts

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
genInputModClass = sequence 
  [genModule "InputParameters" Nothing (Just $ fmap maybeToList genInputClass),
  genModule "DerivedValues" (Just $ fmap maybeToList genInputDerived) Nothing,
  genModule "InputConstraints" (Just $ fmap maybeToList genInputConstraints) 
    Nothing]

genInputModNoClass :: (RenderSym repr) => Reader (State repr)
  [repr (Module repr)]
genInputModNoClass = liftS $
  genModule "InputParameters" (Just $ concat <$> mapM (fmap maybeToList) 
    [genInputDerived, genInputConstraints]) Nothing

genInputClass :: (RenderSym repr) => Reader (State repr) (Maybe (repr (Class 
  repr)))
genInputClass = do
  g <- ask
  let ins       = inputs $ codeSpec g
      genClass :: (RenderSym repr) => [String] -> Reader (State repr) (Maybe 
        (repr (Class repr)))
      genClass [] = return Nothing 
      genClass _ = do
        let inputVars = map (\x -> pubMVar 0 (var (codeName x) (convType $ 
              codeType x))) ins
        return $ Just $ pubClass "InputParameters" Nothing inputVars []
  genClass $ mapMaybe (\x -> Map.lookup (codeName x) (eMap $ codeSpec g)) ins

genInputConstraints :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Method repr)))
genInputConstraints = do
  g <- ask
  let cm       = cMap $ csi $ codeSpec g
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
        mthd <- publicMethod (mState void) "input_constraints" parms (return 
          [block sf, block hw])
        return $ Just mthd
  genConstraints $ Map.lookup "input_constraints" (eMap $ codeSpec g)

genInputDerived :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Method repr)))
genInputDerived = do
  g <- ask
  let dvals = derivedInputs $ csi $ codeSpec g
      genDerived :: (RenderSym repr) => Maybe String -> Reader (State repr) 
        (Maybe (repr (Method repr)))
      genDerived Nothing = return Nothing
      genDerived (Just _) = do
        parms <- getDerivedParams
        inps <- mapM (\x -> genCalcBlock CalcAssign (codeName x) (convType $ 
          codeType x) (codeEquat x)) dvals
        mthd <- publicMethod (mState void) "derived_values" parms (return inps)
        return $ Just mthd
  genDerived $ Map.lookup "derived_values" (eMap $ codeSpec g)

-- need Expr -> String to print constraint
constrWarn :: (RenderSym repr) => Expr -> repr (Body repr)
constrWarn _ = oneLiner $ printStrLn "Warning: constraint violated"

constrExc :: (RenderSym repr) => Expr -> repr (Body repr)
constrExc _ = oneLiner $ throw "InputError"

genInputFormatMod :: (RenderSym repr) => Reader (State repr) 
  [repr (Module repr)]
genInputFormatMod = liftS $ genModule "InputFormat" (Just $ 
  fmap maybeToList genInputFormat) Nothing

genInputFormat :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Method repr)))
genInputFormat = do
  g <- ask
  let dd = junkLine : intersperse junkLine (map singleton (extInputs $ csi $
        codeSpec g))
      genInFormat :: (RenderSym repr) => Maybe String -> Reader (State repr) 
        (Maybe (repr (Method repr)))
      genInFormat Nothing = return Nothing
      genInFormat (Just _) = do
        ins <- getInputFormatIns
        outs <- getInputFormatOuts
        mthd <- publicInOutFunc "get_input" ins outs (readData dd)
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
genCalcFunc :: (RenderSym repr) => CodeDefinition -> Reader (State repr) (repr
  (Method repr))
genCalcFunc cdef = do
  parms <- getCalcParams cdef
  blck <- genCalcBlock CalcReturn (codeName cdef) (convType $ codeType cdef) 
    (codeEquat cdef)
  publicMethod
    (mState $ convType (codeType cdef))
    (codeName cdef)
    parms
    (return [blck])

data CalcType = CalcAssign | CalcReturn deriving Eq

genCalcBlock :: (RenderSym repr) => CalcType -> String -> 
  repr (StateType repr) -> Expr -> Reader (State repr) (repr (Block repr))
genCalcBlock t v st (Case e) = genCaseBlock t v st e
genCalcBlock t v st e
    | t == CalcAssign  = fmap block $ liftS $ do { vv <- variable v st; ee <-
      convExpr e; l <- maybeLog vv; return $ multi $ assign vv ee : l}
    | otherwise        = block <$> liftS (returnState <$> convExpr e)

genCaseBlock :: (RenderSym repr) => CalcType -> String -> repr (StateType repr) 
  -> [(Expr,Relation)] -> Reader (State repr) (repr (Block repr))
genCaseBlock t v st cs = do
  ifs <- mapM (\(e,r) -> liftM2 (,) (convExpr r) (fmap body $ liftS $
    genCalcBlock t v st e)) cs
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
            v_outfile = var l_outfile outfile
        parms <- getOutputParams
        outp <- mapM (\x -> do
          v <- variable (codeName x) (convType $ codeType x)
          return [ printFileStr v_outfile (codeName x ++ " = "),
                   printFileLn v_outfile v
                 ] ) (outputs $ csi $ codeSpec g)
        mthd <- publicMethod (mState void) "write_output" parms (return [block $
          [
          varDec v_outfile,
          openFileW v_outfile (litString "output.txt") ] ++
          concat outp ++ [ closeFile v_outfile ]])
        return $ Just mthd
  genOutput $ Map.lookup "write_output" (eMap $ codeSpec g)

-----

genMethodCall :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr)
  -> Comments -> Logging -> repr (MethodType repr) -> Label -> 
  [repr (Parameter repr)] -> Reader (State repr) [repr (Block repr)] -> 
  Reader (State repr) (repr (Method repr))
genMethodCall s pr doComments doLog t n p b = do
  let loggedBody LogFunc = loggedMethod n vals b
      loggedBody LogAll  = loggedMethod n vals b
      loggedBody _       = b
      commBody CommentFunc = commMethod n pNames
      commBody _           = id
      pTypes = map parameterType p
      pNames = map parameterName p
      vals = zipWith var pNames pTypes
  bod <- commBody doComments (loggedBody doLog)
  return $ function n s pr t p (body bod)

genInOutFunc :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) 
  -> Comments -> Logging -> Label -> [repr (Value repr)] -> [repr (Value repr)] 
  -> Reader (State repr) [repr (Block repr)] 
  -> Reader (State repr) (repr (Method repr))
genInOutFunc s pr doComments doLog n ins outs b = do
  let loggedBody LogFunc = loggedMethod n ins b
      loggedBody LogAll  = loggedMethod n ins b
      loggedBody _       = b
      commBody CommentFunc = commMethod n pNames
      commBody _           = id
      pNames = map valueName ins
  bod <- commBody doComments (loggedBody doLog)
  return $ inOutFunc n s pr ins outs (body bod)

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

loggedMethod :: (RenderSym repr) => Label -> [repr (Value repr)] -> 
  Reader (State repr) [repr (Block repr)] -> 
  Reader (State repr) [repr (Block repr)]
loggedMethod n vals b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile outfile
  in do
    g <- ask
    rest <- b
    return $ block [
      varDec v_outfile,
      openFileA v_outfile (litString $ logName g),
      printFileStrLn v_outfile ("function " ++ n ++ " called with inputs: {"),
      multi $ printInputs vals v_outfile,
      printFileStrLn v_outfile "  }",
      closeFile v_outfile ]
      : rest
  where
    printInputs [] _ = []
    printInputs [v] v_outfile = [
      printFileStr v_outfile ("  " ++ valueName v ++ " = "), 
      printFileLn v_outfile v]
    printInputs (v:vs) v_outfile = [
      printFileStr v_outfile ("  " ++ valueName v ++ " = "), 
      printFile v_outfile v, 
      printFileStrLn v_outfile ", "] ++ printInputs vs v_outfile
    

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
  return $ buildModule n ls ms cs


genMain :: (RenderSym repr) => Reader (State repr) (repr (Module repr))
genMain = genModule "Control" (Just $ liftS genMainFunc) Nothing

genMainFunc :: (RenderSym repr) => Reader (State repr) (repr (Method repr))
genMainFunc =
  let v_filename = var "filename" string
  in do
    g <- ask
    logInFile <- maybeLog v_filename
    ip <- getInputDecl
    gi <- getInputCall
    dv <- getDerivedCall
    ic <- getConstraintCall
    varDef <- mapM getCalcCall (execOrder $ csi $ codeSpec g)
    wo <- getOutputCall
    return $ mainMethod "" $ bodyStatements $
      initLogFileVar (logKind g) ++
      varDecDef v_filename (arg 0) : logInFile ++
      catMaybes ([ip, gi, dv, ic] ++ varDef ++ [wo])

getInputDecl :: (RenderSym repr) => Reader (State repr) (Maybe (repr (
  Statement repr)))
getInputDecl = do
  g <- ask
  let v_params = var "inParams" (obj "InputParameters")
      getDecl _ [] = return Nothing
      getDecl Unbundled ins = do
        vals <- mapM (\x -> variable (codeName x) (convType $ codeType x)) ins
        return $ Just $ multi $ map varDec vals
      getDecl Bundled _ = return $ Just $ extObjDecNewVoid "InputParameters"
        v_params 
  getDecl (inStruct g) (inputs $ codeSpec g)

getFuncCall :: (RenderSym repr) => String -> repr (StateType repr) -> 
  Reader (State repr) [repr (Parameter repr)] -> 
  Reader (State repr) (Maybe (repr (Value repr)))
getFuncCall n t funcPs = do
  g <- ask
  let getCall Nothing = return Nothing
      getCall (Just m) = do
        ps <- funcPs
        let pvals = getArgs ps
        val <- fApp m n t pvals
        return $ Just val
  getCall $ Map.lookup n (eMap $ codeSpec g)

getInOutCall :: (RenderSym repr) => String -> 
  Reader (State repr) [repr (Value repr)] ->
  Reader (State repr) [repr (Value repr)] -> 
  Reader (State repr) (Maybe (repr (Statement repr)))
getInOutCall n inFunc outFunc = do
  g <- ask
  let getCall Nothing = return Nothing
      getCall (Just m) = do
        ins <- inFunc
        outs <- outFunc
        stmt <- fAppInOut m n ins outs 
        return $ Just stmt
  getCall $ Map.lookup n (eMap $ codeSpec g)

getInputCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getInputCall = getInOutCall "get_input" getInputFormatIns getInputFormatOuts

getDerivedCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getDerivedCall = do
  val <- getFuncCall "derived_values" void getDerivedParams
  return $ fmap valState val

getConstraintCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getConstraintCall = do
  val <- getFuncCall "input_constraints" void getConstraintParams
  return $ fmap valState val

getCalcCall :: (RenderSym repr) => CodeDefinition -> Reader (State repr) 
  (Maybe (repr (Statement repr)))
getCalcCall c = do
  val <- getFuncCall (codeName c) (convType $ codeType c) (getCalcParams c)
  v <- variable (nopfx $ codeName c) (convType $ codeType c)
  l <- maybeLog v
  return $ fmap (multi . (: l) . varDecDef v) val

getOutputCall :: (RenderSym repr) => Reader (State repr) 
  (Maybe (repr (Statement repr)))
getOutputCall = do
  val <- getFuncCall "write_output" void getOutputParams
  return $ fmap valState val

getInputFormatIns :: (RenderSym repr) => Reader (State repr) 
  [repr (Value repr)]
getInputFormatIns = do
  g <- ask
  let getIns :: (RenderSym repr) => Structure -> [repr (Value repr)]
      getIns Unbundled = []
      getIns Bundled = [var "inParams" (obj "InputParameters")]
  return $ var "filename" string : getIns (inStruct g)

getInputFormatOuts :: (RenderSym repr) => Reader (State repr) 
  [repr (Value repr)]
getInputFormatOuts = do
  g <- ask
  let getOuts :: (RenderSym repr) => Structure -> [repr (Value repr)]
      getOuts Unbundled = toValues $ extInputs $ csi $ codeSpec g
      getOuts Bundled = []
  return $ getOuts (inStruct g)
  
toValues :: (RenderSym repr) => [CodeChunk] -> [repr (Value repr)]
toValues = map (\c -> var (codeName c) ((convType . codeType) c))

getDerivedParams :: (RenderSym repr) => 
  Reader (State repr) [repr (Parameter repr)]
getDerivedParams = do
  g <- ask
  let s = csi $ codeSpec g
      dvals = derivedInputs s
      reqdVals = concatMap (flip codevars (sysinfodb s) . codeEquat) dvals
  getParams reqdVals

getConstraintParams :: (RenderSym repr) => 
  Reader (State repr) [repr (Parameter repr)]
getConstraintParams = do 
  g <- ask
  let cm = cMap $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      db = sysinfodb $ csi $ codeSpec g
      varsList = filter (\i -> member (i ^. uid) cm) (inputs $ codeSpec g)
      reqdVals = nub $ varsList ++ concatMap (\v -> constraintvarsandfuncs v db 
        mem) (getConstraints cm varsList)
  getParams reqdVals

getCalcParams :: (RenderSym repr) => CodeDefinition -> 
  Reader (State repr) [repr (Parameter repr)]
getCalcParams c = do
  g <- ask
  getParams $ codevars' (codeEquat c) $ sysinfodb $ csi $ codeSpec g

getOutputParams :: (RenderSym repr) => Reader (State repr) 
  [repr (Parameter repr)]
getOutputParams = do
  g <- ask
  getParams $ outputs $ csi $ codeSpec g

-----

loggedVar :: (RenderSym repr) => repr (Value repr) -> 
  Reader (State repr) (repr (Statement repr))
loggedVar v =
  let l_outfile = "outfile"
      v_outfile = var l_outfile outfile
  in do
    g <- ask
    return $ multi [
      openFileA v_outfile (litString $ logName g),
      printFileStr v_outfile ("var '" ++ valueName v ++ "' assigned to "),
      printFile v_outfile v,
      printFileStrLn v_outfile (" in module " ++ currentModule g),
      closeFile v_outfile ]

-- helpers

nopfx :: String -> String
nopfx s = fromMaybe s (stripPrefix funcPrefix s)

variable :: (RenderSym repr) => String -> repr (StateType repr) -> 
  Reader (State repr) (repr (Value repr))
variable s' t' = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
      doit :: (RenderSym repr) => String -> repr (StateType repr) -> 
        Reader (State repr) (repr (Value repr))
      doit s t | member s mm =
        maybe (error "impossible") (convExpr . codeEquat) (Map.lookup s mm) --extvar "Constants" s
               | s `elem` map codeName (inputs cs) = return $ inputVariable 
                 (inStruct g) s t
               | otherwise                         = return $ var s t
  doit s' t'

inputVariable :: (RenderSym repr) => Structure -> String -> 
  repr (StateType repr) -> repr (Value repr)
inputVariable Unbundled s t = var s t
inputVariable Bundled s t = var "inParams" (obj "InputParameters") $-> var s t
  
fApp :: (RenderSym repr) => String -> String -> repr (StateType repr) -> 
  [repr (Value repr)] -> Reader (State repr) (repr (Value repr))
fApp m s t vl = do
  g <- ask
  return $ if m /= currentModule g then extFuncApp m s t vl else funcApp s t vl

fAppInOut :: (RenderSym repr) => String -> String -> [repr (Value repr)] -> 
  [repr (Value repr)] -> Reader (State repr) (repr (Statement repr))
fAppInOut m n ins outs = do
  g <- ask
  return $ if m /= currentModule g then extInOutCall m n ins outs 
    else inOutCall n ins outs

getParams :: (RenderSym repr) => [CodeChunk] -> Reader (State repr) 
  [repr (Parameter repr)]
getParams cs = do
  g <- ask
  let ins = inputs $ codeSpec g
      consts = map codevar $ constants $ csi $ codeSpec g
      inpParams = filter (`elem` ins) cs
      inPs = getInputParams (inStruct g) inpParams
      conParams = filter (`elem` consts) cs
      conPs = getConstParams conParams
      csSubIns = cs \\ (ins ++ consts)
      ps = map mkParam csSubIns
  return $ inPs ++ conPs ++ ps

mkParam :: (RenderSym repr) => CodeChunk -> repr (Parameter repr)
mkParam p = paramFunc (codeType p) $ var pName pType
  where paramFunc (C.List _) = pointerParam
        paramFunc _ = stateParam
        pName = codeName p
        pType = convType $ codeType p

getInputParams :: (RenderSym repr) => Structure -> [CodeChunk] ->
  [repr (Parameter repr)]
getInputParams _ [] = []
getInputParams Unbundled cs = map mkParam cs
getInputParams Bundled _ = [pointerParam $ var pName pType]
  where pName = "inParams"
        pType = obj "InputParameters"

-- Right now, we always inline constants. In the future, this will be captured by a choice and this function should be updated to read that choice
getConstParams :: [CodeChunk] -> [repr (Parameter repr)]
getConstParams _ = []

getArgs :: (RenderSym repr) => [repr (Parameter repr)] -> [repr (Value repr)]
getArgs = map (\p -> var (parameterName p) (parameterType p))

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
  let v = codevar (symbLookup c (symbolTable $ sysinfodb $ csi $ codeSpec g))
  variable (codeName v) (convType $ codeType v)
convExpr (FCall (C c) x) = do
  g <- ask
  let info = sysinfodb $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      funcCd = codefunc (symbLookup c (symbolTable info))
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
  convExpr $ renderRealInt (lookupC (sysinfodb $ csi $ codeSpec g) c) ri

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
genModDef :: (RenderSym repr) => CS.Mod -> Reader (State repr) 
  (repr (Module repr))
genModDef (CS.Mod n fs) = genModule n (Just $ mapM genFunc fs) Nothing

genFunc :: (RenderSym repr) => Func -> Reader (State repr) (repr (Method repr))
genFunc (FDef (FuncDef n i o s)) = do
  g <- ask
  parms <- getParams i
  stmts <- mapM convStmt s
  vals <- mapM (\x -> variable (codeName x) (convType $ codeType x)) 
    (fstdecl (sysinfodb $ csi $ codeSpec g) s \\ i)
  publicMethod (mState $ convType o) n parms
    (return [block $ map varDec vals ++ stmts])
genFunc (FData (FuncData n ddef)) = genDataFunc n ddef
genFunc (FCD cd) = genCalcFunc cd

convStmt :: (RenderSym repr) => FuncStmt -> Reader (State repr) 
  (repr (Statement repr))
convStmt (FAsg v e) = do
  e' <- convExpr e
  v' <- variable (codeName v) (convType $ codeType v)
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
convStmt (FDec v (C.List t)) = return $ listDec 0 (var (codeName v)
  (listType dynamic_ (convType t)))
convStmt (FDec v t) = do 
  val <- variable (codeName v) (convType t)
  return $ varDec val
convStmt (FProcCall n l) = do
  e' <- convExpr (FCall (asExpr n) l)
  return $ valState e'
convStmt (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ valState $ listAppend a' b'

genDataFunc :: (RenderSym repr) => Name -> DataDesc -> Reader (State repr)
  (repr (Method repr))
genDataFunc nameTitle ddef = do
  parms <- getParams $ getInputs ddef
  publicMethod (mState void) nameTitle (p_filename : parms)  (readData ddef)
  where l_filename = "filename"
        v_filename = var l_filename string
        p_filename = stateParam v_filename

-- this is really ugly!!
readData :: (RenderSym repr) => DataDesc -> Reader (State repr)
  [repr (Block repr)]
readData ddef = do
  inD <- mapM inData ddef
  return [block $ [
    varDec v_infile,
    varDec v_line,
    listDec 0 v_lines,
    listDec 0 v_linetokens,
    openFileR v_infile v_filename ] ++
    concat inD ++ [
    closeFile v_infile ]]
  where inData :: (RenderSym repr) => Data -> Reader (State repr) [repr (Statement repr)]
        inData (Singleton v) = do
            vv <- variable (codeName v) (convType $ codeType v)
            l <- maybeLog vv
            return [multi $ getFileInput v_infile vv : l]
        inData JunkData = return [discardFileLine v_infile]
        inData (Line lp d) = do
          lnI <- lineData Nothing lp
          logs <- getEntryVarLogs lp
          return $ [getFileInputLine v_infile v_line, 
            stringSplit d v_linetokens v_line] ++ lnI ++ logs
        inData (Lines lp ls d) = do
          lnV <- lineData (Just "_temp") lp
          logs <- getEntryVarLogs lp
          let readLines Nothing = [getFileInputAll v_infile v_lines,
                forRange l_i (litInt 0) (listSize v_lines) (litInt 1)
                  (bodyStatements $ stringSplit d v_linetokens (
                  listAccess v_lines v_i) : lnV)]
              readLines (Just numLines) = [forRange l_i (litInt 0) 
                (litInt numLines) (litInt 1)
                (bodyStatements $
                  [getFileInputLine v_infile v_line,
                   stringSplit d v_linetokens v_line
                  ] ++ lnV)]
          return $ readLines ls ++ logs
        ---------------
        lineData :: (RenderSym repr) => Maybe String -> LinePattern -> 
          Reader (State repr) [repr (Statement repr)]
        lineData s (Straight p) = patternData s p (litInt 0)
        lineData s (Repeat p Nothing) = do
          pat <- patternData s p v_j
          return $ clearTemps s p ++ 
            [forRange l_j (litInt 0) (cast int
              (listSize v_linetokens #/ litInt (toInteger $ length p))) 
              (litInt 1) ( bodyStatements pat )] ++ 
            appendTemps s p
        lineData s (Repeat p (Just numPat)) = do
          pat <- patternData s p v_j
          return $ clearTemps s p ++ 
            [forRange l_j (litInt 0) (litInt numPat) (litInt 1) 
              (bodyStatements pat)] ++ 
            appendTemps s p
        ---------------
        clearTemps :: (RenderSym repr) => Maybe String -> [Entry] -> 
          [repr (Statement repr)]
        clearTemps Nothing _ = []
        clearTemps (Just sfx) es = mapMaybe (clearTemp sfx) es
        ---------------
        clearTemp :: (RenderSym repr) => String -> Entry -> 
          Maybe (repr (Statement repr))
        clearTemp sfx (Entry v) = Just $ listDecDef (var (codeName v ++ 
          sfx) (convType $ getListType (codeType v) 1)) []
        clearTemp sfx (ListEntry _ v) = Just $ listDecDef (var 
          (codeName v ++ sfx) (convType $ getListType (codeType v) 1)) []
        clearTemp _ JunkEntry = Nothing
        ---------------
        appendTemps :: (RenderSym repr) => Maybe String -> [Entry] -> 
          [repr (Statement repr)]
        appendTemps Nothing _ = []
        appendTemps (Just sfx) es = mapMaybe (appendTemp sfx) es
        ---------------
        appendTemp :: (RenderSym repr) => String -> Entry -> 
          Maybe (repr (Statement repr))
        appendTemp sfx (Entry v) = Just $ valState $ listAppend 
          (var (codeName v) (convType $ codeType v)) 
          (var (codeName v ++ sfx) (convType $ codeType v))
        appendTemp sfx (ListEntry _ v) = Just $ valState $ listAppend 
          (var (codeName v) (convType $ codeType v))
          (var (codeName v ++ sfx) (convType $ codeType v))
        appendTemp _ JunkEntry = Nothing
        ---------------
        patternData :: (RenderSym repr) => Maybe String -> [Entry] -> 
          repr (Value repr) -> Reader (State repr) [repr (Statement repr)]
        patternData s d patNo = do
          let l = toInteger $ length d
          ent <- zipWithM (entryData s) 
            (map (\z -> (patNo #* litInt l) #+ litInt z) [0..l-1]) d
          return $ concat ent
        ---------------
        entryData :: (RenderSym repr) => Maybe String -> repr (Value repr) -> 
          Entry -> Reader (State repr) [repr (Statement repr)]
        entryData s tokIndex (Entry v) = do
          vv <- variable (codeName v ++ fromMaybe "" s) (convType $ codeType v)
          l <- maybeLog vv
          return [multi $ assign vv (cast (convType $ codeType v)
            (listAccess v_linetokens tokIndex)) : l]
        entryData s tokIndex (ListEntry indx v) = do
          vv <- variable (codeName v ++ fromMaybe "" s) (convType $ codeType v)
          return [
            valState (listAppend vv
            (cast (convType $ getListType (codeType v) (toInteger $ length indx))
            (listAccess v_linetokens tokIndex)))]
        entryData _ _ JunkEntry = return []
        ---------------
        l_line, l_lines, l_linetokens, l_infile, l_filename, l_i, l_j :: Label
        v_line, v_lines, v_linetokens, v_infile, v_filename, v_i, v_j ::
          (RenderSym repr) => (repr (Value repr))
        l_line = "line"
        v_line = var l_line string
        l_lines = "lines"
        v_lines = var l_lines (listType static_ string)
        l_linetokens = "linetokens"
        v_linetokens = var l_linetokens (listType static_ string)
        l_infile = "infile"
        v_infile = var l_infile infile
        l_filename = "filename"
        v_filename = var l_filename string
        l_i = "i"
        v_i = var l_i int
        l_j = "j"
        v_j = var l_j int

getEntryVars :: (RenderSym repr) => LinePattern -> 
  Reader (State repr) [repr (Value repr)]
getEntryVars lp = mapM (\v -> variable (codeName v) (convType $ codeType v))
  (getPatternInputs lp)

getEntryVarLogs :: (RenderSym repr) => LinePattern -> 
  Reader (State repr) [repr (Statement repr)]
getEntryVarLogs lp = do
  vs <- getEntryVars lp
  logs <- mapM maybeLog vs
  return $ concat logs

getListType :: C.CodeType -> Integer -> C.CodeType
getListType _ 0 = error "No index given"
getListType (C.List t) 1 = t
getListType (C.List t) n = getListType t (n-1)
getListType _ _ = error "Not a list type"
