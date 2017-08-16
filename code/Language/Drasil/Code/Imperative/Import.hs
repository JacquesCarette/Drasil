module Language.Drasil.Code.Imperative.Import where

import Language.Drasil.Code.Code as C
import Language.Drasil.Code.Imperative.AST as I hiding ((&=),assign)
import qualified Language.Drasil.Code.Imperative.AST as I (assign)
import Language.Drasil.Code.Imperative.LanguageRenderer (Options(..))
import Language.Drasil.Code.Imperative.Parsers.ConfigParser (pythonLabel, cppLabel, cSharpLabel, javaLabel)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code
import Language.Drasil.Expr as E
import Language.Drasil.Expr.Extract hiding (vars)
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))
import qualified Language.Drasil.CodeSpec as CS (Mod(..))
import Language.Drasil.DataDesc

import Prelude hiding (log, exp, return, const)
import Data.List (intersperse, (\\))
import System.Directory
import Data.Map (member)

data Generator = Generator { 
  generateCode :: IO (),
  
  genModules :: [Module],
  
  codeSpec :: CodeSpec,
  
  genInputMod :: [CodeChunk] -> ConstraintMap -> [Module],
  genInputClass :: [CodeChunk] -> ConstraintMap -> Class,
  genInputFormat :: [CodeChunk] -> Method,
  genInputConstraints :: [CodeChunk] -> ConstraintMap -> Method,
  
  genConstMod :: Module,
  
  genCalcMod :: String -> [CodeDefinition] -> Module,
  genCalcFunc :: CodeDefinition -> Method,
  genCalcBlock :: Expr -> Body,
  genCaseBlock :: [(Expr,Relation)] -> Body,  

  genOutputMod :: [CodeChunk] -> [Module],
  genOutputFormat :: [CodeChunk] -> Method,
  
  genMethodCall :: Scope -> Permanence -> MethodType -> Label -> [Parameter] -> Body -> Method,
  
  logName :: String,
  
  publicMethod :: MethodType -> Label -> [Parameter] -> Body -> Method,
  privateMethod :: MethodType -> Label -> [Parameter] -> Body -> Method,
  
  sfwrCBody :: Expr -> Body,
  physCBody :: Expr -> Body,
  
  assign :: Value -> Value -> Statement,
  variable :: String -> Value
}

generator :: CodeSpec -> Generator -> Generator
generator spec g = 
  let chs = choices spec
      sfwrConstraintFunc = case (onSfwrConstraint chs) of Warning   -> constrWarn
                                                          Exception -> constrExc
      physConstraintFunc = case (onPhysConstraint chs) of Warning   -> constrWarn
                                                          Exception -> constrExc
      inputModFunc =       case (inputStructure chs)   of Loose     -> genInputModNoClass
                                                          AsClass   -> genInputModClass
      assignFunc =         case (logging chs)          of LogVar    -> loggedAssign
                                                          LogAll    -> loggedAssign
                                                          _         -> (\_ -> I.assign)                                                          
  in Generator {
      generateCode = generateCodeD g,
      
      genModules = genModulesD g,
      
      codeSpec = spec,
      
      genInputMod = inputModFunc g,
      genInputClass = genInputClassD g,
      genInputFormat = genInputFormatD g,
      genInputConstraints = genInputConstraintsD g,
      
      genConstMod = genConstModD g,
      
      genCalcMod = genCalcModD g,
      genCalcFunc = genCalcFuncD g,
      genCalcBlock = genCalcBlockD g,
      genCaseBlock = genCaseBlockD g,  
 
      genOutputMod = genOutputModD g,
      genOutputFormat = genOutputFormatD g,
      
      genMethodCall = genMethodCallD g,
      
      logName = logFile $ choices spec,
      
      publicMethod = genMethodCall g Public Static,
      privateMethod = genMethodCall g Private Dynamic,
      
      sfwrCBody = sfwrConstraintFunc g,
      physCBody = physConstraintFunc g,
      
      assign = assignFunc g,
      variable = varFuncD g
    }
    
varFuncD :: Generator -> String -> Value
varFuncD g s 
  | member s (constMap $ codeSpec g) = extvar "Constants" s
  | s `elem` (map codeName $ inputs $ codeSpec g) = (var "inParams")$->(var s)
  | otherwise                        = var s  

generateCodeD :: Generator -> IO () 
generateCodeD g = let s = codeSpec g
                      modules = genModules g 
  in do workingDir <- getCurrentDirectory
        mapM_ (\x -> do 
             createDirectoryIfMissing False (getDir x) 
             setCurrentDirectory (getDir x)
             createCodeFiles $ makeCode 
               (getLabel x)
               (Options Nothing Nothing Nothing (Just "Code")) 
               (toAbsCode (codeName $ program s) modules)
             setCurrentDirectory workingDir) (lang $ choices s)    
  where getLabel Cpp = cppLabel
        getLabel CSharp = cSharpLabel
        getLabel Java = javaLabel
        getLabel Python = pythonLabel
        getDir Cpp = "cpp"
        getDir CSharp = "csharp"
        getDir Java = "java"
        getDir Python = "python"
        
genModulesD :: Generator -> [Module]
genModulesD g = genInputMod g (inputs $ codeSpec g) (cMap $ codeSpec g)
             ++ [genConstMod g]
            -- ++ map (\(FuncMod n d) -> genCalcMod g n d) (fMods $ codeSpec g)
             ++ genOutputMod g (outputs $ codeSpec g)
             ++ map (genModDef g) (mods $ codeSpec g) -- hack


------- INPUT ----------  

genInputModClass :: Generator -> [CodeChunk] -> ConstraintMap -> [Module]
genInputModClass g ins cm = [buildModule "InputParameters" [] [] [] [(genInputClass g ins cm)]]

genInputModNoClass :: Generator -> [CodeChunk] -> ConstraintMap -> [Module]
genInputModNoClass g ins cm = [
    buildModule "InputParameters" [] 
    (map (\x -> VarDecDef (codeName x) (convType $ codeType x) (defaultValue' $ convType $ codeType x)) ins) 
    [genInputFormat g ins, genInputConstraints g ins cm] 
    []
  ]

genInputClassD :: Generator -> [CodeChunk] -> ConstraintMap -> Class
genInputClassD g ins cm = pubClass
  "InputParameters"
  Nothing
  genInputVars
  ( 
    [ constructor 
        "InputParameters" 
        []
        [zipBlockWith (assign g) vars vals],
      genInputFormat g ins,
      genInputConstraints g ins cm
    ]
  )  
  where vars         = map (var . codeName) ins
        vals         = map (defaultValue' . convType . codeType) ins        
        genInputVars = 
          map (\x -> pubMVar 0 (convType $ codeType x) (codeName x)) ins

genInputFormatD :: Generator -> [CodeChunk] -> Method
genInputFormatD g ins = 
  let l_infile = "infile"
      v_infile = var l_infile
      l_filename = "filename"
      p_filename = param l_filename string
      v_filename = var l_filename
  in
    publicMethod g methodTypeVoid "get_inputs" [ p_filename ] [ block $ [
      varDec l_infile infile,
      openFileR v_infile v_filename ] ++
      map (\x -> getFileInput v_infile (convType $ codeType x) (variable g $ codeName x)) ins ++ [
      closeFile v_infile ] ]
          
genInputConstraintsD :: Generator -> [CodeChunk] -> ConstraintMap -> Method
genInputConstraintsD g vars cm = 
  let sfwrCs = concatMap (\x -> sfwrLookup x cm) vars 
      physCs = concatMap (\x -> physLookup x cm) vars
  in
    publicMethod g methodTypeVoid "input_constraints" [] [ block $
      (map (\x -> ifCond [((?!) (convExpr g x), sfwrCBody g x)] noElse) sfwrCs) ++
      (map (\x -> ifCond [((?!) (convExpr g x), physCBody g x)] noElse) physCs) ]      

        
-- need Expr -> String to print constraint
constrWarn :: Generator -> Expr -> Body
constrWarn _ _ = oneLiner $ printStrLn "Warning: constraint violated"

constrExc :: Generator -> Expr -> Body
constrExc _ _ = oneLiner $ throw "InputError"

---- CONST ----

genConstModD :: Generator -> Module
genConstModD g = buildModule "Constants" [] [] [] [genConstClassD g]

genConstClassD :: Generator -> Class
genConstClassD g = pubClass "Constants" Nothing genVars []
  where genVars = map (\x -> pubGVar 0 (convType $ codeType x) (codeName x)) (const $ codeSpec g)
    
------- CALC ----------    
    
genCalcModD :: Generator -> String -> [CodeDefinition] -> Module
genCalcModD g n defs = buildModule n [] [] (map (genCalcFunc g) (filter (validExpr . codeEquat) defs)) []   
        
genCalcFuncD :: Generator -> CodeDefinition -> Method
genCalcFuncD g cdef = 
  publicMethod g 
    (methodType $ convType (codeType cdef)) 
    (codeName cdef)
    (getParams g (codevars $ codeEquat cdef)) 
    (genCalcBlock g $ codeEquat cdef)

genCalcBlockD :: Generator -> Expr -> Body
genCalcBlockD g e
  | containsCase e   = genCaseBlock g $ getCases e
  | otherwise        = oneLiner $ return $ convExpr g e

genCaseBlockD :: Generator -> [(Expr,Relation)] -> Body
genCaseBlockD g cs = oneLiner $ ifCond (genIf cs) noElse
  where genIf :: [(Expr,Relation)] -> [(Value,Body)]
        genIf = map (\(e,r) -> (convExpr g r, genCalcBlock g e))

----- OUTPUT -------
          
genOutputModD :: Generator -> [CodeChunk] -> [Module]
genOutputModD g outs = [buildModule "OutputFormat" [] [] [genOutputFormat g outs] []]  
    
genOutputFormatD :: Generator -> [CodeChunk] -> Method
genOutputFormatD g outs = 
  let l_outfile = "outfile"
      v_outfile = var l_outfile
      l_filename = "filename"
      p_filename = param l_filename string
      v_filename = var l_filename
  in
    publicMethod g methodTypeVoid "write_output" (p_filename:getParams g outs) [ block $ [
      varDec l_outfile outfile,
      openFileW v_outfile v_filename ] ++
      concatMap 
        (\x -> [ printFileStr v_outfile ((codeName x) ++ " = "), 
                 printFileLn v_outfile (convType $ codeType x) (variable g $ codeName x)
               ] ) outs ++ [
      closeFile v_outfile ] ]         
    
-----

genMethodCallD :: Generator -> Scope -> Permanence -> MethodType -> Label -> [Parameter] 
                  -> Body -> Method
genMethodCallD g s pr t n p b = Method n s pr t p (commBody doComments (loggedBody doLog))
  where
    ch = choices $ codeSpec g
    doLog = logging ch
    doComments = comments ch
    loggedBody LogFunc = loggedMethod g n p b
    loggedBody LogAll  = loggedMethod g n p b
    loggedBody _       = b
    commBody CommentFunc = commMethod g n p
    commBody _           = id

commMethod :: Generator -> Label -> [Parameter] -> Body -> Body
commMethod g n p b = (
  block [
    comment $ "function '" ++ n ++ "': " ++ (funcTerm n (fMap $ codeSpec g)),
    multi $ map 
      (\x -> comment $ "parameter '" ++ (paramName x) ++ "': " ++ (varTerm (paramName x) (vMap $ codeSpec g))) p    
  ]) : b
    
loggedMethod :: Generator -> Label -> [Parameter] -> Body -> Body
loggedMethod g n p b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile 
  in ( block [
    varDec l_outfile outfile,
    openFileW v_outfile (litString $ logName g),  
    printFileStr v_outfile ("function " ++ n ++ "("),
    printParams p v_outfile,
    printFileStrLn v_outfile ") called",
    closeFile v_outfile ] )
    : b
  where
    printParams ps v_outfile = multi $ 
      intersperse (printFileStr v_outfile ", ") $
      map (\x -> printFile v_outfile (paramType x) (paramVal x)) ps

----

loggedAssign :: Generator -> Value -> Value -> Statement
loggedAssign g a b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile 
  in multi [
    I.assign a b,
    varDec l_outfile outfile,
    openFileW v_outfile (litString $ logName g),  
    printFileStr v_outfile ("var '" ++ (valName a) ++ "' assigned to "),
    printFileLn v_outfile (convType $ varType (valName b) (vMap $ codeSpec g)) b,
    closeFile v_outfile ]
      
-- helpers
    
getParams :: Generator -> [CodeChunk] -> [Parameter]
getParams g cs = 
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      ps = map (\y -> param (codeName y) (convType $ codeType y)) 
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g)) csSubIns)     
  in  if length csSubIns < length cs
      then (param "inParams" (obj "InputParameters")):ps  -- todo:  make general
      else ps
          
paramType :: Parameter -> StateType
paramType (StateParam _ s) = s
paramType (FuncParam _ _ _) = error "Function param not implemented"

paramVal :: Parameter -> Value
paramVal (StateParam l _) = var l
paramVal (FuncParam _ _ _) = error "Function param not implemented"     

paramName :: Parameter -> String
paramName (StateParam l _) = l
paramName (FuncParam _ _ _) = error "Function param not implemented"   

valName :: Value -> String
valName (Var _ n) = n
valName (ObjVar o v) = valName o ++ "." ++ valName v
valName _ = error "Value has no name"
          
convType :: C.CodeType -> I.StateType
convType C.Boolean = bool
convType C.Integer = int
convType C.Float = float
convType C.Char = char
convType C.String = string
convType (C.List t) = listT $ convType t
convType (C.Object n) = obj n
convType _ = error "No type conversion"

-- Some Expr can't be converted to code yet...
-- rather than stop execution with failure,
-- just check ahead of time and don't try to convert for now
validExpr :: Expr -> Bool
validExpr (V _)        = True
validExpr (Dbl _)      = True
validExpr (Int _)      = True
validExpr (Bln _)      = True
validExpr (a :/ b)     = (validExpr a) && (validExpr b)
validExpr (a :* b)     = (validExpr a) && (validExpr b)
validExpr (a :+ b)     = (validExpr a) && (validExpr b)
validExpr (a :^ b)     = (validExpr a) && (validExpr b)
validExpr (a :- b)     = (validExpr a) && (validExpr b)
validExpr (a :. b)     = (validExpr a) && (validExpr b)
validExpr (a :&& b)    = (validExpr a) && (validExpr b)
validExpr (a :|| b)    = (validExpr a) && (validExpr b)
validExpr (Deriv _ _ _) = False
validExpr (E.Not e)      = validExpr e
validExpr (Neg e)      = validExpr e
validExpr (C _)        = True
validExpr (FCall (C _) x)  = foldl (&&) True (map validExpr x)
validExpr (FCall _ _)  = False
validExpr (a := b)     = (validExpr a) && (validExpr b)
validExpr (a :!= b)    = (validExpr a) && (validExpr b)
validExpr (a :> b)     = (validExpr a) && (validExpr b)
validExpr (a :< b)     = (validExpr a) && (validExpr b)
validExpr (a :<= b)    = (validExpr a) && (validExpr b)
validExpr (a :>= b)    = (validExpr a) && (validExpr b)
validExpr (UnaryOp u)  = validunop u
validExpr (Grouping e) = validExpr e
validExpr (BinaryOp _) = False
validExpr (Case c)     = foldl (&&) True (map (\(e, r) -> validExpr e && validExpr r) c)
validExpr _            = False

validunop :: UFunc -> Bool
validunop (E.Log e)          = validExpr e
validunop (E.Abs e)          = validExpr e
validunop (E.Exp e)          = validExpr e
validunop (E.Sin e)          = validExpr e
validunop (E.Cos e)          = validExpr e
validunop (E.Tan e)          = validExpr e
validunop (E.Csc e)          = validExpr e
validunop (E.Sec e)          = validExpr e
validunop (E.Cot e)          = validExpr e
validunop _                  = False

convExpr :: Generator -> Expr -> Value
convExpr _ (V v)        = litString v  -- V constructor should be removed
convExpr _ (Dbl d)      = litFloat d
convExpr _ (Int i)      = litInt i
convExpr _ (Bln b)      = litBool b
convExpr g (a :/ b)     = (convExpr g a) #/ (convExpr g b)
convExpr g (a :* b)     = (convExpr g a) #* (convExpr g b)
convExpr g (a :+ b)     = (convExpr g a) #+ (convExpr g b)
convExpr g (a :^ b)     = (convExpr g a) #^ (convExpr g b)
convExpr g (a :- b)     = (convExpr g a) #- (convExpr g b)
convExpr g (a :. b)     = (convExpr g a) #* (convExpr g b)
convExpr g (a :&& b)    = (convExpr g a) ?&& (convExpr g b)
convExpr g (a :|| b)    = (convExpr g a) ?|| (convExpr g b)
convExpr _ (Deriv _ _ _) = error "not implemented"
convExpr g (E.Not e)      = (?!) (convExpr g e)
convExpr g (Neg e)      = (#~) (convExpr g e)
convExpr g (C c)        = variable g (codeName (SFCN c))
convExpr g (Index a i)  = (convExpr g a)$.(listAccess $ convExpr g i)
convExpr g (Len a)      = (convExpr g a)$.listSize
convExpr g (Append a v) = (convExpr g a)$.(listAppend $ convExpr g v)
convExpr g (FCall (C c) x)  = funcApp' (codeName (SFCN c)) (map (convExpr g) x)
convExpr _ (FCall _ _)  = error "not implemented"
convExpr g (a := b)     = (convExpr g a) ?== (convExpr g b)
convExpr g (a :!= b)    = (convExpr g a) ?!= (convExpr g b)
convExpr g (a :> b)     = (convExpr g a) ?> (convExpr g b)
convExpr g (a :< b)     = (convExpr g a) ?< (convExpr g b)
convExpr g (a :<= b)    = (convExpr g a) ?<= (convExpr g b)
convExpr g (a :>= b)    = (convExpr g a) ?>= (convExpr g b)
convExpr g (UnaryOp u)  = unop g u
convExpr g (Grouping e) = convExpr g e
convExpr _ (BinaryOp _) = error "not implemented"
convExpr _ (Case _)     = error "Case should be dealt with separately"
convExpr _ _           = error "not implemented"

unop :: Generator -> UFunc -> Value
unop g (E.Log e)          = I.log (convExpr g e)
unop g (E.Abs e)          = (#|) (convExpr g e)
unop g (E.Exp e)          = I.exp (convExpr g e)
unop g (E.Sin e)          = I.sin (convExpr g e)
unop g (E.Cos e)          = I.cos (convExpr g e)
unop g (E.Tan e)          = I.tan (convExpr g e)
unop g (E.Csc e)          = I.csc (convExpr g e)
unop g (E.Sec e)          = I.sec (convExpr g e)
unop g (E.Cot e)          = I.cot (convExpr g e)
unop _ _                  = error "not implemented"


containsCase :: Expr -> Bool
containsCase (Case _) = True
containsCase (a :/ b)     = (containsCase a) || (containsCase b)
containsCase (a :* b)     = (containsCase a) || (containsCase b)
containsCase (a :+ b)     = (containsCase a) || (containsCase b)
containsCase (a :^ b)     = (containsCase a) || (containsCase b)
containsCase (a :- b)     = (containsCase a) || (containsCase b)
containsCase (a :. b)     = (containsCase a) || (containsCase b)
containsCase (a :&& b)    = (containsCase a) || (containsCase b)
containsCase (a :|| b)    = (containsCase a) || (containsCase b)
containsCase (Deriv _ _ _) = error "not implemented"
containsCase (E.Not e)      = containsCase e
containsCase (Neg e)      = containsCase e
containsCase (a := b)     = (containsCase a) || (containsCase b)
containsCase (a :!= b)    = (containsCase a) || (containsCase b)
containsCase (a :> b)     = (containsCase a) || (containsCase b)
containsCase (a :< b)     = (containsCase a) || (containsCase b)
containsCase (a :<= b)    = (containsCase a) || (containsCase b)
containsCase (a :>= b)    = (containsCase a) || (containsCase b)
containsCase (UnaryOp u)  = unopcase u
containsCase (Grouping e) = containsCase e
containsCase (BinaryOp _) = error "not implemented"
containsCase _            = False

unopcase :: UFunc -> Bool
unopcase (E.Log e)          = containsCase e
unopcase (E.Abs e)          = containsCase e
unopcase (E.Exp e)          = containsCase e
unopcase (E.Sin e)          = containsCase e
unopcase (E.Cos e)          = containsCase e
unopcase (E.Tan e)          = containsCase e
unopcase (E.Sec e)          = containsCase e
unopcase (E.Csc e)          = containsCase e
unopcase (E.Cot e)          = containsCase e
unopcase _                  = error "not implemented"

getCases :: Expr -> [(Expr, Relation)]
getCases (Case a)     = a
getCases e            = getCases (compactCase e)

compactCase :: Expr -> Expr
compactCase (a :/ b)     = compactCaseBinary (:/) a b
compactCase (a :* b)     = compactCaseBinary (:*) a b
compactCase (a :+ b)     = compactCaseBinary (:+) a b
compactCase (a :^ b)     = compactCaseBinary (:^) a b
compactCase (a :- b)     = compactCaseBinary (:-) a b
compactCase (a :. b)     = compactCaseBinary (:.) a b
compactCase (a :&& b)    = compactCaseBinary (:&&) a b
compactCase (a :|| b)    = compactCaseBinary (:||) a b
compactCase (Deriv _ _ _) = error "not implemented"
compactCase (E.Not e)    = compactCaseUnary E.Not e
compactCase (Neg e)      = compactCaseUnary Neg e
compactCase (a := b)     = compactCaseBinary (:=) a b
compactCase (a :!= b)    = compactCaseBinary (:!=) a b
compactCase (a :> b)     = compactCaseBinary (:>) a b
compactCase (a :< b)     = compactCaseBinary (:<) a b
compactCase (a :<= b)    = compactCaseBinary (:<=) a b
compactCase (a :>= b)    = compactCaseBinary (:>=) a b
compactCase (UnaryOp u)  = unopcomcase u
compactCase (Grouping e) = compactCaseUnary Grouping e
compactCase (BinaryOp _) = error "not implemented"
compactCase e            = e

unopcomcase :: UFunc -> Expr
unopcomcase (E.Log e)   = compactCaseUnary (UnaryOp . E.Log) e
unopcomcase (E.Abs e)   = compactCaseUnary (UnaryOp . E.Abs) e
unopcomcase (E.Exp e)   = compactCaseUnary (UnaryOp . E.Exp) e
unopcomcase (E.Sin e)   = compactCaseUnary (UnaryOp . E.Sin) e
unopcomcase (E.Cos e)   = compactCaseUnary (UnaryOp . E.Cos) e
unopcomcase (E.Tan e)   = compactCaseUnary (UnaryOp . E.Tan) e
unopcomcase (E.Sec e)   = compactCaseUnary (UnaryOp . E.Sec) e
unopcomcase (E.Csc e)   = compactCaseUnary (UnaryOp . E.Csc) e
unopcomcase (E.Cot e)   = compactCaseUnary (UnaryOp . E.Cot) e
unopcomcase _           = error "not implemented"

compactCaseBinary :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
compactCaseBinary op (Case c) b = Case (map (\(e, r) -> (e `op` b, r)) c)
compactCaseBinary op a (Case c) = Case (map (\(e, r) -> (a `op` e, r)) c)
compactCaseBinary op a b        = (compactCase a) `op` (compactCase b)

compactCaseUnary :: (Expr -> Expr) -> Expr -> Expr
compactCaseUnary op (Case c) = Case (map (\(e, r) -> (op e, r)) c)
compactCaseUnary op a        = op (compactCase a)

-- medium hacks --
genModDef :: Generator -> DMod -> Module
genModDef g (DMod libs (CS.Mod n fs)) = buildModule n libs [] (map (genFunc g) fs) []

genFunc :: Generator -> Func -> Method
genFunc g (FDef (FuncDef n i o s)) = publicMethod g (methodType $ convType o) n (getParams g i) [ block (map (convStmt g) s) ]
genFunc g (FData (FuncData n dd)) = genDataFunc g n dd
genFunc g (FCD cd) = genCalcFunc g cd

convStmt :: Generator -> FuncStmt -> Statement
convStmt g (FAsg v e) = assign g (var $ codeName v) (convExpr g e)
convStmt g (FFor v e st) = for (varDecDef (codeName v) int (litInt 0)) (convExpr g e) ((&++) (var (codeName v)))
  [ block (map (convStmt g) st) ]
convStmt g (FWhile e st) = while (convExpr g e) [ block (map (convStmt g) st) ]
convStmt g (FCond e tSt []) = ifCond [(convExpr g e, [ block (map (convStmt g) tSt) ])] noElse
convStmt g (FCond e tSt eSt) = ifCond [(convExpr g e, [ block (map (convStmt g) tSt) ])] [ block (map (convStmt g) eSt) ]  
convStmt g (FRet e) = return $ convExpr g e
convStmt _ (FThrow s) = throw s
convStmt g (FTry t c) = tryCatch [ block (map (convStmt g) t) ] [ block (map (convStmt g) c) ]
convStmt _ (FContinue) = continue
convStmt g (FVal e) = valStmt $ convExpr g e
convStmt _ (FDec v (C.List t)) = listDec' (codeName v) (convType t) 0
convStmt _ (FDec v t) = varDec (codeName v) (convType t)

-- this is really ugly!!    
genDataFunc :: Generator -> Name -> DataDesc -> Method
genDataFunc g name dd = 
    publicMethod g methodTypeVoid name (p_filename : (getParams g $ getInputs dd)) $
      body $ [
      varDec l_infile infile,
      varDec l_line string,
      listDec' l_lines string 0,
      listDec' l_linetokens string 0,
      openFileR v_infile v_filename ] ++ 
      (concatMap inData dd) ++ [
      closeFile v_infile ]
  where inData :: Data -> [Statement]
        inData (Singleton v) = [getFileInput v_infile (convType $ codeType v) (variable g $ codeName v)]
        inData JunkData = [discardFileLine v_infile]
        inData (Line lp d) = 
          [ getFileInputLine v_infile v_line,
            stringSplit v_linetokens v_line d
          ] ++ lineData lp (litInt 0)
        inData (Lines lp Nothing d) = 
          [ getFileInputAll v_infile v_lines,
            for (varDecDef l_i int (litInt 0)) (v_i ?< v_lines$.listSize) ((&++) v_i)
              ( body 
                ( [ stringSplit v_linetokens (v_lines$.(listAccess v_i)) d
                  ] ++ lineData lp v_i
                )
              )
          ] 
        inData (Lines lp (Just numLines) d) = 
          [ for (varDecDef l_i int (litInt 0)) (v_i ?< (litInt numLines)) ((&++) v_i)
              ( body 
                ( [ getFileInputLine v_infile v_line,
                    stringSplit v_linetokens v_line d
                  ] ++ lineData lp v_i
                )
              )
          ] 
        ---------------
        lineData :: LinePattern -> Value -> [Statement]
        lineData (Straight p) lineNo = patternData p lineNo (litInt 0)        
        lineData (Repeat p Nothing) lineNo = 
          [ for (varDecDef l_j int (litInt 0)) (v_j ?< (v_linetokens$.listSize #/ (litInt $ toInteger $ length p))$.(cast int float)) ((&++) v_j)
              ( body (patternData p lineNo v_j) )
          ]
        lineData (Repeat p (Just numPat)) lineNo = 
          [ for (varDecDef l_j int (litInt 0)) (v_j ?< (litInt numPat)) ((&++) v_j)
              ( body (patternData p lineNo v_j) )
          ]   
        ---------------
        patternData :: [Entry] -> Value -> Value -> [Statement]
        patternData d lineNo patNo = 
          let l = toInteger $ length d
          in  concatMap (\(x,y) -> entryData x lineNo patNo y) $ zip (map (\z -> (patNo #* (litInt l)) #+ (litInt z)) [0..l-1]) d
        ---------------
        entryData :: Value -> Value -> Value -> Entry -> [Statement]
        entryData tokIndex _ _ (Entry v) = [assign g (variable g $ codeName v) $
          (v_linetokens$.(listAccess tokIndex))$.(cast (convType $ codeType v) string)]
        entryData tokIndex lineNo patNo (ListEntry indx v) =
          checkIndex indx lineNo patNo (variable g $ codeName v) (codeType v) ++
            [ assign g (indexData indx lineNo patNo (variable g $ codeName v)) $
              (v_linetokens$.(listAccess tokIndex))$.(cast (listType (codeType v) (toInteger $ length indx)) string)
            ]
        entryData _ _ _ JunkEntry = []
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
          [ while (v$.listSize ?<= (litInt i)) ( body [ valStmt $ v$.(listExtend $ listType' s n) ] ) ]
          ++ checkIndex' is (n-1) l p (v$.(listAccess $ litInt i)) s
        checkIndex' ((WithLine):is) n l p v s = 
          [ while (v$.listSize ?<= l) ( body [ valStmt $ v$.(listExtend $ listType' s n ) ] ) ]
          ++ checkIndex' is (n-1) l p (v$.(listAccess l)) s
        checkIndex' ((WithPattern):is) n l p v s =
          [ while (v$.listSize ?<= p) ( body [ valStmt $ v$.(listExtend $ listType' s n ) ] ) ]
          ++ checkIndex' is (n-1) l p (v$.(listAccess p)) s
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
