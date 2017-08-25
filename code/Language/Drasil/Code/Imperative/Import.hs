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
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe)
import Language.Drasil.ChunkDB (SymbolMap, symbLookup)


data Generator = Generator { 
  
  generateCode :: Generator -> IO (),
  
  genModules :: Generator -> [Module],
  
  codeSpec :: CodeSpec,
  
  genInputMod :: Generator -> [Module],
  genInputClass :: Generator -> Class,
  genInputFormat :: Generator -> Method,
  genInputConstraints :: Generator -> Method,
  
  genConstMod :: Generator -> Module,
  
  genCalcMod :: Generator -> String -> [CodeDefinition] -> Module,
  genCalcFunc :: Generator -> CodeDefinition -> Method,
  --genCalcBlock :: Expr -> Body,
  --genCaseBlock :: [(Expr,Relation)] -> Body,  

  genOutputMod :: Generator -> [CodeChunk] -> [Module],
  genOutputFormat :: Generator -> [CodeChunk] -> Method,
   
  logName :: String,
  
  publicMethod :: Generator -> MethodType -> Label -> [Parameter] -> Body -> Method,
  privateMethod :: Generator -> MethodType -> Label -> [Parameter] -> Body -> Method,
  
  sfwrCBody :: Generator -> Expr -> Body,
  physCBody :: Generator -> Expr -> Body,
  
  assign :: Generator -> Value -> Value -> Statement,
  variable :: Generator -> String -> Value,
  fApp :: Generator -> String -> ([Value] -> Value),
  
  currentModule :: String
}

generator :: CodeSpec -> SymbolMap -> Generator
generator spec sm = 
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
      generateCode = generateCodeD,
      
      genModules = flip genModulesD sm,
      
      codeSpec = spec,
      
      genInputMod = flip inputModFunc sm,
      genInputClass = genInputClassD,
      genInputFormat = genInputFormatD,
      genInputConstraints = flip genInputConstraintsD sm,
      
      genConstMod = flip genConstModD sm,
      
      genCalcMod = genCalcModD,
      genCalcFunc = \x y -> genCalcFuncD x y sm,
      --genCalcBlock = genCalcBlockD g,
      --genCaseBlock = genCaseBlockD g,  
 
      genOutputMod = genOutputModD,
      genOutputFormat = genOutputFormatD,
      
      logName = logFile $ choices spec,
      
      publicMethod = genMethodCallD Public Static,
      privateMethod = genMethodCallD Private Dynamic,
      
      sfwrCBody = sfwrConstraintFunc,
      physCBody = physConstraintFunc,
      
      assign = assignFunc,
      variable = \x y -> varFuncD x y sm,
      fApp = fAppFuncD,
      
      currentModule = ""
    }
    
varFuncD :: Generator -> String -> SymbolMap -> Value
varFuncD g s sm
  | member s (constMap $ codeSpec g) = 
      maybe (error "impossible") (flip (convExpr g) sm . codeEquat) (Map.lookup s (constMap $ codeSpec g)) --extvar "Constants" s
  | s `elem` (map codeName $ inputs $ codeSpec g) = (var "inParams")$->(var s)
  | otherwise                        = var s  
  
fAppFuncD :: Generator -> String -> ([Value] -> Value)
fAppFuncD g s 
  | member s (eMap $ codeSpec g) =
      maybe (error "impossible") 
        (\x -> if x /= currentModule g then funcApp x s else funcApp' s) 
        (Map.lookup s (eMap $ codeSpec g))
  | otherwise = funcApp' s

generateCodeD :: Generator -> IO () 
generateCodeD g = let s = codeSpec g
                      modules = genModules g g
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
        
genModulesD :: Generator -> SymbolMap -> [Module]
genModulesD g sm = genMain g sm : genInputMod g g
            -- ++ [genConstMod g]    inlining for now
            -- ++ map (\(FuncMod n d) -> genCalcMod g n d) (fMods $ codeSpec g)
             ++ genOutputMod g g (outputs $ codeSpec g)
             ++ map (flip (genModDef g) sm) (mods $ codeSpec g) -- hack


------- INPUT ----------  

genInputModClass :: Generator -> SymbolMap -> [Module]
genInputModClass g sm = 
  [ genModule g "InputParameters" Nothing (Just $ \x -> [genInputClass g x]),
    genModule g "DerivedValues" (Just $ \x -> [genInputDerived x sm]) Nothing,
    genModule g "InputConstraints" (Just $ \x -> [genInputConstraints g x]) Nothing
  ]

genInputModNoClass :: Generator -> SymbolMap -> [Module]
genInputModNoClass g sm =
  let ins = inputs $ codeSpec g
  in  [ buildModule "InputParameters" [] 
          (map (\x -> VarDecDef (codeName x) (convType $ codeType x) (defaultValue' $ convType $ codeType x)) ins) 
          [{-genInputFormat g ins,-}genInputDerived g sm, genInputConstraints g g] 
          []
      ]

genInputClassD :: Generator -> Class
genInputClassD g =
  pubClass
    "InputParameters"
    Nothing
    inputVars
    ( 
      [ constructor 
          "InputParameters" 
          []
          [zipBlockWith (assign g g) vars vals]--,
          -- genInputFormat g ins,
          -- genInputConstraints g ins cm
      ]
    )  
  where ins          = inputs $ codeSpec g
        --cm           = cMap $ codeSpec g
        inputVars    = 
          map (\x -> pubMVar 0 (convType $ codeType x) (codeName x)) ins
        vars         = map (svToVar) inputVars
        vals         = map (defaultValue' . convType . codeType) ins        
        

genInputFormatD :: Generator -> Method
genInputFormatD g = 
  let ins = extInputs $ codeSpec g
      l_infile = "infile"
      v_infile = var l_infile
      l_filename = "filename"
      p_filename = param l_filename string
      v_filename = var l_filename
  in
    publicMethod g g methodTypeVoid "get_inputs" [ p_filename ] [ block $ [
      varDec l_infile infile,
      openFileR v_infile v_filename ] ++
      map (\x -> getFileInput v_infile (convType $ codeType x) (variable g g $ codeName x)) ins ++ [
      closeFile v_infile ] ]
          
genInputConstraintsD :: Generator -> SymbolMap -> Method
genInputConstraintsD g sm = 
  let vars   = inputs $ codeSpec g
      cm     = cMap $ codeSpec g
      sfwrCs = concatMap (\x -> sfwrLookup x cm) vars 
      physCs = concatMap (\x -> physLookup x cm) vars
  in
    publicMethod g g methodTypeVoid "input_constraints" (getParams g $ vars)
      [ block $
        (map (\x -> ifCond [((?!) (convExpr g x sm), sfwrCBody g g x)] noElse) sfwrCs) ++
        (map (\x -> ifCond [((?!) (convExpr g x sm), physCBody g g x)] noElse) physCs) 
      ]      

genInputDerived :: Generator -> SymbolMap -> Method
genInputDerived g sm = 
  let dvals = derivedInputs $ codeSpec g
  in  publicMethod g g methodTypeVoid "derived_values" (getParams g $ map codevar dvals) 
        (concatMap (\x -> genCalcBlock g CalcAssign (codeName x) (codeEquat x) sm) dvals)
      
-- need Expr -> String to print constraint
constrWarn :: Generator -> Expr -> Body
constrWarn _ _ = oneLiner $ printStrLn "Warning: constraint violated"

constrExc :: Generator -> Expr -> Body
constrExc _ _ = oneLiner $ throw "InputError"

---- CONST ----

genConstModD :: Generator -> SymbolMap -> Module
genConstModD g sm = buildModule "Constants" [] 
  (map (\x -> VarDecDef (codeName x) (convType $ codeType x) (flip (convExpr g) sm $ codeEquat x)) (const $ codeSpec g))
  [] [{- genConstClassD g -}]

genConstClassD :: Generator -> Class
genConstClassD g = pubClass "Constants" Nothing genVars []
  where genVars = map (\x -> pubGVar 0 (convType $ codeType x) (codeName x)) (const $ codeSpec g)
    
------- CALC ----------    
    
genCalcModD :: Generator -> String -> [CodeDefinition] -> Module
genCalcModD g n defs = buildModule n [] [] (map (genCalcFunc g g) (filter (validExpr . codeEquat) defs)) []   
        
genCalcFuncD :: Generator -> CodeDefinition -> SymbolMap -> Method
genCalcFuncD g cdef sm = 
  publicMethod g g
    (methodType $ convType (codeType cdef)) 
    (codeName cdef)
    (getParams g (flip codevars' sm $ codeEquat cdef)) 
    (genCalcBlock g CalcReturn (codeName cdef) (codeEquat cdef) sm)

data CalcType = CalcAssign | CalcReturn deriving Eq

genCalcBlock :: Generator -> CalcType -> String -> Expr -> SymbolMap -> Body
genCalcBlock g t v e sm
  | containsCase e   = genCaseBlock g t v (getCases e) sm
  | t == CalcAssign  = oneLiner $ assign g g (variable g g v) (convExpr g e sm)
  | otherwise        = oneLiner $ return $ convExpr g e sm

genCaseBlock :: Generator -> CalcType -> String -> [(Expr,Relation)] -> SymbolMap -> Body
genCaseBlock g t v cs sm = oneLiner $ ifCond (genIf cs) noElse
  where genIf :: [(Expr,Relation)] -> [(Value,Body)]
        genIf = map (\(e,r) -> (convExpr g r sm, genCalcBlock g t v e sm))

----- OUTPUT -------
          
genOutputModD :: Generator -> [CodeChunk] -> [Module]
genOutputModD g outs = [genModule g "OutputFormat" (Just $ \x -> [genOutputFormat g x outs]) Nothing]  
    
genOutputFormatD :: Generator -> [CodeChunk] -> Method
genOutputFormatD g outs = 
  let l_outfile = "outfile"
      v_outfile = var l_outfile
  in
    publicMethod g g methodTypeVoid "write_output" (getParams g outs) [ block $ [
      varDec l_outfile outfile,
      openFileW v_outfile (litString "output.txt") ] ++
      concatMap 
        (\x -> [ printFileStr v_outfile ((codeName x) ++ " = "), 
                 printFileLn v_outfile (convType $ codeType x) (variable g g $ codeName x)
               ] ) outs ++ [
      closeFile v_outfile ] ]         
    
-----

genMethodCallD :: Scope -> Permanence -> Generator -> MethodType -> Label -> [Parameter] 
                  -> Body -> Method
genMethodCallD s pr g t n p b = Method n s pr t p (commBody doComments (loggedBody doLog))
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

---- MAIN ---

genModule :: Generator 
               -> Name 
               -> Maybe (Generator -> [FunctionDecl])
               -> Maybe (Generator -> [Class])
               -> Module
genModule g' n maybeMs maybeCs = 
  let g  = g' { currentModule = n }
      ls = maybe [] id (Map.lookup n (dMap $ codeSpec g))
      ms = maybe [] (\x -> x g) maybeMs
      cs = maybe [] (\x -> x g) maybeCs  
  in  buildModule n ls [] ms cs

  
genMain :: Generator -> SymbolMap -> Module
genMain g sm = genModule g "Control" (Just $ \x -> [genMainFunc x sm]) Nothing

genMainFunc :: Generator -> SymbolMap -> FunctionDecl
genMainFunc g sm = 
  let l_filename = "inputfile"
      v_filename = var l_filename
      l_params = "inParams"
      v_params = var l_params
  in
    mainMethod $ body $ [
      varDecDef l_filename string $ arg 0 ,
      objDecNewVoid l_params "InputParameters" (obj "InputParameters") ,
      valStmt $ fApp g g "get_input" [v_filename, v_params] ,     
      valStmt $ fApp g g "derived_values" [v_params] ,      
      valStmt $ fApp g g "input_constraints" [v_params]
    ] ++ map (\x -> varDecDef (codeName x) (convType $ codeType x) 
                    (fApp g g (codeName x) (getArgs g $ flip codevars' sm $ codeEquat x)))
          (execOrder $ codeSpec g)
    ++ [
      valStmt $ fApp g g "write_output" $ getArgs g $ outputs $ codeSpec g
    ]


-----

loggedAssign :: Generator -> Value -> Value -> Statement
loggedAssign g a b =
  let l_outfile = "outfile"
      v_outfile = var l_outfile 
  in multi [
    I.assign a b,
    varDec l_outfile outfile,
    openFileW v_outfile (litString $ logName g),  
    printFileStr v_outfile ("var '" ++ (valName a) ++ "' assigned to "),
    printFile v_outfile (convType $ varType (valName b) (vMap $ codeSpec g)) b,
    printFileStrLn v_outfile (" in module " ++ currentModule g),
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
      
getArgs :: Generator -> [CodeChunk] -> [Value]
getArgs g cs = 
  let ins = inputs $ codeSpec g
      csSubIns = cs \\ ins
      args = map (var . codeName) 
            (filter (\x -> not $ member (codeName x) (constMap $ codeSpec g)) csSubIns)     
  in  if length csSubIns < length cs
      then (var "inParams"):args  -- todo:  make general
      else args
      
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
valName (Lit (LitBool b)) = show b
valName (Lit (LitInt i)) = show i
valName (Lit (LitFloat f)) = show f
valName (Lit (LitChar c)) = [c]
valName (Lit (LitStr s)) = s             
valName (Var _ n) = n
valName (ObjVar o v) = valName o ++ "." ++ valName v
valName (ObjAccess o (ListAccess v)) = valName o ++ "[" ++ valName v ++ "]"
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
validunop (E.Sqrt e)         = validExpr e
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

convExpr :: Generator -> Expr -> SymbolMap -> Value
convExpr _ (V v)        _ = litString v  -- V constructor should be removed
convExpr _ (Dbl d)      _ = litFloat d
convExpr _ (Int i)      _ = litInt i
convExpr _ (Bln b)      _ = litBool b
convExpr g (a :/ b)     sm = (convExpr g a sm) #/ (convExpr g b sm)
convExpr g (a :* b)     sm = (convExpr g a sm) #* (convExpr g b sm)
convExpr g (a :+ b)     sm = (convExpr g a sm) #+ (convExpr g b sm)
convExpr g (a :^ b)     sm = (convExpr g a sm) #^ (convExpr g b sm)
convExpr g (0 :- b)     sm = (convExpr g (Neg b) sm)
convExpr g (a :- b)     sm = (convExpr g a sm) #- (convExpr g b sm)
convExpr g (a :. b)     sm = (convExpr g a sm) #* (convExpr g b sm)
convExpr g (a :&& b)    sm = (convExpr g a sm) ?&& (convExpr g b sm)
convExpr g (a :|| b)    sm = (convExpr g a sm) ?|| (convExpr g b sm)
convExpr _ (Deriv _ _ _) _ = litString "**convExpr :: Deriv unimplemented**"
convExpr g (E.Not e)    sm = (?!) (convExpr g e sm)
convExpr g (Neg e)      sm = (#~) (convExpr g e sm)
convExpr g (C c)        sm = variable g g (codeName (SFCN (symbLookup c sm)))
convExpr g (Index a i)  sm = (convExpr g a sm)$.(listAccess $ convExpr g i sm)
convExpr g (Len a)      sm = (convExpr g a sm)$.listSize
convExpr g (Append a v) sm = (convExpr g a sm)$.(listAppend $ convExpr g v sm)
convExpr g (FCall (C c) x) sm = fApp g g (codeName (SFCN (symbLookup c sm))) (map (flip (convExpr g) sm) x)
convExpr _ (FCall _ _)   _ = litString "**convExpr :: BinaryOp unimplemented**"
convExpr g (a := b)     sm = (convExpr g a sm) ?== (convExpr g b sm)
convExpr g (a :!= b)    sm = (convExpr g a sm) ?!= (convExpr g b sm)
convExpr g (a :> b)     sm = (convExpr g a sm) ?>  (convExpr g b sm)
convExpr g (a :< b)     sm = (convExpr g a sm) ?<  (convExpr g b sm)
convExpr g (a :<= b)    sm = (convExpr g a sm) ?<= (convExpr g b sm)
convExpr g (a :>= b)    sm = (convExpr g a sm) ?>= (convExpr g b sm)
convExpr g (UnaryOp u)  sm = unop g u sm
convExpr g (Grouping e) sm = convExpr g e sm
convExpr _ (BinaryOp _)  _ = litString "**convExpr :: BinaryOp unimplemented**"
convExpr _ (Case _)      _ = error "**convExpr :: Case should be dealt with separately**"
convExpr _ _             _ = litString "**convExpr :: ? unimplemented**"

unop :: Generator -> UFunc -> SymbolMap -> Value
unop g (E.Sqrt e)  sm = (#/^) (convExpr g e sm)
unop g (E.Log e)   sm = I.log (convExpr g e sm)
unop g (E.Abs e)   sm = (#|)  (convExpr g e sm)
unop g (E.Exp e)   sm = I.exp (convExpr g e sm)
unop g (E.Sin e)   sm = I.sin (convExpr g e sm)
unop g (E.Cos e)   sm = I.cos (convExpr g e sm)
unop g (E.Tan e)   sm = I.tan (convExpr g e sm)
unop g (E.Csc e)   sm = I.csc (convExpr g e sm)
unop g (E.Sec e)   sm = I.sec (convExpr g e sm)
unop g (E.Cot e)   sm = I.cot (convExpr g e sm)
unop _ _            _ = error "not implemented"


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
unopcase (E.Sqrt e)         = containsCase e
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
unopcomcase (E.Sqrt e)  = compactCaseUnary (UnaryOp . E.Sqrt) e
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
genModDef :: Generator -> CS.Mod -> SymbolMap -> Module
genModDef g (CS.Mod n fs) sm = genModule g n (Just $ \x -> map (flip (genFunc x) sm) fs) Nothing

genFunc :: Generator -> Func -> SymbolMap -> Method
genFunc g (FDef (FuncDef n i o s)) sm = publicMethod g g 
  (methodType $ convType o) n (getParams g i) [ block (map (flip (convStmt g) sm) s) ]
genFunc g (FData (FuncData n dd))   _ = genDataFunc g n dd
genFunc g (FCD cd) _ = genCalcFunc g g cd

convStmt :: Generator -> FuncStmt -> SymbolMap -> Statement
convStmt g (FAsg v e) sm = assign g g (var $ codeName v) (convExpr g e sm)
convStmt g (FFor v e st) sm = for (varDecDef (codeName v) int (litInt 0)) (convExpr g e sm) ((&++) (var (codeName v)))
  [ block (map (flip (convStmt g) sm) st) ]
convStmt g (FWhile e st) sm = while (convExpr g e sm) [ block (map (flip (convStmt g) sm) st) ]
convStmt g (FCond e tSt []) sm = ifCond [(convExpr g e sm, 
  [ block (map (flip (convStmt g) sm) tSt) ])] noElse
convStmt g (FCond e tSt eSt) sm = ifCond [(convExpr g e sm, 
  [ block (map (flip (convStmt g) sm) tSt) ])] [ block (map (flip (convStmt g) sm) eSt) ]  
convStmt g (FRet e) sm = return $ convExpr g e sm
convStmt _ (FThrow s) _ = throw s
convStmt g (FTry t c) sm = tryCatch [ block (map (flip (convStmt g) sm) t) ] 
  [ block (map (flip (convStmt g) sm) c) ]
convStmt _ (FContinue) _ = continue
convStmt g (FVal e) sm = valStmt $ convExpr g e sm
convStmt _ (FDec v (C.List t)) _ = listDec' (codeName v) (convType t) 0
convStmt _ (FDec v t) _ = varDec (codeName v) (convType t)

-- this is really ugly!!    
genDataFunc :: Generator -> Name -> DataDesc -> Method
genDataFunc g name dd = 
    publicMethod g g methodTypeVoid name (p_filename : (getParams g $ getInputs dd)) $
      body $ [
      varDec l_infile infile,
      varDec l_line string,
      listDec' l_lines string 0,
      listDec' l_linetokens string 0,
      openFileR v_infile v_filename ] ++ 
      (concatMap inData dd) ++ [
      closeFile v_infile ]
  where inData :: Data -> [Statement]
        inData (Singleton v) = [getFileInput v_infile (convType $ codeType v) (variable g g $ codeName v)]
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
        entryData tokIndex _ _ (Entry v) = [assign g g (variable g g $ codeName v) $
          (v_linetokens$.(listAccess tokIndex))$.(cast (convType $ codeType v) string)]
        entryData tokIndex lineNo patNo (ListEntry indx v) =
          checkIndex indx lineNo patNo (variable g g $ codeName v) (codeType v) ++
            [ assign g g (indexData indx lineNo patNo (variable g g $ codeName v)) $
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
