module Language.Drasil.Code.Imperative.Import where

import Language.Drasil.Code.Code as C
import Language.Drasil.Code.Imperative.AST as I
import Language.Drasil.Code.Imperative.LanguageRenderer (Options(..))
import Language.Drasil.Code.Imperative.Parsers.ConfigParser (pythonLabel)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code
import Language.Drasil.Expr as E
import Language.Drasil.Expr.Extract hiding (vars)
import Language.Drasil.CodeSpec

import Prelude hiding (log, exp, return)
import Data.List (intersperse)

data Generator = Generator { 
  generateCode :: IO (),
  
  genModules :: [Module],
  
  genInputMod :: [CodeChunk] -> ConstraintMap -> [Module],
  genCalcMod :: [CodeDefinition] -> [Module],
  genOutputMod :: [CodeChunk] -> [Module],
  
  genInputClass :: [CodeChunk] -> ConstraintMap -> Class,
  genInputFormat :: [CodeChunk] -> Method,
  genInputConstraints :: [CodeChunk] -> ConstraintMap -> Method,

  genCalcFuncs :: [CodeDefinition] -> [Method],
  genCalcBlock :: CodeDefinition -> Body,
  genCaseBlock :: [(Expr,Relation)] -> Body,  

  genOutputFormat :: [CodeChunk] -> Method,
  
  genMethodCall :: Scope -> Permanence -> MethodType -> Label -> [Parameter] -> Body -> Method,
  
  logName :: String,
  
  publicMethod :: MethodType -> Label -> [Parameter] -> Body -> Method,
  privateMethod :: MethodType -> Label -> [Parameter] -> Body -> Method,
  
  sfwrCBody :: Expr -> Body,
  physCBody :: Expr -> Body  
}

generator :: CodeSpec -> Generator -> Generator
generator spec g = 
  let chs = choices spec
      methodCallFunc =     case (logging chs) of          LogFunc   -> loggedMethod
                                                          LogAll    -> loggedMethod
                                                          _         -> genMethodCallD
      sfwrConstraintFunc = case (onSfwrConstraint chs) of Warning   -> constrWarn
                                                          Exception -> constrExc
      physConstraintFunc = case (onPhysConstraint chs) of Warning   -> constrWarn
                                                          Exception -> constrExc
      inputModFunc =       case (inputStructure chs)   of Loose     -> genInputModNoClass
                                                          AsClass   -> genInputModClass
  in Generator {
      generateCode = generateCodeD spec g,
      
      genModules = genModulesD spec g,
      
      genInputMod = inputModFunc g,
      genCalcMod = genCalcModD g,
      genOutputMod = genOutputModD g,
      
      genInputClass = genInputClassD g,
      genInputFormat = genInputFormatD g,
      genInputConstraints = genInputConstraintsD g,

      genCalcFuncs = genCalcFuncsD g,
      genCalcBlock = genCalcBlockD g,
      genCaseBlock = genCaseBlockD g,  

      genOutputFormat = genOutputFormatD g,
      
      genMethodCall = methodCallFunc g,
      
      logName = logFile $ choices spec,
      
      publicMethod = genMethodCall g Public Static,
      privateMethod = genMethodCall g Private Dynamic,
      
      sfwrCBody = sfwrConstraintFunc g,
      physCBody = physConstraintFunc g
    }

generateCodeD :: CodeSpec -> Generator -> IO () 
generateCodeD s g = let modules = genModules g 
   in createCodeFiles $ makeCode 
        pythonLabel
        (Options Nothing Nothing Nothing (Just "Code")) 
        (toAbsCode (codeName $ program s) modules)

genModulesD :: CodeSpec -> Generator -> [Module]
genModulesD (CodeSpec _ i o d cm _) g = genInputMod g i cm
                                     ++ genCalcMod g d
                                     ++ genOutputMod g o


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
        [zipBlockWith (&=) vars vals],
      genInputFormat g ins,
      genInputConstraints g ins cm
    ]
  )  
  where vars         = map (\x -> var $ codeName x) ins
        vals         = map (\x -> defaultValue' $ convType $ codeType x) ins        
        genInputVars = 
          map (\x -> pubMVar 4 (convType $ codeType x) (codeName x)) ins

genInputFormatD :: Generator -> [CodeChunk] -> Method
genInputFormatD g ins = let l_infile = "infile"
                            v_infile = var l_infile
                            l_filename = "filename"
                            p_filename = param l_filename string
                            v_filename = var l_filename
  in
    publicMethod g methodTypeVoid "get_inputs" 
      [ p_filename ]
      [ block $
          [
            varDec l_infile infile,
            openFileR v_infile v_filename
          ] 
          ++
          map (\x -> getFileInput v_infile (convType $ codeType x) (var $ codeName x)) ins
          ++ 
          [ closeFile v_infile ]
      ]
          
genInputConstraintsD :: Generator -> [CodeChunk] -> ConstraintMap -> Method
genInputConstraintsD g vars cm = 
  let sfwrCs = concatMap (\x -> sfwrLookup x cm) vars 
      physCs = concatMap (\x -> physLookup x cm) vars
  in
    publicMethod g methodTypeVoid "input_constraints" [] [ block $
      (map (\x -> ifCond [((?!) (convExpr x), sfwrCBody g x)] noElse) sfwrCs)
      ++
      (map (\x -> ifCond [((?!) (convExpr x), physCBody g x)] noElse) physCs)
    ]

-- need Expr -> String to print constraint
constrWarn :: Generator -> Expr -> Body
constrWarn _ _ = oneLiner $ printStrLn "Warning: constraint violated"

constrExc :: Generator -> Expr -> Body
constrExc _ _ = oneLiner $ throw "InputError"

    
------- CALC ----------    
    
genCalcModD :: Generator -> [CodeDefinition] -> [Module]
genCalcModD g defs = [buildModule "Calculations" [] [] (genCalcFuncs g defs) []]   
        
genCalcFuncsD :: Generator -> [CodeDefinition] -> [Method]
genCalcFuncsD g = map 
  ( \x -> publicMethod g 
            (methodType $ convType (codeType x)) 
            ("calc_" ++ codeName x) 
            (getParams (codevars $ codeEquat x)) 
            (genCalcBlock g x)
  )

genCalcBlockD :: Generator -> CodeDefinition -> Body
genCalcBlockD g def 
  | isCase (codeEquat def) = genCaseBlock g $ getCases (codeEquat def)
  | otherwise              = oneLiner $ return $ convExpr (codeEquat def)
  where isCase (Case _) = True
        isCase _        = False
        getCases (Case cs) = cs
        getCases _         = error "impossible to get here"


genCaseBlockD :: Generator -> [(Expr,Relation)] -> Body
genCaseBlockD g cs = oneLiner $ ifCond (genIf cs) noElse
  where genIf :: [(Expr,Relation)] -> [(Value,Body)]
        genIf = map 
          (\(e,r) -> (convExpr r, oneLiner $ return (convExpr e)))


----- OUTPUT -------
          
genOutputModD :: Generator -> [CodeChunk] -> [Module]
genOutputModD g outs = [buildModule "OutputFormat" [] [] [genOutputFormat g outs] []]  
    
genOutputFormatD :: Generator -> [CodeChunk] -> Method
genOutputFormatD g outs = let l_outfile = "outfile"
                              v_outfile = var l_outfile
                              l_filename = "filename"
                              p_filename = param l_filename string
                              v_filename = var l_filename
  in
    publicMethod g methodTypeVoid "write_output" 
      (p_filename:getParams outs)
      [ block $
          [
            varDec l_outfile outfile,
            openFileW v_outfile v_filename
          ] 
          ++
          concatMap 
            (\x -> [ printFileStr v_outfile ((codeName x) ++ " = "), 
                     printFileLn v_outfile (convType $ codeType x) 
                       (var $ codeName x)
                   ] ) outs
          ++ 
          [ closeFile v_outfile ]
      ]         
    
-----

genMethodCallD :: Generator -> Scope -> Permanence -> MethodType -> Label -> [Parameter] 
                  -> Body -> Method
genMethodCallD _ s pr t n p b = Method n s pr t p b

loggedMethod :: Generator -> Scope -> Permanence -> MethodType -> Label -> [Parameter] 
                  -> Body -> Method
loggedMethod g s pr t n p b = let l_outfile = "outfile"
                                  v_outfile = var l_outfile 
  in
    Method n s pr t p $ 
    (
      block [
        varDec l_outfile outfile,
        openFileW v_outfile (litString $ logName g),  
        printFileStr v_outfile ("function " ++ n ++ "("),
        printParams p v_outfile,
        printFileStrLn v_outfile ") called",
        closeFile v_outfile      
      ]
    ) : b
  where
    printParams ps v_outfile = multi $ 
      intersperse (printFileStr v_outfile ", ") $
      map (\x -> printFile v_outfile (paramType x) (paramVal x)) ps
  

-- helpers
    
getParams :: (CodeEntity c) => [c] -> [Parameter]
getParams = map (\y -> param (codeName y) (convType $ codeType y))
          
paramType :: Parameter -> StateType
paramType (StateParam _ s) = s
paramType (FuncParam _ _ _) = error "Function param not implemented"

paramVal :: Parameter -> Value
paramVal (StateParam l _) = var l
paramVal (FuncParam _ _ _) = error "Function param not implemented"        
          
convType :: C.CodeType -> I.StateType
convType C.Boolean = bool
convType C.Integer = int
convType C.Float = float
convType C.Char = char
convType C.String = string
convType (C.List t) = listT $ convType t
convType (C.Object n) = obj n
convType _ = error "No type conversion"

convExpr :: Expr -> Value
convExpr (V v)        = litString v  -- V constructor should be removed
convExpr (Dbl d)      = litFloat d
convExpr (Int i)      = litInt i
convExpr (Bln b)      = litBool b
convExpr (a :/ b)     = (convExpr a) #/ (convExpr b)
convExpr (a :* b)     = (convExpr a) #* (convExpr b)
convExpr (a :+ b)     = (convExpr a) #+ (convExpr b)
convExpr (a :^ b)     = (convExpr a) #^ (convExpr b)
convExpr (a :- b)     = (convExpr a) #- (convExpr b)
convExpr (a :. b)     = (convExpr a) #* (convExpr b)
convExpr (a :&& b)    = (convExpr a) ?&& (convExpr b)
convExpr (a :|| b)    = (convExpr a) ?|| (convExpr b)
convExpr (Deriv _ _ _) = error "not implemented"
convExpr (E.Not e)      = (?!) (convExpr e)
convExpr (Neg e)      = (#~) (convExpr e)
convExpr (C c)        = var (codeName (SFCN c))
convExpr (FCall (C c) x)  = funcApp' (codeName (SFCN c)) (map convExpr x)
convExpr (FCall _ _)  = error "not implemented"
convExpr (a := b)     = (convExpr a) ?== (convExpr b)
convExpr (a :!= b)    = (convExpr a) ?!= (convExpr b)
convExpr (a :> b)     = (convExpr a) ?> (convExpr b)
convExpr (a :< b)     = (convExpr a) ?< (convExpr b)
convExpr (a :<= b)    = (convExpr a) ?<= (convExpr b)
convExpr (a :>= b)    = (convExpr a) ?>= (convExpr b)
convExpr (UnaryOp u)  = unop u
convExpr (Grouping e) = convExpr e
convExpr (BinaryOp _) = error "not implemented"
convExpr (Case _)     = error "Case should be dealt with separately"
convExpr _            = error "not implemented"

unop :: UFunc -> Value
unop (E.Log e)          = I.log (convExpr e)
unop (E.Abs e)          = (#|) (convExpr e)
unop (E.Exp e)          = I.exp (convExpr e)
unop _                  = error "not implemented"
