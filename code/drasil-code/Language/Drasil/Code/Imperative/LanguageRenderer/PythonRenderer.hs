-- | The logic to render Python code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.PythonRenderer (
    -- * Python Code Configuration -- defines syntax of all Python code
    pythonConfig
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST 
  hiding (body,comment,bool,int,float,char,guard,update)
import Language.Drasil.Code.Imperative.Build.AST (interpMM)
import Language.Drasil.Code.Imperative.LanguageRenderer (Config(Config), FileType(Source),
  DecDef(Dec, Def), getEnv, complexDoc, inputDoc, ioDoc, functionListDoc, functionDoc, unOpDoc,
  valueDoc, methodTypeDoc, methodDoc, methodListDoc, statementDoc, stateDoc, stateListDoc,
  scopeDoc, retDoc, printDoc, patternDoc, paramDoc, paramListDoc, classDoc, objAccessDoc,
  objVarDoc, clsDecListDoc, clsDecDoc, litDoc, iterationDoc, funcDoc, funcAppDoc, exprDoc,
  exceptionDoc, declarationDoc, enumElementsDoc, conditionalDoc, callFuncParamList,
  blockDoc, bodyDoc,binOpDoc, body, bottom, top, assignDoc, elseIf, ifBodyStart, blockEnd,
  printFunc, printFileFunc, printFileLnFunc, printLnFunc, stateType, blockStart, clsDec,
  listObj, package, list, iterInLabel, iterForEachLabel, inherit, inputFunc, include,
  includeScope, fileName, ext, dir, enumsEqualInts, commentStart, endStatement, bitArray,
  renderCode, argsList, Options, ioDocD, StatementLocation(NoLoop, Loop), dot, inputDocD,
  valueDocD, valueDocD', methodListDocD, paramDocD, paramListDocD,
  objAccessDocD, iterationDocD, funcDocD, stateTypeD, fileCode,
  functionListDocD, methodTypeDocD, statementDocD, stateDocD, stateListDocD,
  retDocD, patternDocD, clsDecListDocD, clsDecDocD, funcAppDocD, 
  litDocD, callFuncParamListD, bodyDocD, blockDocD, binOpDocD,
  classDec, fileNameD, forLabel, exprDocD, declarationDocD,
  typeOfLit, fixCtorNames, unOpDocD', conditionalDocD', assignDocD', runnable)
import Language.Drasil.Code.Imperative.Helpers (blank,oneTab)

import Data.List (intersperse)
import Prelude hiding (print,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, semi, colon, parens, (<>), (<+>), empty, equals,
  brackets, vcat, doubleQuotes, render, char, int, ($+$))

pythonConfig :: Options -> Config -> Config
pythonConfig _ c = 
    Config {
        renderCode = renderCode' c,
        
        argsList         = text "sys.argv",
        bitArray         = empty,
        commentStart     = text "#",
        endStatement     = empty,
        enumsEqualInts   = True,
        ext              = ".py",
        dir              = "python",
        runnable         = interpMM "python",
        fileName         = fileNameD c,
        include          = include',
        includeScope     = const empty,
        inherit          = empty,
        inputFunc        = text "raw_input()",    --change to "input()" for Python 3.0+
        iterForEachLabel = forLabel,
        iterInLabel      = text "in",
        list             = const empty,
        listObj          = empty,
        clsDec           = classDec,
        package          = const empty,
        printFunc        = text "print",
        printLnFunc      = empty,
        printFileFunc    = const empty,
        printFileLnFunc  = const empty,
        stateType        = pystateType c,
        
        blockStart = colon, blockEnd = empty,
        ifBodyStart = blockStart c, elseIf = text "elif",
        
        top    = pytop c,
        body   = pybody c,
        bottom = const empty,
        
        assignDoc = assignDocD' c, binOpDoc = binOpDoc', bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamListD c,
        conditionalDoc = conditionalDocD' c, declarationDoc = declarationDoc' c, enumElementsDoc = enumElementsDoc' c, exceptionDoc = exceptionDoc' c, exprDoc = exprDoc' c, funcAppDoc = funcAppDocD c,
        funcDoc = funcDoc' c, iterationDoc = iterationDoc' c, litDoc = litDoc',
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDoc' c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDoc' c, paramDoc = paramDoc' c, paramListDoc = paramListDocD c, patternDoc = patternDocD c, printDoc = printDoc' c, retDoc = retDocD c, scopeDoc = const empty,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDoc' c,
        methodListDoc = methodListDocD c, methodTypeDoc = methodTypeDocD c, 
        functionListDoc = functionListDocD c, functionDoc = functionDoc' c,
        unOpDoc = unOpDocD'', valueDoc = valueDoc' c, ioDoc = ioDoc' c,
        inputDoc = inputDoc' c,
        complexDoc = complexDoc' c,
        getEnv = const $ error "getEnv for pythong not yet implemented"
    }

-- convenience
imp, incl, initName :: Label
imp = "import*"
incl = "from"
initName = "__init__"

unOpDocD'' :: UnaryOp -> Doc
unOpDocD'' Ln = text "math.log"
unOpDocD'' Log = text "math.log10"
unOpDocD'' op = unOpDocD' op

-- short names, packaged up above (and used below)
renderCode' :: Config -> AbstractCode -> Code
renderCode' c (AbsCode p) = Code $ fileCode c p Source (ext c)

include' :: Label -> Doc
include' n = text incl <+> text n <+> text imp

pystateType :: Config -> StateType -> DecDef -> Doc
pystateType _   (List _ _)     _ = brackets (empty)
pystateType c s@(Base Integer) d = stateTypeD c s d
pystateType c s@(Base Float)   d = stateTypeD c s d
pystateType _   (Base String)  _ = text "str"
pystateType _   (Base _)       _ = empty
pystateType c  s               d = stateTypeD c s d

pytop :: Config -> FileType -> Label -> Module -> Doc
pytop _ _ _ m =
  vcat [
    text "from __future__ import print_function",
    text "import sys",
    text "import math" 
  ] 
  $+$
  (vcat $ map (\x -> text "import" <+> text x) (libs m))
      


pybody :: Config -> FileType -> Label -> Module -> Doc
pybody c f p (Mod _ _ vs fs cs) = 
  (vcat $ map (statementDoc c NoLoop . DeclState) vs)
  $+$ blank $+$
  functionListDoc c f p fs
  $+$ blank $+$
  (vcat $ intersperse blank (map (classDoc c f p) (fixCtorNames initName cs))) 

-- code doc functions
binOpDoc' :: BinaryOp -> Doc
binOpDoc' Power = text "**"
binOpDoc' And = text "and"
binOpDoc' Or = text "or"
binOpDoc' op = binOpDocD op

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' _ (VarDec _ _) = empty
declarationDoc' c (ListDec lt n t _) =  text n <+> equals <+> stateType c (List lt t) Dec
declarationDoc' c (ListDecValues _ n _ vs) = text n <+> equals <+> brackets (callFuncParamList c vs)
declarationDoc' c (VarDecDef n _ v) = text n <+> equals <+> valueDoc c v
declarationDoc' c (ConstDecDef n l) = declarationDoc c $ VarDecDef n (Base $ typeOfLit l) (Lit l)
declarationDoc' c d = declarationDocD c d

enumElementsDoc' :: Config -> [Label] -> Doc
enumElementsDoc' _ es = vcat $
    zipWith (\e i -> text e <+> equals <+> int i) es nums
    where nums = [0..length es - 1]
    
exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' c (Throw s) = text "raise" <+> text "Exception" <> parens (litDoc c $ LitStr s)
exceptionDoc' c (TryCatch tryB catchB) = vcat [
    text "try" <+> colon,
    oneTab $ bodyDoc c tryB,
    text "except" <+> text "Exception" <+> text "as" <+> text "exc" <+> colon,
    oneTab $ bodyDoc c catchB]

exprDoc' :: Config -> Expression -> Doc
exprDoc' c (Exists (ObjAccess v (ListAccess i))) = exprDoc c $ BinaryExpr (v $. ListSize) Greater i
exprDoc' c e@(Exists (Arg _)) = exprDocD c e
exprDoc' c (Exists v) = exprDoc c $ BinaryExpr v NotEqual $ var "None"
exprDoc' c e = exprDocD c e

funcDoc' :: Config -> Function -> Doc
funcDoc' c (Cast t _) = stateType c t Def
funcDoc' _ (Get n) = dot <> text n
funcDoc' c (Set n v) = dot <> text n <+> equals <+> valueDoc c v
funcDoc' c (IndexOf v) = dot <> funcAppDoc c "index" [v]
funcDoc' _ ListSize = text "len"
funcDoc' c (ListAccess i) = brackets $ valueDoc c i
funcDoc' c (ListAdd i v) = dot <> funcAppDoc c "insert" [i, v]
funcDoc' c (ListPopulate size t) = brackets (valueDoc c dftVal) <+> char '*' <+> valueDoc c size
    where dftVal = case t of Base bt   -> defaultValue bt
                             _         -> error $ "ListPopulate does not yet support list type " ++ render (doubleQuotes $ stateType c t Def)
funcDoc' c (ListExtend t) = dot <> text "append" <> parens (dftVal)
    where dftVal = case t of Base bt   -> valueDoc c (defaultValue bt)
                             List _ _  -> brackets empty   
                             _         -> error $ "ListExtend does not yet support list type " ++ render (doubleQuotes $ stateType c t Def)
funcDoc' c f = funcDocD c f

iterationDoc' :: Config -> Iteration -> Doc
iterationDoc' c (For (DeclState (VarDecDef i (Base Integer) initv)) (Expr (BinaryExpr _ Less finalv)) (AssignState (PlusPlus _)) b) = 
    vcat [
        forLabel <+> text i <+> (iterInLabel c) <+> text "range" <> parens (valueDoc c initv <> text ", " <> valueDoc c finalv) <> colon,
        oneTab $ bodyDoc c b]
iterationDoc' c (For (DeclState (VarDecDef i (Base Integer) initv)) (Expr (BinaryExpr _ Less finalv)) (AssignState (PlusEquals _ stepv)) b) = 
    vcat [
        forLabel <+> text i <+> (iterInLabel c) <+> text "range" <> parens (valueDoc c initv <> text ", " <> valueDoc c finalv <> text ", " <> valueDoc c stepv) <> colon,
        oneTab $ bodyDoc c b]
iterationDoc' c (For initv guard update b) = vcat [
    forLabel <+> statementDoc c Loop initv <> semi <+> valueDoc c guard <> semi <+> statementDoc c Loop update,
    oneTab $ bodyDoc c b]
iterationDoc' c (ForEach i listVar@(ListVar _ _) b) = vcat [
    (iterForEachLabel c) <+> valueDoc c (var i) <+> (iterInLabel c) <+> valueDoc c listVar <> colon,
    oneTab $ bodyDoc c b]
iterationDoc' c i = iterationDocD c i

litDoc' :: Literal -> Doc
litDoc' (LitBool True) = text "True"
litDoc' (LitBool False) = text "False"
litDoc' l = litDocD l

classDoc' :: Config -> FileType -> Label -> Class -> Doc
classDoc' c f _ (MainClass _ _ fs) = methodListDoc c f "" fs
classDoc' c f _ m = vcat [
    clsDec c <+> text (className m) <> baseClass <> colon,
    oneTab $ modInnerDoc]
    where modInnerDoc = case m of (Class n _ _ _ fs) -> methodListDoc c f n fs
                                  (Enum _ _ es) -> enumElementsDoc c es
                                  MainClass{} -> error "unreachable"
          baseClass = case m of (Class _ p _ _ _) -> case p of Nothing -> empty
                                                               Just pn -> parens (text pn)
                                _ -> empty

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v@(Self) f = valueDoc c v <> funcDoc c f
objAccessDoc' c v f@(ListSize) = funcDoc c f <> parens (valueDoc c v)
objAccessDoc' c v f@(ListPopulate _ _) = valueDoc c v <+> equals <+> funcDoc c f
objAccessDoc' c v   (Floor) = funcAppDoc c "math.floor" [v]
objAccessDoc' c v   (Ceiling) = funcAppDoc c "math.ceil" [v]
objAccessDoc' c v f = objAccessDocD c v f

objVarDoc' :: Config -> Value -> Value -> Doc
objVarDoc' c v1 v2 = valueDoc c v1 <> dot <> valueDoc c v2

paramDoc' :: Config -> Parameter -> Doc
paramDoc' _ (StateParam n _) = text n
paramDoc' c p = paramDocD c p

printDoc' :: Config -> IOType -> Bool -> StateType -> Value -> Doc
printDoc' c Console False _ v = printFunc c <> parens (valueDoc c v <> text ", end=''")
printDoc' c Console True _ v = printFunc c <> parens (valueDoc c v)
printDoc' c (File f) False _ v = printFunc c <> parens (valueDoc c v <> text ", end='', file=" <> valueDoc c f)
printDoc' c (File f) True _ v = printFunc c <> parens (valueDoc c v <> text ", file=" <> valueDoc c f)

methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c _ _ (Method n _ _ (Construct _) ps b) = vcat [
    text "def" <+> text n <> parens (valueDoc c Self <> oneParam <> paramListDoc c ps) <> colon,
    oneTab $ bodyDoc c b]
        where oneParam | not $ null ps = text ", "
                       | otherwise     = empty
methodDoc' c _ _ (Method n _ _ _ ps b) = vcat [
    text "def" <+> text n <> parens (valueDoc c Self <> oneParam <> paramListDoc c ps) <> colon,
    oneTab bodyD]
        where oneParam | not $ null ps = text ", "
                       | otherwise     = empty
              bodyD | null b    = text "None"
                    | otherwise = bodyDoc c b
methodDoc' c _ _ (MainMethod b) = bodyDoc c b
methodDoc' _ _ _ _ = empty

valueDoc' :: Config -> Value -> Doc
valueDoc' _ (Self) = text "self"
valueDoc' c (StateObj _ t@(List _ _) _) = stateType c t Def
valueDoc' c (StateObj l t vs) = prefixLib l <> stateType c t Def <> parens (callFuncParamList c vs)
  where prefixLib Nothing = empty
        prefixLib (Just lib) = text lib <> dot
valueDoc' c v@(Arg _) = valueDocD' c v
valueDoc' c (FuncApp (Just l) n vs) = funcAppDoc c (l ++ "." ++ n) vs
valueDoc' c (Condi cond te ee) = parens (valueDoc' c te <+> text "if" <+>
          parens (valueDoc' c cond) <+> text "else" <+> valueDoc' c ee)
valueDoc' c v = valueDocD c v

functionDoc' :: Config -> FileType -> Label -> Method -> Doc
functionDoc' _ _ _ (Method _ _ _ (Construct _) _ _) = error "Constructor cannot exist outside of class"
functionDoc' c _ _ (Method n _ _ _ ps b) = vcat [
    text "def" <+> text n <> parens (paramListDoc c ps) <> colon,
    oneTab bodyD]
        where bodyD | null b    = text "None"
                    | otherwise = bodyDoc c b
functionDoc' c _ _ (MainMethod b) = bodyDoc c b
functionDoc' _ _ _ _ = error "Class method type cannot exist outside of class"

inputDoc' :: Config -> IOType -> StateType -> Maybe Value -> Doc
inputDoc' c io _ Nothing = statementDoc c NoLoop (valStmt $ inputFn io)
inputDoc' c io (Base Boolean) (Just v) = statementDoc c NoLoop
  (v &= inputFn io ?!= litString "0")
inputDoc' c io (Base Integer) (Just v) = statementDoc c NoLoop
  (v &= funcApp' "int" [inputFn io])
inputDoc' c io (Base Float) (Just v) = statementDoc c NoLoop
  (v &= funcApp' "float" [inputFn io])
inputDoc' c io (Base String) (Just v) = statementDoc c NoLoop
  (v &= objMethodCall (inputFn io) "rstrip" [])
inputDoc' _ _ (Base (FileType _)) _ = error "File type is not valid input"
inputDoc' c io (Base _) (Just v) = statementDoc c NoLoop
  (v &= inputFn io)
inputDoc' c io s v = inputDocD c io s v 

ioDoc' :: Config -> IOSt -> Doc
ioDoc' c (OpenFile f n m) = statementDoc c NoLoop (f &= funcApp' "open" [n, litString (modeStr m)])
  where modeStr Read = "r"
        modeStr Write = "w"  
ioDoc' c io = ioDocD c io

complexDoc' :: Config -> Complex -> Doc
complexDoc' c (ReadLine f (Just v)) = statementDoc c NoLoop (v &= objMethodCall f "readline" [])
complexDoc' c (ReadLine f Nothing)  = statementDoc c NoLoop (valStmt $ objMethodCall f "readline" [])
complexDoc' c (ReadAll f v) = statementDoc c NoLoop (v &= objMethodCall f "readlines" [])
complexDoc' c (ListSlice _ vnew vold b e s) = 
  valueDoc c vnew <+> equals <+> valueDoc c vold <> (brackets $ 
  getVal b <> colon <> getVal e <> colon <> getVal s)
    where getVal Nothing  = empty
          getVal (Just v) = valueDoc c v
complexDoc' c (StringSplit vnew s d) = 
  valueDoc c vnew <+> equals <+> valueDoc c s <> dot <> funcAppDoc c "split" [litString [d]]
  
-- helpers

inputFn :: IOType -> Value
inputFn Console = funcApp' "raw_input" []
inputFn (File f) = objMethodCall f "readline" []
