{-# LANGUAGE PostfixOperators #-}
-- | The logic to render C++ code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer (
    -- * C++ Code Configuration -- defines syntax of all C++ code
    cppConfig
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST
  hiding (body, comment, bool, int, float, char, tryBody, catchBody, initState, guard, update)
import Language.Drasil.Code.Imperative.Build.AST (buildAll, cppCompiler, nativeBinary)
import Language.Drasil.Code.Imperative.LanguageRenderer (Config(Config), FileType(Source, Header),
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
  valueDocD, valueDocD', methodDocD, methodDocD', methodListDocD, paramDocD, paramListDocD, 
  objAccessDocD, iterationDocD, funcDocD, declarationDocD, assignDocD, stateTypeD, fileCode,
  functionListDocD, methodTypeDocD, unOpDocD, statementDocD, scopeDocD, stateDocD, stateListDocD,
  doubleSlash, retDocD, patternDocD, clsDecListDocD, clsDecDocD, funcAppDocD, enumElementsDocD,
  exprDocD', litDocD, conditionalDocD'', callFuncParamListD, bodyDocD, blockDocD, binOpDocD,
  classDec, namespaceD, includeD, fileNameD, cpplist, buildConfig, runnable)
import Language.Drasil.Code.Imperative.Helpers (blank, oneTab, oneTabbed, vmap, vibmap)

import Prelude hiding (break, print, return,(<>))
import Data.List.Utils (endswith)
import Text.PrettyPrint.HughesPJ hiding (Str)

validListTypes :: [Label]
validListTypes = ["deque", "vector"]

cppConfig :: Options -> Config -> Config
cppConfig options c =
    let listType = case (cpplist options) of Nothing -> "vector"
                                             Just lt -> if lt `elem` validListTypes then lt
                                                        else error $ "Unsupported C++ list type specified in config file: " ++ lt ++ "\nSupported types are: " ++ show validListTypes
    in Config {
        renderCode = renderCode' c,

        argsList         = text "argv",
        bitArray         = text "vector<bool>",
        commentStart     = doubleSlash,
        endStatement     = semi,
        enumsEqualInts   = False,
        ext              = ".cpp",
        dir              = "cpp",
        buildConfig      = buildAll $ \i o -> [cppCompiler, unwords $
          filter (not . endswith ".hpp") i, "--std=c++11", "-o", o],
        runnable         = nativeBinary,
        fileName         = fileNameD c,
        include          = includeD "#include",
        includeScope     = const empty,
        inherit          = colon,
        inputFunc        = text "std::cin",
        iterForEachLabel = empty,
        iterInLabel      = empty,
        list             = const $ text listType,
        listObj          = empty,
        clsDec           = classDec,
        package          = namespaceD,
        printFunc        = text "std::cout",
        printLnFunc      = text "std::cout",
        printFileFunc    = valueDoc c,
        printFileLnFunc  = valueDoc c,
        stateType        = cppstateType c,

        blockStart = lbrace, blockEnd = rbrace,
        ifBodyStart = blockStart c, elseIf = text "else if",

        top    = cpptop c,
        body   = cppbody c,
        bottom = cppbottom,

        assignDoc = assignDoc' c, binOpDoc = binOpDocD, bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamListD c,
        conditionalDoc = conditionalDocD'' c, declarationDoc = declarationDoc' c, enumElementsDoc = enumElementsDocD c, exceptionDoc = exceptionDoc' c, exprDoc = exprDocD' c, funcAppDoc = funcAppDocD c,
        funcDoc = funcDoc' c, iterationDoc = iterationDoc' c, litDoc = litDocD,
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDoc' c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDoc' c, paramDoc = paramDoc' c, paramListDoc = paramListDocD c, patternDoc = patternDocD c, printDoc = printDoc' c, retDoc = retDocD c, scopeDoc = scopeDocD,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDoc' c,
        methodListDoc = methodListDoc' c, methodTypeDoc = methodTypeDocD c, unOpDoc = unOpDocD, valueDoc = valueDoc' c,
        functionDoc = functionDoc' c, functionListDoc = functionListDocD c,
        ioDoc = ioDoc' c,inputDoc = inputDoc' c,
        complexDoc = complexDoc' c,
        getEnv = const $ error "Cpp does not implement getEnv (yet)"
    }

-- for convenience
cppHeaderExt :: String
cppHeaderExt = ".hpp"

-- short names, packaged up above (and used below)
renderCode' :: Config -> AbstractCode -> Code
renderCode' c (AbsCode p@(Pack l ms)) =
  Code $ (fileCode c (Pack l (ignoreMain ms)) Header cppHeaderExt) ++
         (fileCode c p Source (ext c))


cppstateType :: Config -> StateType -> DecDef -> Doc
cppstateType _ (Base (FileType Read)) _    = text "ifstream"
cppstateType _ (Base (FileType Write)) _   = text "ofstream"
cppstateType _ (Base Boolean) _ = text "bool"
cppstateType _ (Base Float) _   = text "double"
cppstateType _ (Type name) Dec  = text name
cppstateType c (Iterator t) _   = text "std::" <> stateType c (List Dynamic t) Dec <> text "::iterator"
cppstateType c s d              = stateTypeD c s d

cpptop :: Config -> FileType -> Label -> Module -> Doc
cpptop c Header _ (Mod n l _ _ _) = vcat $
    (map (\x -> include c ("\"" ++ x ++ cppHeaderExt ++ "\"")) l)
    ++ [
    text "#ifndef" <+> text n <> text "_h",
    text "#define" <+> text n <> text "_h",
    blank,
    include c "<string>",
    include c $ "<" ++ render (list c Dynamic) ++ ">",
    blank,
    usingNameSpace c "std" (Just "string"),
    usingNameSpace c "std" (Just $ render (list c Dynamic)),
    usingNameSpace c "std" (Just "ifstream"),
    usingNameSpace c "std" (Just "ofstream")]
cpptop c Source _ m@(Mod n l _ _ _) = vcat $ [          --TODO remove includes if they aren't used
    if notMainModule m 
      then include c ("\"" ++ n ++ cppHeaderExt ++ "\"")
      else empty,
    blank] 
    ++
    (map (\x -> include c ("\"" ++ x ++ cppHeaderExt ++ "\"")) l)
    ++ 
    [blank,
    include c "<algorithm>",
    include c "<iostream>",
    include c "<fstream>",
    include c "<iterator>",     --used only when printing a list
    include c "<string>",
    include c "<math.h>",       --used for Floor and Ceiling functions
    include c "<sstream>",
    include c "<limits>",
    include c $ "<" ++ render (list c Dynamic) ++ ">",
    blank,
    usingNameSpace c "std" (Just "string"),
    usingNameSpace c "std" (Just $ render (list c Dynamic)),
    usingNameSpace c "std" (Just "ifstream"),
    usingNameSpace c "std" (Just "ofstream")]

cppbody :: Config -> FileType -> Label -> Module -> Doc
cppbody c f@(Header) p (Mod _ _ _ fs cs) =
    vcat [
      clsDecListDoc c cs,
      blank,
      vibmap (classDoc c f p) cs,
      blank,
      functionListDoc c f p fs
    ]
cppbody c f@(Source) p (Mod _ _ _ fs cs) =
   vcat [
     vibmap (classDoc c f p) cs,
     functionListDoc c f p fs
   ]

cppbottom :: FileType -> Doc
cppbottom Header = text "#endif"
cppbottom Source = empty

-- code doc functions
assignDoc' :: Config -> Assignment -> Doc
--assignDoc' c (Assign v Input) = inputFunc c <+> text ">>" <+> valueDoc c v
--assignDoc' c (Assign v (InputFile f)) = valueDoc c f <+> text ">>" <+> valueDoc c v
assignDoc' = assignDocD

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' c (ListDec lt n t s) = stateType c (List lt t) Dec <+> text n <> parens (int s)
declarationDoc' c (ListDecValues lt n t vs) = vcat [
    stateType c t Dec <+> text temp <> text "[]" <+> equals <+> braces (callFuncParamList c vs) <> endStatement c,
    stateType c (List lt t) Dec <+> text n <> parens(text temp <> comma <+> text temp <+> text "+" <+> text "sizeof" <> parens (text temp) <+> text "/" <+> text "sizeof" <> parens (text temp <> text "[0]"))]
    where temp = n ++ "_temp"
declarationDoc' c d = declarationDocD c d

exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' c (Throw s) = text "throw" <> parens (litDoc c $ LitStr s)
exceptionDoc' c (TryCatch tryBody catchBody) = vcat [
    text "try" <+> lbrace,
    oneTab $ bodyDoc c tryBody,
    rbrace <+> text "catch" <+> {- parens (text "string" <+> text "exc") <+> -} parens (text "...") <+> lbrace,
    oneTab $ bodyDoc c catchBody,
    rbrace]

funcDoc' :: Config -> Function -> Doc
funcDoc' c (Func n vs) = dot <> funcAppDoc c n vs
funcDoc' c (Get n) = dot <> funcAppDoc c (getterName n) []
funcDoc' c (Set n v) = dot <> funcAppDoc c (setterName n) [v]
funcDoc' _ (IndexOf _) = error "IndexOf function must be rendered at the ObjAccess level in C++"
funcDoc' c ListSize = dot <> funcAppDoc c "size" []
funcDoc' c (ListAccess i) = dot <> funcAppDoc c "at" [i]
funcDoc' c (ListAppend v) = dot <> funcAppDoc c "push_back" [v]
funcDoc' c (ListAdd _ v) = dot <> funcAppDoc c "push_back" [v]
funcDoc' c (ListSet i v) = dot <> funcAppDoc c "at" [i] <+> equals <+> valueDoc c v
funcDoc' c f = funcDocD c f

iterationDoc' :: Config -> Iteration -> Doc
iterationDoc' c (ForEach it listVar@(ListVar _ t) b) = iterationDoc c $ For initState guard update $ bodyReplace (var it) (var $ "(*" ++ it ++ ")") b
    where initState = DeclState $ VarDecDef it (Iterator t) (listVar $. IterBegin)
          guard     = binExpr (var it) NotEqual (listVar $. IterEnd)
          update    = (&.++)it
iterationDoc' c i = iterationDocD c i

classDoc' :: Config -> FileType -> Label -> Class -> Doc
classDoc' c (Header) _ (Enum n _ es) = vcat [
    text "enum" <+> text n <+> lbrace,
    oneTab $ enumElementsDoc c es,
    rbrace <> endStatement c]
classDoc' _ (Source) _ Enum{} = empty
classDoc' c ft@(Header) _ (Class n p _ vs fs) =
    let makeTransforms = map convertToMethod
        funcs = fs ++ [destructor c n vs]
        pubFuncs = concatMap (\f@(Method _ s _ _ _ _) -> [f | s == Public]) $ makeTransforms funcs
        pubVars = concatMap (\v@(StateVar _ s _ _ _) -> [v | s == Public]) vs
        privFuncs = concatMap (\f@(Method _ s _ _ _ _) -> [f | s == Private]) $ makeTransforms funcs
        privVars = concatMap (\v@(StateVar _ s _ _ _) -> [v | s == Private]) vs
        pubBlank = if null pubVars then empty else blank
        privBlank = if null privFuncs then empty else blank
        baseClass = case p of Nothing -> empty
                              Just pn -> inherit c <+> scopeDoc c Public <+> text pn
    in vcat [
        text "class" <+> text n <+> baseClass <+> lbrace,
        oneTabbed [
            scopeDoc c Public <> colon,
            oneTabbed [
                stateListDoc c pubVars,
                pubBlank,
                methodListDoc c ft n pubFuncs],
            blank,
            scopeDoc c Private <> colon,
            oneTabbed [
                stateListDoc c privVars,
                privBlank,
                methodListDoc c ft n privFuncs]],
        rbrace <> endStatement c]
classDoc' c ft@(Source) _ (Class n _ _ vs fs) = methodListDoc c ft n $ fs ++ [destructor c n vs]
classDoc' _ (Header) _ MainClass{} = empty
classDoc' c ft _ (MainClass _ vs fs) = vcat [
    stateListDoc c vs,
    stateBlank,
    methodListDoc c ft "" fs]
    where stateBlank = if null vs then empty else blank

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v (IndexOf vr) = funcAppDoc c "find" [v $. IterBegin, v $. IterEnd, vr] <+> text "-" <+> valueDoc c (v $. IterBegin)
objAccessDoc' c v   (Floor) = funcAppDoc c "floor" [v]
objAccessDoc' c v   (Ceiling) = funcAppDoc c "ceil" [v]
objAccessDoc' c v (Cast (Base Float) (Base String)) = funcAppDoc c "std::stod" [v]
objAccessDoc' c v (ListExtend t) = valueDoc c v <> dot <> text "push_back" <> parens (dftVal)
    where dftVal = case t of Base bt     -> valueDoc c (defaultValue bt)
                             List lt t'  -> stateType c (List lt t') Dec <> parens (empty)
                             _           -> error $ "ListExtend does not yet support list type " ++ render (doubleQuotes $ stateType c t Def)
objAccessDoc' c v f = objAccessDocD c v f

objVarDoc' :: Config -> Value -> Value -> Doc
objVarDoc' c (Self) v = valueDoc c v
objVarDoc' c v1 v2 = valueDoc c v1 <> dot <> valueDoc c v2

paramDoc' :: Config -> Parameter -> Doc
paramDoc' c (StateParam n t@(List _ _)) = stateType c t Dec <+> text "&" <> text n
paramDoc' c (StateParam n t@(Type _)) = stateType c t Dec <+> text "&" <> text n 
paramDoc' c p = paramDocD c p

printDoc' :: Config -> IOType -> Bool -> StateType -> Value -> Doc
printDoc' _ _ _ _ (ListVar _ (List _ _)) = error "C++: Printing of nested lists is not yet supported"
printDoc' c Console newLn _ v@(ListVar _ t) = vcat [
    statementDoc c NoLoop $ printStr "[",
    statementDoc c NoLoop $ ValState $ FuncApp Nothing "copy" [v $. IterBegin, v $. IterEnd, FuncApp Nothing iter [var "std::cout", litString ","]],
    statementDoc c Loop $ printLastStr "]"]
    where iter = "std::ostream_iterator<" ++ render(stateType c t Dec) ++ ">"
          printLastStr = if newLn then printStrLn else printStr
printDoc' c Console newLn _ v = printFunc c <+> text "<<" <+> valueDoc c v <+> endl
    where endl = if newLn then text "<<" <+> text "std::endl" else empty
printDoc' c (File f) newLn _ v = valueDoc c f <+> text "<<" <+> valueDoc c v <+> endl
    where endl = if newLn then text "<<" <+> text "std::endl" else empty
    
methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c ft@(Header) m f = transDecLine c ft m f
methodDoc' c ft@(Source) m f@(Method _ _ _ _ _ b) = vcat [
    transDecLine c ft m f <+> lbrace,
    oneTab $ bodyDoc c b,
    rbrace]
methodDoc' c ft@(Source) m f@(MainMethod _) = methodDocD' c ft m f
methodDoc' c ft m f = methodDocD c ft m f

functionDoc' :: Config -> FileType -> Label -> Method -> Doc
functionDoc' c Header _ (Method n _ _ t ps _) = methodTypeDoc c t <+> text n <> parens (paramListDoc c ps) <> endStatement c
functionDoc' c Source _ (Method n _ _ t ps b) = vcat [
    methodTypeDoc c t <+> text n <> parens (paramListDoc c ps) <+> lbrace,
    oneTab $ bodyDoc c b,
    rbrace]
functionDoc' c ft m f = methodDoc c ft m f

methodListDoc' :: Config -> FileType -> Label -> [Method] -> Doc
methodListDoc' c f@(Header) m fs = vmap (methodDoc c f m) fs
methodListDoc' c f m fs = methodListDocD c f m fs

valueDoc' :: Config -> Value -> Doc
valueDoc' _ (EnumElement _ e) = text e
valueDoc' c v@(Arg _) = valueDocD' c v
--valueDoc' c Input = inputFunc c <> dot <> text "ignore()"
--valueDoc' c (InputFile v) = valueDoc c v <> dot <> text "ignore()"
valueDoc' c (StateObj _ t vs) = stateType c t Def <> parens (callFuncParamList c vs)
valueDoc' _ (Var _ v) = text v
valueDoc' c v = valueDocD c v

inputDoc' :: Config -> IOType -> StateType -> Maybe Value -> Doc
inputDoc' _ _ (Base (FileType _)) _ = error "File type is not valid input"
inputDoc' c io _ Nothing = inputFn c io <> dot <> text "ignore(std::numeric_limits<std::streamsize>::max(), ' ')"
inputDoc' c io (Base _) (Just v) = vcat [
    inputFn c io <+> text ">>" <+> valueDoc c v <> semi,
    inputFn c io <> dot <> text "ignore(std::numeric_limits<std::streamsize>::max(), '\\n')" 
  ]
inputDoc' c io s v = inputDocD c io s v 

complexDoc' :: Config -> Complex -> Doc
complexDoc' c (ReadLine f Nothing)  = valueDoc c f <> dot <> text "ignore(std::numeric_limits<std::streamsize>::max(), '\\n')" <> semi
complexDoc' c (ReadLine f (Just v)) = statementDoc c NoLoop (valStmt $ funcApp' "std::getline" [f, v])
complexDoc' c (ReadAll f v) = let l_line = "nextLine"
                                  v_line = var "nextLine" 
                              in
  bodyDoc c 
    [ 
      block [
        varDec l_line string,
        while (funcApp' "std::getline" [f, v_line]) (oneLiner $ valStmt $ v$.(listAppend v_line))
      ]
    ]
    
complexDoc' c (ListSlice st vnew vold b e s) = let l_temp = "temp"
                                                   v_temp = var l_temp
                                                   l_i = "i"
                                                   v_i = var l_i
                                               in
  vcat [
    blockStart c,
    oneTab $ bodyDoc c [ 
      block [
        listDec' l_temp st 0,
        for (varDecDef l_i (Base Integer) (getB b)) (v_i ?< getE e) (getS s v_i)
          (oneLiner $ valStmt $ v_temp$.(listAppend (vold$.(listAccess v_i)))),
        vnew &= v_temp
      ] 
    ],
    blockEnd c
  ]
  where  getB Nothing = litInt 0
         getB (Just n) = n
         getE Nothing = vold$.listSize
         getE (Just n) = n
         getS Nothing v = (&++) v
         getS (Just n) v = v &+= n
complexDoc' c (StringSplit vnew s d) =    let l_ss = "ss"
                                              v_ss = var "ss"
                                              l_word = "word"
                                              v_word = var "word" 
                                          in
  vcat [
    blockStart c,
    oneTab $ bodyDoc c [ 
      block [
        valStmt $ vnew$.(Func "clear" []),
        DeclState $ VarDec l_ss (Type "std::stringstream"),
        valStmt $ objMethodCall v_ss "str" [s],
        varDec l_word string,
        while (funcApp' "std::getline" [v_ss, v_word, litChar d]) (oneLiner $ valStmt $ vnew$.(listAppend v_word))
      ] 
    ],
    blockEnd c
  ]
  
  
ioDoc' :: Config -> IOSt -> Doc
ioDoc' c (OpenFile f n m) = valueDoc c f <> dot <> text "open" <> (parens $ valueDoc c n <> text ", " <> modeType m) <> semi
  where modeType Read = text "std::fstream::in"
        modeType Write = text "std::fstream::out | std::fstream::app"
ioDoc' c io = ioDocD c io
  
----------------------
-- Helper Functions --
----------------------
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

destructor :: Config -> Label -> [StateVar] -> Method
destructor _ n vs =
    let checkDelPriority s@(StateVar _ _ _ _ del) | del < alwaysDel   = []
                                                  | otherwise         = [s]
        deleteVars = concatMap checkDelPriority vs
        deleteLoops = concatMap (\v@(StateVar _ _ _ t _) -> case t of List _ _ -> [v]
                                                                      _  -> []) deleteVars
        i = "i"
        guard l = var i ?< (l $. ListSize)
        loopBody l = oneLiner $ FreeState (l $. at i)
        initv = (i &.= litInt 0)
        deleteLoop l = IterState (For initv (guard l) (i &.++) (loopBody l))
        deleteVar (StateVar lbl _ _ (List _ _) _) = deleteLoop (var lbl)
        deleteVar (StateVar lbl _ _ _ _) = FreeState $ var lbl
        deleteStatements = map deleteVar deleteVars
        loopIndexDec | null deleteLoops = []
                     | otherwise = [varDec i $ Base Integer]
        dbody = [ Block $ loopIndexDec ++ deleteStatements ]
    in pubMethod Void ('~':n) [] dbody

transDecLine :: Config -> FileType -> Label -> Method -> Doc
transDecLine c (Header) _ (Method n _ _ t ps _) | isDtor n = text n <> parens (paramListDoc c ps) <> endStatement c
                                                | otherwise = methodTypeDoc c t <+> ({- listRef <> -} text n <> parens (paramListDoc c ps) <> endStatement c)
  --  where listRef = case t of (MState (List _ _)) -> text "&"
    --                          _           -> empty
transDecLine c (Source) m (Method n _ _ t ps _) = ttype <+> ({- listRef <> -} text m <> doubleColon <> text n <> parens (paramListDoc c ps))
    where doubleColon = if null m then empty else colon <> colon
          ttype | isDtor n = empty
                | otherwise = methodTypeDoc c t
transDecLine c ft m f = transDecLine c ft m $ convertToMethod f

usingNameSpace :: Config -> String -> Maybe String -> Doc
usingNameSpace c n (Just m) = text "using" <+> text n <> colon <> colon <> text m <> endStatement c
usingNameSpace c n Nothing  = text "using namespace" <+> text n <> endStatement c


inputFn :: Config -> IOType -> Doc
inputFn c Console = inputFunc c
inputFn c (File f) = valueDoc c f
