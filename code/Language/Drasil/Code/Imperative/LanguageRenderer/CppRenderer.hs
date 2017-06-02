-- | The logic to render C++ code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer (
    -- * C++ Code Configuration -- defines syntax of all C++ code
    cppConfig
) where

import Language.Drasil.Config(splitSource)
import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST
  hiding (comment,bool,int,float,char,tryBody,catchBody,initState,guard,update)
import Language.Drasil.Code.Imperative.LanguageRenderer
import Language.Drasil.Code.Imperative.Helpers (blank,
                                                oneTab,oneTabbed,vmap,vibmap)

import Prelude hiding (break,print,return)
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
        fileName         = \p _ -> p,
        include          = includeD "#include",
        includeScope     = \_ -> empty,
        inherit          = colon,
        inputFunc        = text "std::cin",
        iterForEachLabel = empty,
        iterInLabel      = empty,
        list             = \_ -> text listType,
        listObj          = empty,
        clsDec           = classDec,
        package          = namespaceD,
        printFunc        = text "std::cout",
        printLnFunc      = text "std::cout",
        printFileFunc    = \f -> valueDoc c f,
        printFileLnFunc  = \f -> valueDoc c f,
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
        functionDoc = functionDocD c, functionListDoc = functionListDocD c,
        ioDoc = ioDocD c,inputDoc = inputDocD c,
        getEnv = \_ -> error "Cpp does not implement getEnv (yet)"
    }

-- for convenience
cppHeaderExt :: String
cppHeaderExt = ".hpp"
ptr, ptrAccess :: Doc
ptr = text "*"
ptrAccess = text "->"

-- short names, packaged up above (and used below)
renderCode' :: Config -> [Label] -> AbstractCode -> Code
renderCode' c ms (AbsCode p) =
    if splitSource
    then Code $ (fileCodeSplit c p ms Header cppHeaderExt) ++
                (fileCodeSplit c p ms Source (ext c))
    else Code [ fileCode c p ms Header cppHeaderExt,
                fileCode c p ms Source (ext c) ]


cppstateType :: Config -> StateType -> DecDef -> Doc
cppstateType _ (Base (FileType Read)) _    = text "ifstream"
cppstateType _ (Base (FileType Write)) _   = text "ofstream"
cppstateType _ (Base Boolean) _ = text "bool"
cppstateType _ (Type name) Dec  = text name <> ptr
cppstateType c (Iterator t) _   = text "std::" <> stateType c (List Dynamic t) Dec <> text "::iterator"
cppstateType c s d              = stateTypeD c s d

cpptop :: Config -> FileType -> Label -> [Module] -> Doc
cpptop c Header p _ = vcat [
    text "#ifndef" <+> text p <> text "_h",
    text "#define" <+> text p <> text "_h",
    blank,
    include c "<string>",
    include c $ "<" ++ render (list c Dynamic) ++ ">",
    blank,
    usingNameSpace c "std" (Just "string"),
    usingNameSpace c "std" (Just $ render (list c Dynamic)),
    usingNameSpace c "std" (Just "ifstream"),
    usingNameSpace c "std" (Just "ofstream")]
cpptop c Source p _ = vcat [          --TODO remove includes if they aren't used
    include c ("\"" ++ p ++ cppHeaderExt ++ "\""),
    include c "<algorithm>",
    include c "<iostream>",
    include c "<fstream>",
    include c "<iterator>",     --used only when printing a list
    include c "<string>",
    include c "<math.h>",       --used for Floor and Ceiling functions
    include c $ "<" ++ render (list c Dynamic) ++ ">",
    blank,
    usingNameSpace c p Nothing,
    usingNameSpace c "std" (Just "string"),
    usingNameSpace c "std" (Just $ render (list c Dynamic)),
    usingNameSpace c "std" (Just "ifstream"),
    usingNameSpace c "std" (Just "ofstream")]

cppbody :: Config -> FileType -> Label -> [Module] -> Doc
cppbody c f@(Header) p modules = let cs = foldl1 (++) (map classes modules) in
    vcat [
    package c p <+> lbrace,
    oneTabbed [
        clsDecListDoc c cs,
        blank,
        vibmap (classDoc c f p) cs],
    rbrace]
cppbody c f@(Source) p modules = let cs = foldl1 (++) (map classes modules) in
   vibmap (classDoc c f p) cs

cppbottom :: FileType -> Doc
cppbottom Header = text "#endif"
cppbottom Source = empty

-- code doc functions
assignDoc' :: Config -> Assignment -> Doc
--assignDoc' c (Assign v Input) = inputFunc c <+> text ">>" <+> valueDoc c v
--assignDoc' c (Assign v (InputFile f)) = valueDoc c f <+> text ">>" <+> valueDoc c v
assignDoc' c a = assignDocD c a

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' c (ListDec lt n t s) = stateType c (List lt t) Dec <+> text n <> parens (int s)
declarationDoc' c (ListDecValues lt n t vs) = vcat [
    stateType c t Dec <+> text temp <> text "[]" <+> equals <+> braces (callFuncParamList c vs) <> endStatement c,
    stateType c (List lt t) Dec <+> text n <> parens(text temp <> comma <+> text temp <+> text "+" <+> text "sizeof" <> parens (text temp) <+> text "/" <+> text "sizeof" <> parens (text temp <> text "[0]"))]
    where temp = n ++ "_temp"
declarationDoc' c d = declarationDocD' c d

exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' c (Throw s) = text "throw" <> parens (litDoc c $ LitStr s)
exceptionDoc' c (TryCatch tryBody catchBody) = vcat [
    text "try" <+> lbrace,
    oneTab $ bodyDoc c tryBody,
    rbrace <+> text "catch" <+> parens (text "string" <+> text "exc") <+> lbrace,
    oneTab $ bodyDoc c catchBody,
    rbrace]

funcDoc' :: Config -> Function -> Doc
funcDoc' c (Func n vs) = ptrAccess <> funcAppDoc c n vs
funcDoc' c (Get n) = ptrAccess <> funcAppDoc c (getterName n) []
funcDoc' c (Set n v) = ptrAccess <> funcAppDoc c (setterName n) [v]
funcDoc' _ (IndexOf _) = error "IndexOf function must be rendered at the ObjAccess level in C++"
funcDoc' c ListSize = dot <> funcAppDoc c "size" []
funcDoc' c (ListAccess i) = dot <> funcAppDoc c "at" [i]
funcDoc' c (ListAdd _ v) = dot <> funcAppDoc c "push_back" [v]
funcDoc' c (ListSet i v) = dot <> funcAppDoc c "at" [i] <+> equals <+> valueDoc c v
funcDoc' c f = funcDocD c f

iterationDoc' :: Config -> Iteration -> Doc
iterationDoc' c (ForEach it listVar@(ListVar _ t) b) = iterationDoc c $ For initState guard update $ bodyReplace (Var it) (Var $ "(*" ++ it ++ ")") b
    where initState = DeclState $ VarDecDef it (Iterator t) (listVar $. IterBegin)
          guard     = binExpr (Var it) NotEqual (listVar $. IterEnd)
          update    = (&.++)it
iterationDoc' c i = iterationDocD c i

classDoc' :: Config -> FileType -> Label -> Class -> Doc
classDoc' c (Header) _ (Enum n _ es) = vcat [
    text "enum" <+> text n <+> lbrace,
    oneTab $ enumElementsDoc c es,
    rbrace <> endStatement c]
classDoc' _ (Source) _ (Enum _ _ _) = empty
classDoc' c ft@(Header) _ (Class n p _ vs fs) =
    let makeTransforms = map convertToMethod
        funcs = fs ++ [destructor c n vs]
        pubFuncs = concatMap (\f@(Method _ s _ _ _) -> if s == Public then [f] else []) $ makeTransforms funcs
        pubVars = concatMap (\v@(StateVar _ s _ _ _) -> if s == Public then [v] else []) vs
        privFuncs = concatMap (\f@(Method _ s _ _ _) -> if s == Private then [f] else []) $ makeTransforms funcs
        privVars = concatMap (\v@(StateVar _ s _ _ _) -> if s == Private then [v] else []) vs
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
classDoc' _ (Header) _ (MainClass _ _ _) = empty
classDoc' c ft _ (MainClass _ vs fs) = vcat [
    stateListDoc c vs,
    stateBlank,
    methodListDoc c ft "" fs]
    where stateBlank = if null vs then empty else blank

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v (IndexOf vr) = funcAppDoc c "find" [v $. IterBegin, v $. IterEnd, vr] <+> text "-" <+> valueDoc c (v $. IterBegin)
objAccessDoc' c v   (Floor) = funcAppDoc c "floor" [v]
objAccessDoc' c v   (Ceiling) = funcAppDoc c "ceil" [v]
objAccessDoc' c v f = objAccessDocD c v f

objVarDoc' :: Config -> Value -> Value -> Doc
objVarDoc' c (Self) v = valueDoc c v
objVarDoc' c v1 v2 = valueDoc c v1 <> ptrAccess <> valueDoc c v2

paramDoc' :: Config -> Parameter -> Doc
paramDoc' c (StateParam n t@(List _ _)) = stateType c t Dec <+> text "&" <> text n
paramDoc' c p = paramDocD c p

printDoc' :: Config -> IOType -> Bool -> StateType -> Value -> Doc
printDoc' _ _ _ _ (ListVar _ (List _ _)) = error "C++: Printing of nested lists is not yet supported"
printDoc' c Console newLn _ v@(ListVar _ t) = vcat [
    statementDoc c NoLoop $ printStr "[",
    statementDoc c NoLoop $ ValState $ FuncApp Nothing "copy" [v $. IterBegin, v $. IterEnd, FuncApp Nothing iter [Var "std::cout", litString ","]],
    statementDoc c Loop $ printLastStr "]"]
    where iter = "std::ostream_iterator<" ++ render(stateType c t Dec) ++ ">"
          printLastStr = if newLn then printStrLn else printStr
printDoc' c Console newLn _ v = printFunc c <+> text "<<" <+> valueDoc c v <+> endl
    where endl = if newLn then text "<<" <+> text "std::endl" else empty
printDoc' _ (File _) _ _ _ = empty --TODO!

methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c ft@(Header) m f = transDecLine c ft m f
methodDoc' c ft@(Source) m f@(Method _ _ _ _ b) = vcat [
    transDecLine c ft m f <+> lbrace,
    oneTab $ bodyDoc c b,
    rbrace]
methodDoc' c ft@(Source) m f@(MainMethod _) = methodDocD' c ft m f
methodDoc' c ft m f = methodDocD c ft m f

methodListDoc' :: Config -> FileType -> Label -> [Method] -> Doc
methodListDoc' c f@(Header) m fs = vmap (methodDoc c f m) fs
methodListDoc' c f m fs = methodListDocD c f m fs

valueDoc' :: Config -> Value -> Doc
valueDoc' _ (EnumElement _ e) = text e
valueDoc' c v@(Arg _) = valueDocD' c v
--valueDoc' c Input = inputFunc c <> dot <> text "ignore()"
--valueDoc' c (InputFile v) = valueDoc c v <> dot <> text "ignore()"
valueDoc' c v = valueDocD c v

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
        guard l = Var i ?< (l $. ListSize)
        loopBody l = oneLiner $ FreeState (l $. at i)
        initv = (i &.= litInt 0)
        deleteLoop l = IterState (For initv (guard l) ((&.++)i) (loopBody l))
        deleteVar (StateVar lbl _ _ (List _ _) _) = deleteLoop (Var lbl)
        deleteVar (StateVar lbl _ _ _ _) = FreeState $ Var lbl
        deleteStatements = map deleteVar deleteVars
        loopIndexDec | null deleteLoops = []
                     | otherwise = [varDec i $ Base Integer]
        dbody = [ Block $ loopIndexDec ++ deleteStatements ]
    in pubMethod Void ('~':n) [] dbody

transDecLine :: Config -> FileType -> Label -> Method -> Doc
transDecLine c (Header) _ (Method n _ t ps _) | isDtor n = text n <> parens (paramListDoc c ps) <> endStatement c
                                              | otherwise = methodTypeDoc c t <+> (listRef <> text n <> parens (paramListDoc c ps) <> endStatement c)
    where listRef = case t of (MState (List _ _)) -> text "&"
                              _           -> empty
transDecLine c (Source) m (Method n _ t ps _) = ttype <+> (listRef <> text m <> doubleColon <> text n <> parens (paramListDoc c ps))
    where doubleColon = if null m then empty else colon <> colon
          listRef = case t of (MState (List _ _)) -> text "&"
                              _           -> empty
          ttype | isDtor n = empty
                | otherwise = methodTypeDoc c t
transDecLine c ft m f = transDecLine c ft m $ convertToMethod f

usingNameSpace :: Config -> String -> Maybe String -> Doc
usingNameSpace c n (Just m) = text "using" <+> text n <> colon <> colon <> text m <> endStatement c
usingNameSpace c n Nothing  = text "using namespace" <+> text n <> endStatement c
