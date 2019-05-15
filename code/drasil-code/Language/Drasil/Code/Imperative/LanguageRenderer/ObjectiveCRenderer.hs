{-# LANGUAGE LambdaCase, PostfixOperators #-}
-- | The logic to render Objective-C code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.ObjectiveCRenderer (
    -- * Objective-C Code Configuration -- defines syntax of all Objective-C code
    objcConfig
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST 
  hiding (body,comment,bool,int,float,char,tryBody,catchBody,initState,guard,
          update,forBody)
import Language.Drasil.Code.Imperative.Build.AST (buildAll, cCompiler, nativeBinary)
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
  renderCode, argsList, Options, ioDocD, inputDocD,
  valueDocD, methodDocD, methodDocD', methodListDocD, paramDocD, 
  objAccessDocD, iterationDocD, funcDocD, declarationDocD, assignDocD, stateTypeD, fileCode,
  functionListDocD, unOpDocD, statementDocD, scopeDocD, stateDocD, stateListDocD,
  doubleSlash, retDocD, patternDocD, clsDecListDocD, clsDecDocD, enumElementsDocD,
  exprDocD', litDocD, conditionalDocD'', bodyDocD, blockDocD, binOpDocD,
  classDec, includeD, fileNameD, addDefaultCtor, fixCtorNames, complexDocD,
  functionDocD, objVarDocD, objcstaticlist, buildConfig, runnable)
import Language.Drasil.Code.Imperative.Helpers (blank,oneTab,oneTabbed,
                                            doubleQuotedText,himap,vmap,vibmap)

import Prelude hiding (break,print,return,init,(<>))
import Text.PrettyPrint.HughesPJ hiding (Str,integer)

validListTypes :: [Label]
validListTypes = ["NSArray", "NSMutableArray"]

objcConfig :: Options -> Config -> Config
objcConfig options c =
    let staticListType = case objcstaticlist options of Nothing -> "NSArray"
                                                        Just lt -> if lt `elem` validListTypes then lt
                                                                   else error $ "Unsupported Objective-C static list type specified in config file: " ++ lt ++ "\nSupported types are: " ++ show validListTypes
    in Config {
        renderCode = renderCode' c,
        
        argsList         = text "argv",
        bitArray         = empty,
        commentStart     = doubleSlash,
        endStatement     = semi,
        enumsEqualInts   = False,
        ext              = ".m",
        dir              = "obj-c",
        buildConfig      = buildAll $ \i o -> [cCompiler, unwords i, "-o", o],
        runnable         = nativeBinary,
        fileName         = fileNameD c,
        include          = includeD "#import",
        includeScope     = const $ empty,
        inherit          = colon,
        inputFunc        = text "scanf",
        iterForEachLabel = empty,
        iterInLabel      = text "in",
        list             = \case Static  -> text staticListType
                                 Dynamic -> text "NSMutableArray",
        listObj          = empty,
        clsDec           = text "@" <> classDec,
        package          = const $ empty,
        printFunc        = text "printf",
        printLnFunc      = text "printf",
        printFileFunc    = const $ empty,
        printFileLnFunc  = const $ empty,
        stateType        = objcstateType c,
        
        blockStart = lbrace, blockEnd = rbrace, 
        ifBodyStart = blockStart c, elseIf = text "else if",
        
        top    = objctop c,
        body   = objcbody c,
        bottom = const $ empty,
        
        assignDoc = assignDoc' c, binOpDoc = binOpDocD, bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamList' c,
        conditionalDoc = conditionalDocD'' c, declarationDoc = declarationDoc' c, enumElementsDoc = enumElementsDocD c, exceptionDoc = exceptionDoc' c, exprDoc = exprDoc' c, funcAppDoc = funcAppDoc' c,
        funcDoc = funcDoc' c, iterationDoc = iterationDoc' c, litDoc = litDoc',
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDoc' c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDocD c, paramDoc = paramDoc' c, paramListDoc = paramListDoc' c, patternDoc = patternDocD c, printDoc = printDoc' c, retDoc = retDocD c, scopeDoc = scopeDocD,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDoc' c,
        methodListDoc = methodListDoc' c, methodTypeDoc = methodTypeDoc' c, unOpDoc = unOpDoc', valueDoc = valueDoc' c,
        functionDoc = functionDocD c, functionListDoc = functionListDocD c,
        ioDoc = ioDocD c,inputDoc = inputDocD c,
        complexDoc = complexDocD c,
        getEnv = const $ error "getEnv not implemented (yet) in ObjC"
    }

-- for convenience
defaultInit, dealloc, objcHeaderExt, release, sagaInit :: String
defaultInit = "init"
dealloc = "dealloc"
objcHeaderExt = ".h"
release = "release"
sagaInit = "Init"

super :: Value
super = var "super"

dash, ptr, str :: Doc
dash = text "-"
ptr = text "*"
str = text "NSString"

-- short names, packaged up above (and used below)
renderCode' :: Config -> AbstractCode -> Code
renderCode' c (AbsCode p@(Pack l ms)) =
  Code $ (fileCode c (Pack l (ignoreMain ms)) Header  objcHeaderExt) ++
    (fileCode c p Source (ext c))

objcstateType :: Config -> StateType -> DecDef -> Doc
objcstateType c (List lt _) Dec    = list c lt <> ptr
objcstateType c (List lt _) _      = list c lt
objcstateType _ (Base Integer) _   = text "NSInteger"
objcstateType _ (Base Boolean) _   = text "BOOL"
objcstateType _ (Base String) Dec  = str <> ptr
objcstateType _ (Base String) _    = str
objcstateType _ (Type name) Dec    = text name <> ptr
objcstateType c s d                = stateTypeD c s d

objctop :: Config -> FileType -> Label -> Module -> Doc
objctop c Header _ _ = vcat [
    include c "<Foundation/NSObject.h>",
    include c "<Foundation/NSString.h>",
    include c "<Foundation/NSArray.h>"]
objctop c Source p _ = vcat [
    include c ("\"" ++ p ++ objcHeaderExt ++ "\""),
    blank,
    include c "<Foundation/NSObject.h>",
    include c "<Foundation/NSString.h>",
    include c "<Foundation/NSArray.h>",
    include c "<Foundation/NSValue.h>",
    include c "<Foundation/NSAutoreleasePool.h>"]

objcbody :: Config -> FileType -> Label -> Module -> Doc
objcbody c f@Header p (Mod _ _ _ _ cs) =
    vcat [
    clsDecListDoc c cs,
    blank,
    vibmap (classDoc c f p) (fixCtorNames sagaInit cs)]
objcbody c f@Source p (Mod _ _ _ _ cs) =
    vibmap (classDoc c f p) (fixCtorNames sagaInit cs)

-- code doc functions
assignDoc' :: Config -> Assignment -> Doc
--assignDoc' c (Assign v Input) = vcat [      --assumes v is NSString
--    text "char*" <+> temp <> endStatement c,
--    inputFunc c <> parens (text "\"%s\"," <+> temp) <> endStatement c,
--    valueDoc c v <+> equals <+> nsFromCString c temp]
--    where temp = text "temp"
assignDoc' = assignDocD

callFuncParamList' :: Config -> [Value] -> Doc
callFuncParamList' c = colonMapListDoc (text " : ") (valueDoc c)

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' c (ListDec lt n t s) = stateType c (List lt t) Dec <+> text n <+> equals <+> valueDoc c (StateObj Nothing (List lt t) [Lit $ LitInt $ toInteger s])
declarationDoc' c (ListDecValues lt n t vs) = stateType c (List lt t) Dec <+> text n <+> equals <+> brackets (alloc c (List lt t) <+> initList)
    where initList = if null vs then text defaultInit else text "initWithObjects" <> listInitObjectsDoc c vs
declarationDoc' c d = declarationDocD c d

exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' c (Throw s) = text "@throw" <> parens (litDoc c $ LitStr s)
exceptionDoc' c (TryCatch tryBody catchBody) = vcat [
    text "@try" <+> lbrace,
    oneTab $ bodyDoc c tryBody,
    rbrace <+> text "@catch" <+> parens (text "NSString*" <+> text "exc") <+> lbrace,
    oneTab $ bodyDoc c catchBody,
    rbrace]

exprDoc' :: Config -> Expression -> Doc
exprDoc' c (BinaryExpr v1 _ v2@(Lit (LitStr _))) = objAccessDoc c v1 $ Func "isEqual" [v2]
exprDoc' c (Exists (Arg i)) = argsListAccess c i
exprDoc' c e = exprDocD' c e

funcAppDoc' :: Config -> Label -> [Value] -> Doc
funcAppDoc' c n vs = brackets (valueDoc c Self <> innerFuncAppDoc c n vs)

funcDoc' :: Config -> Function -> Doc
funcDoc' c (Func n vs) = innerFuncAppDoc c n vs
funcDoc' c (Get n) = innerFuncAppDoc c (getterName n) []
funcDoc' c (Set n v) = innerFuncAppDoc c (setterName n) [v]
funcDoc' c (IndexOf v) = innerFuncAppDoc c "indexOfObject" [v]
funcDoc' c ListSize = innerFuncAppDoc c "count" []
funcDoc' c (ListAccess i) = innerFuncAppDoc c "objectAtIndex" [i]
funcDoc' c (ListAdd _ v) = innerFuncAppDoc c "addObject" [v]
funcDoc' c (ListSet i v) = space <> text "replaceObjectAtIndex:" <+> valueDoc c i <+> text "withObject:" <+> valueDoc c v
funcDoc' _ (ListPopulate _ _) = error "ListPopulate must be rendered at the ObjAccess level in Objective-C"
funcDoc' c f = funcDocD c f

iterationDoc' :: Config -> Iteration -> Doc
--the index is declared inside the for statement (e.g. 'for (int i = 0;...)');
--must compile the resulting files with gcc -std=c99 when doing this
iterationDoc' c (ForEach i listVar@(ListVar _ _) b) = iterationDoc c $ For initState guard update $ bodyReplace (var i) (listVar $. at i) b
    where initState = varDecDef i (Base Integer) (litInt 0)
          guard     = var i ?< (listVar $. ListSize)
          update    = (&.++)i
    -- the following ForEach implementation will only be valid in Objective-C 2.0 or later
    {-vcat [
        iterForEachLabel c <+> parens (stateType c t Dec <+> text var <+> iterInLabel c <+> valueDoc c listVar) <+> lbrace,
        oneTab $ bodyDoc c b,
        rbrace]-}
iterationDoc' c i = iterationDocD c i

litDoc' :: Literal -> Doc
litDoc' (LitBool True)  = text "YES"
litDoc' (LitBool False) = text "NO"
litDoc' (LitStr v)      = text "@" <> doubleQuotedText v
litDoc' l               = litDocD l

classDoc' :: Config -> FileType -> Label -> Class -> Doc
classDoc' c Header _ (Enum n _ es) = vcat [
    text "typedef" <+> text "enum" <+> lbrace,
    oneTab $ enumElementsDoc c es,
    rbrace <+> text n <> endStatement c]
classDoc' _ Source _ Enum{} = empty
classDoc' c ft@Header _ (Class n p _ vs fs) =
    let pubVars = concatMap (\v@(StateVar _ s _ _ _) -> [v | s == Public]) vs
        privVars = concatMap (\v@(StateVar _ s _ _ _) -> [v | s == Private]) vs
        pubScope = if null pubVars then empty else text "@" <> scopeDoc c Public
        privScope = if null privVars then empty else text "@" <> scopeDoc c Private
        pubBlank = if null pubVars then empty else blank
        funcBlank = if null fs then empty else blank
        baseClass = case p of Nothing -> text "NSObject"
                              Just pn -> text pn
    in vcat [
        text "@interface" <+> text n <> inherit c <+> baseClass <+> lbrace,
        oneTabbed [
            pubScope,
            oneTab $ stateListDoc c pubVars,
            pubBlank,
            privScope,
            oneTab $ stateListDoc c privVars
        ],
        rbrace,
        funcBlank,
        methodListDoc c ft n $ addDefaultCtor c n sagaInit fs,
        text "@end"
    ]
classDoc' c ft@Source _ (Class n _ _ vs fs) =
    let funcs = fs ++ [destructor c vs]
    in vcat [
        text "@implementation" <+> text n,
        methodListDoc c ft n $ addDefaultCtor c n sagaInit funcs,
        text "@end"
    ]
classDoc' _ Header _ MainClass{} = empty
classDoc' c ft _ (MainClass n vs fs) = vcat [
    stateListDoc c vs,
    stateBlank,
    methodListDoc c ft n fs]
    where stateBlank = if null vs then empty else blank

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v f@(Cast _ _) = objAccessDocD c v f
objAccessDoc' c v (ListPopulate size t) = iterationDoc c $ For (varDecDef i (Base Integer) (litInt 0)) (var i ?< size) (i &.++) forBody
    where i = "i"
          dftVal = case t of Base bt -> defaultValue bt
                             _       -> error $ "ListPopulate does not yet support list type " ++ render (doubleQuotes $ stateType c t Def)
          forBody = oneLiner $ ValState $ v $. ListAdd (var i) dftVal
objAccessDoc' c v Floor = text "floor" <> parens(valueDoc c v)
objAccessDoc' c v Ceiling = text "ceil" <> parens(valueDoc c v)
objAccessDoc' c v f = brackets (valueDoc c v <> funcDoc c f)

paramDoc' :: Config -> Parameter -> Doc
paramDoc' c (StateParam n t) = parens (stateType c t Dec) <+> text n
paramDoc' c p@FuncParam{} = paramDocD c p

paramListDoc' :: Config -> [Parameter] -> Doc
paramListDoc' c = colonMapListDoc (text " : ") (paramDoc c)

printDoc' :: Config -> IOType -> Bool -> StateType -> Value -> Doc    --this function assumes that the StateType and Value match up as appropriate (e.g. if the StateType is a List, then the Value should be a ListVar)
printDoc' c Console newLn t v = printFunc c <> parens (text ("\"%" ++ frmt ++ nl ++ "\",") <+> value)
    where nl = if newLn then "\\n" else ""
          value =  case t of 
                     Base String -> 
                       case v of Lit (LitStr s) -> doubleQuotedText s
                                 _              -> objAccessDoc c v (Func "UTF8String" [])
                     _ -> case v of ListVar _ _ -> objAccessDoc c (v $. Func "description" []) (Func "UTF8String" [])
                                    _ -> valueDoc c v
          frmt = case t of Base Boolean -> "B"      --TODO does B work for Booleans? Bools might not be directly printable
                           Base Integer -> "d"
                           Base Float -> "f"
                           Base Character -> "c"
                           Base String -> "s"
                           List _ _ -> "s"
                           EnumType _ -> "d"
                           _ -> error $ "Objective-C: print statement not supported for type " ++ render (stateType c t Def)
printDoc' _ (File _) _ _ _ = error "Not implemented yet!"
                           
methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' _ Header _ (MainMethod _) = empty
methodDoc' c Header _ f@(Method n _ _ _ _ _) | isDtor n  = empty
                                           | otherwise = transDecLine c f <> endStatement c 
methodDoc' c Source _ f@(Method _ _ _ (Construct _) _ b) = vcat [
    transDecLine c f <+> lbrace,
    oneTab $ bodyDoc c $ [
        Block $ AssignState (Assign Self $ ObjAccess super $ Func defaultInit []) :
                ctorPoolDec,
        Block ctorIfState,
        Block $ ctorPoolDrain ++
                [RetState (Ret Self)]
    ],
    rbrace]
    where ctorPoolDec = if null b then [] else [varDecDef "pool" (Type "NSAutoreleasePool") $ var "[[NSAutoreleasePool alloc] init]"]
          ctorIfState = if null b then [] else [CondState (If [(Self, b)] [])]
          ctorPoolDrain = if null b then [] else [ValState $ ObjAccess (var "pool") (Func "drain" [])]
methodDoc' c Source _ f@(Method _ _ _ _ _ b) = vcat [
    transDecLine c f <+> lbrace,
    oneTab $ bodyDoc c b,
    rbrace]
methodDoc' c ft@Source m (MainMethod b) = methodDocD' c ft m $ MainMethod poolB
    where poolB = poolDec ++ b ++ poolDrain
          poolDec = if null b then [] else oneLiner $ varDecDef "pool" (Type "NSAutoreleasePool") $ var "[[NSAutoreleasePool alloc] init]"
          poolDrain = if null b then [] else oneLiner $ ValState $ ObjAccess (var "pool") (Func "drain" [])
methodDoc' c ft m f = methodDocD c ft m f

methodTypeDoc' :: Config -> MethodType -> Doc
methodTypeDoc' c (MState t) = dash <> parens (stateType c t Dec)
methodTypeDoc' _ Void = dash <> parens (text "void")
methodTypeDoc' _ (Construct m) = dash <> parens (text m <> ptr)

methodListDoc' :: Config -> FileType -> Label -> [Method] -> Doc
methodListDoc' c f@Header m fs = vmap (methodDoc c f m) fs
methodListDoc' c f m fs = methodListDocD c f m fs

unOpDoc' :: UnaryOp -> Doc
unOpDoc' Abs = text "ABS"
unOpDoc' op = unOpDocD op

valueDoc' :: Config -> Value -> Doc
valueDoc' _ (EnumElement _ e) = text e
--NSArrays of base types like bools are not allowed, so we store them as an array of NSNumbers instead and convert to base-type values when accessing the list
valueDoc' c (ObjAccess v@(ListVar _ t) f@(ListAccess _)) = getValueDoc t $ objAccessDoc c v f
valueDoc' c (ObjAccess v@(ObjVar _ (ListVar _ t)) f@(ListAccess _)) = getValueDoc t $ objAccessDoc c v f
valueDoc' c (ObjAccess v@(ListVar _ t) f@(ListAdd _ e)) = objAccessDoc c v $ funcReplace e (numFrom c t e) f
valueDoc' c (ObjAccess v@(ObjVar _ (ListVar _ t)) f@(ListAdd _ e)) = objAccessDoc c v $ funcReplace e (numFrom c t e) f
valueDoc' c (ObjAccess v@(ListVar _ t) f@(ListSet _ e)) = objAccessDoc c v $ funcReplace e (numFrom c t e) f
valueDoc' c (ObjAccess v@(ObjVar _ (ListVar _ t)) f@(ListSet _ e)) = objAccessDoc c v $ funcReplace e (numFrom c t e) f
valueDoc' c (ObjAccess v f@(ListAdd _ e@(Var _ _))) = vcat [
    objAccessDoc c v f <> endStatement c,
    objAccessDoc c e (Func release [])]
valueDoc' _ Self = text "self"
valueDoc' c (StateObj _ t@(List lt _) [s]) = brackets (alloc c t <> innerFuncAppDoc c init size)
    where init = case lt of Static  -> defaultInit
                            Dynamic -> "initWithCapacity"
          size = case lt of Static  -> []
                            Dynamic -> [s]
valueDoc' c (StateObj _ t@(List _ _) _) = brackets (alloc c t <> innerFuncAppDoc c defaultInit [])
valueDoc' c (StateObj _ t vs) = brackets (funcDoc c (Cast t t) <+> alloc c t <> innerFuncAppDoc c sagaInit vs) -- cast needs fixing
valueDoc' c (Arg i) = nsFromCString c $ argsListAccess c i
--valueDoc' c Input = inputFunc c <> parens (text "\"%*s\"")
valueDoc' c v = valueDocD c v

----------------------
-- Helper Functions --
----------------------
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

destructor :: Config -> [StateVar] -> Method
destructor _ vs =
    let checkDelPriority s@(StateVar _ _ _ _ del) | del < 2   = []
                                                  | otherwise = [s]
        releaseVars = concatMap checkDelPriority vs
        releaseStatements = concatMap (\(StateVar lbl _ _ _ _) -> [ValState $ (var lbl $. Func release [])]) releaseVars
        releaseBlock = if null releaseVars then [] else [Block releaseStatements]
        deallocBody = releaseBlock ++ oneLiner (ValState (super $. Func dealloc []))
    in Method dealloc Public Dynamic Void [] deallocBody

alloc :: Config -> StateType -> Doc
alloc c t = brackets (stateType c t Def <> innerFuncAppDoc c "alloc" [])

argsListAccess :: Config -> Int -> Doc
argsListAccess c i = argsList c <> brackets (litDoc c $ LitInt $ toInteger $ i + 1)

-- | returns a value that gets the base-type value out of an NSNumber object
getValueDoc :: StateType -> Doc -> Doc
getValueDoc t d = let integer = "integer" in
    case t of EnumType _     -> valFrom integer
              Base Boolean   -> valFrom "bool"
              Base Integer   -> valFrom integer
              Base Float     -> valFrom "float"
              Base Character -> valFrom "char"
              _              -> d
    where valFrom typeName = brackets (d <+> text (typeName ++ "Value"))

nsFromCString :: Config -> Doc -> Doc
nsFromCString c n = brackets (alloc c string <+> text "initWithUTF8String:" <+> n)

-- | returns a value that creates an NSNumber object containing the specified value
makeNumber :: Config -> Value -> Value
makeNumber c v = case v of EnumElement _ _  -> numFrom c (Base Integer) v
                           EnumVar _        -> numFrom c (Base Integer) v
                           Lit (LitInt _)   -> numFrom c (Base Integer) v
                           Lit (LitBool _)  -> numFrom c (Base Boolean) v
                           Lit (LitFloat _) -> numFrom c (Base Float) v
                           Lit (LitChar _)  -> numFrom c (Base Character) v
                           _                -> v

numFrom :: Config -> StateType -> Value -> Value
numFrom c t v = let integer = "Integer" in
    case t of EnumType _     -> numWith integer
              Base Boolean   -> numWith "Bool"
              Base Integer   -> numWith integer
              Base Float     -> numWith "Float"
              Base Character -> numWith "Char"
              _              -> v
    where numWith n = var $ "[NSNumber numberWith" ++ n ++ ": " ++ render (valueDoc c v) ++ "]"

-- | formats a Doc string for the inner part of an Objective-C function call (message)
innerFuncAppDoc :: Config -> Label -> [Value] -> Doc
innerFuncAppDoc c n vs = space <> text n <> callFuncParamList c vs

-- | returns ": " if es is not empty, or "" if it is
listColon :: [a] -> Doc
listColon es = if null es then empty else text ": "

-- | maps s to all elements in es, then formats it into a Doc string using s as a separator
colonMapListDoc :: Doc -> (a -> Doc) -> [a] -> Doc
colonMapListDoc s f es = hcat [listColon es, himap s f es]

-- | formats a Doc string for the declarationDoc line of a function
transDecLine :: Config -> Method -> Doc
transDecLine c (Method n _ _ t ps _) = methodTypeDoc c t <+> text n <> paramListDoc c ps
transDecLine c f = transDecLine c $ convertToMethod f

-- | makes a list c of values into the correct format for initializing a list c with a set of objects
listInitObjectsDoc :: Config -> [Value] -> Doc
listInitObjectsDoc _ [] = empty
listInitObjectsDoc c vs = colonMapListDoc (text ", ") (valueDoc c) vals <> text ", nil"
    where vals    = map (makeNumber c) vs
