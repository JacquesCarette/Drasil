-- | The logic to render Java code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    javaConfig
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST hiding (body,comment,bool,int,float,char)
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
  objAccessDocD, iterationDocD, funcDocD, declarationDocD', assignDocD, stateTypeD, fileCode,
  functionListDocD, methodTypeDocD, unOpDocD, statementDocD, scopeDocD, stateDocD, stateListDocD,
  doubleSlash, retDocD, patternDocD, clsDecListDocD, clsDecDocD, funcAppDocD, enumElementsDocD,
  exprDocD', litDocD, conditionalDocD'', callFuncParamListD, bodyDocD, blockDocD, binOpDocD,
  classDec, namespaceD, includeD, fileNameD, cpplist, new, exprDocD'', declarationDocD,
  typeOfLit, functionDocD, printDocD, objVarDocD, classDocD, forLabel, javalist)
import Language.Drasil.Code.Imperative.Helpers (blank,angles,oneTab,vibmap)

import Prelude hiding (break,print)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals, comma,
  semi, vcat, lbrace, rbrace, doubleQuotes, render, colon)

validListTypes :: [Label]
validListTypes = ["ArrayList", "LinkedList", "Vector"]

javaConfig :: Options -> Config -> Config
javaConfig options c = 
    let listType = case (javalist options) of Nothing -> "Vector"
                                              Just lt -> if lt `elem` validListTypes then lt
                                                         else error $ "Unsupported Java list type specified in config file: " ++ lt ++ "\nSupported types are: " ++ show validListTypes
    in Config {
        renderCode = renderCode' c,
        
        argsList         = text "args",
        bitArray         = text "BitSet",
        commentStart     = doubleSlash,
        endStatement     = semi,
        enumsEqualInts   = False,
        ext              = ".java",
        dir              = "java",
        fileName         = fileNameD c,
        include          = includeD "import",
        includeScope     = (scopeDoc c),
        inherit          = text "extends",
        inputFunc        = parens(text "new Scanner(System.in)"),
        iterForEachLabel = forLabel,
        iterInLabel      = colon,
        list             = \_ -> text listType,
        listObj          = new,
        clsDec           = classDec,
        package          = package',
        printFunc        = text "System.out.print",
        printLnFunc      = text "System.out.println",
        printFileFunc    = \f -> valueDoc c f <> dot <> text "print",
        printFileLnFunc  = \f -> valueDoc c f <> dot <> text "println",
        stateType        = jstateType c,
        
        blockStart = lbrace, blockEnd = rbrace, 
        ifBodyStart = blockStart c, elseIf = text "else if",
        
        top    = jtop c,
        body   = jbody c,
        bottom = \_ -> empty,
        
        assignDoc = assignDocD c, binOpDoc = binOpDoc', bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamListD c,
        conditionalDoc = conditionalDocD'' c, declarationDoc = declarationDoc' c, enumElementsDoc = enumElementsDocD c, exceptionDoc = exceptionDoc' c, exprDoc = exprDoc' c, funcAppDoc = funcAppDocD c,
        funcDoc = funcDoc' c, iterationDoc = iterationDocD c, litDoc = litDocD,
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDocD c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDocD c, paramDoc = paramDocD c, paramListDoc = paramListDocD c, patternDoc = patternDocD c, printDoc = printDocD c, retDoc = retDocD c, scopeDoc = scopeDocD,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDoc' c,
        methodListDoc = methodListDocD c, methodTypeDoc = methodTypeDocD c, unOpDoc = unOpDoc', valueDoc = valueDoc' c,
        ioDoc = ioDoc' c,inputDoc = inputDoc' c,
        functionDoc = functionDocD c, functionListDoc = functionListDocD c,
        complexDoc = complexDoc' c,
        getEnv = \_ -> error "no environment has been set"
    }

-- short names, packaged up above (and used below)
renderCode' :: Config -> AbstractCode -> Code
renderCode' c (AbsCode p) = Code $ fileCode c p Source (ext c)

package' :: Label -> Doc
package' n = text "package" <+> text n

jstateType :: Config -> StateType -> DecDef -> Doc
jstateType c s@(List lt t) d = case t of Base Integer -> list c lt <> angles (text "Integer")
                                         Base Float   -> list c lt <> angles (text "Double")
                                         _            -> stateTypeD c s d
jstateType _ (Base String) _ = text "String"
jstateType _ (Base Float) _ = text "double"
jstateType _ (Base (FileType Read)) _ = text "Scanner"
jstateType _ (Base (FileType Write)) _ = text "PrintWriter"
jstateType c s d = stateTypeD c s d

jtop :: Config -> FileType -> Label -> Module -> Doc
jtop c _ p _ = vcat [
    package c p <> (endStatement c),
    blank,
    include c "java.util.Arrays" <> endStatement c,
    include c "java.util.BitSet" <> endStatement c,     --TODO: only include these if they are used in the code?
    include c "java.util.Scanner" <> endStatement c,
    include c "java.io.PrintWriter" <> endStatement c,
    include c "java.io.File" <> endStatement c,
    include c ("java.util." ++ render (list c Dynamic)) <> endStatement c
    ]

jbody :: Config -> a -> Label -> Module -> Doc
jbody c _ p m = let cs = classes (convToClass m)
  in 
    vibmap (classDoc c Source p) cs

-- code doc functions
valueDoc' :: Config -> Value -> Doc
valueDoc' c (FuncApp (Just l) n vs) = funcAppDoc c (l ++ "." ++ n) vs
valueDoc' c v = valueDocD c v

binOpDoc' :: BinaryOp -> Doc
binOpDoc' Power = text "Math.pow"
binOpDoc' op = binOpDocD op

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' c (ListDecValues lt n t vs) = stateType c (List lt t) Dec <+> text n <+> equals <+> new <+> stateType c (List lt t) Dec <> parens (listElements)
    where listElements = if null vs then empty else text "Arrays.asList" <> parens (callFuncParamList c vs)
declarationDoc' c (ConstDecDef n l) = text "final" <+> stateType c (Base $ typeOfLit l) Dec <+> text n <+> equals <+> litDoc c l
declarationDoc' c d = declarationDocD c d

exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' c (Throw s) = text "throw new" <+> text "Exception" <> parens (litDoc c $ LitStr s)
exceptionDoc' c (TryCatch tryB cb) = vcat [
    text "try" <+> lbrace,
    oneTab $ bodyDoc c tryB,
    rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> lbrace,
    oneTab $ bodyDoc c cb,
    rbrace]

exprDoc' :: Config -> Expression -> Doc
exprDoc' c (BinaryExpr v1 Equal v2@(Lit (LitStr _))) = objAccessDoc c v1 $ Func "equals" [v2]
exprDoc' c (Exists (ObjAccess v (ListAccess i))) = exprDoc c $ BinaryExpr (var $ render (valueDoc c v) ++ ".length") Greater i
exprDoc' c e = exprDocD'' c e

funcDoc' :: Config -> Function -> Doc
funcDoc' c (IndexOf v) = dot <> funcAppDoc c "indexOf" [v]
funcDoc' c ListSize = dot <> funcAppDoc c "size" []
funcDoc' c f@(ListAccess (EnumVar _)) = funcDocD c f
funcDoc' c f@(ListAccess (EnumElement _ _)) = funcDocD c f
funcDoc' c f@(ListAccess (ObjAccess (ListVar _ (EnumType _)) (ListAccess _))) = funcDocD c f
funcDoc' c (ListAccess i) = dot <> funcAppDoc c "get" [i]
funcDoc' c (ListAdd i v) = dot <> funcAppDoc c "add" [i, v]
funcDoc' c (ListAppend v) = dot <> funcAppDoc c "add" [v]
funcDoc' c f@(ListSet (EnumVar _) _) = funcDocD c f
funcDoc' c f@(ListSet (EnumElement _ _) _) = funcDocD c f
funcDoc' c (ListSet i v) = dot <> funcAppDoc c "set" [i, v]
funcDoc' c f = funcDocD c f

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v@(EnumVar _) (Cast (Base Integer) _) = valueDoc c v <> funcDoc c (Func "ordinal" [])
objAccessDoc' c v@(EnumElement _ _) (Cast (Base Integer) _) = valueDoc c v <> funcDoc c (Func "ordinal" [])
objAccessDoc' c v@(ObjAccess (ListVar _ (EnumType _)) (ListAccess _)) (Cast (Base Integer) _) = valueDoc c v <> funcDoc c (Func "ordinal" [])
objAccessDoc' c v Floor = funcAppDoc c "Math.floor" [v]
objAccessDoc' c v Ceiling = funcAppDoc c "Math.ceil" [v]
objAccessDoc' c v (Cast (Base Float) (Base String)) = funcAppDoc c "Double.parseDouble" [v]
objAccessDoc' c v (ListExtend t) = valueDoc c v <> dot <> text "add" <> parens (dftVal)
    where dftVal = case t of Base bt     -> valueDoc c (defaultValue bt)
                             List lt t'  -> new <+> stateType c (List lt t') Dec <> parens (empty)
                             _           -> error $ "ListExtend does not yet support list type " ++ render (doubleQuotes $ stateType c t Def)
objAccessDoc' c v f = objAccessDocD c v f

methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c _ _ (Method n s p t ps b) = vcat [
    --scopeDoc c s <+> perm p <> methodTypeDoc c t <+> text n <> parens (paramListDoc c ps) <> throwState (checkExceptions b) <+> lbrace,
    scopeDoc c s <+> perm p <> methodTypeDoc c t <+> text n <> parens (paramListDoc c ps) <+> text "throws Exception" <+> lbrace,
    oneTab $ bodyDoc c b,  -- all methods throw exception for now,  FIX ME.
    rbrace]
  where perm Dynamic = empty
        perm Static  = text "static "
        --throwState False = empty
        --throwState True  = text " throws Exception"
methodDoc' c _ _ (MainMethod b) = vcat [
    scopeDoc c Public <+> text "static" <+> methodTypeDoc c Void <+> text "main" <> parens (text "String[] args") 
      <+> text "throws Exception" <+> lbrace,  -- main throws exceptions for now, need to fix!
    oneTab $ bodyDoc c b,
    rbrace]
methodDoc' c f m t = methodDocD c f m t

unOpDoc' :: UnaryOp -> Doc
unOpDoc' SquareRoot = text "Math.sqrt"
unOpDoc' Abs = text "Math.abs"
unOpDoc' Log = text "Math.log"
unOpDoc' Exp = text "Math.exp"
unOpDoc' op = unOpDocD op

ioDoc' :: Config -> IOSt -> Doc
ioDoc' c (OpenFile f n Read) = valueDoc c f <+> equals <+> new <+> text "Scanner" <> parens (new <+> text "File" <> parens (valueDoc c n)) <> semi
ioDoc' c (OpenFile f n Write) = valueDoc c f <+> equals <+> new <+> text "PrintWriter" <> parens (valueDoc c n) <> semi
ioDoc' c io = ioDocD c io

inputDoc' :: Config -> IOType -> StateType -> Maybe Value -> Doc
inputDoc' c io _ Nothing = inputFn c io <> dot <> text "next()"
inputDoc' c io (Base t) (Just v) = valueDoc c v <+> equals <+> typeFunc t <> parens (inputFn c io <> dot <> text "nextLine()") 
  where  typeFunc Integer = text "Integer.parseInteger"
         typeFunc Float   = text "Double.parseDouble"
         typeFunc Boolean = text "nextBoolean()"
         typeFunc String  = empty
         typeFunc _       = error "Invalid Java input type"
inputDoc' c io s v = inputDocD c io s v 

complexDoc' :: Config -> Complex -> Doc
complexDoc' c (ReadLine f Nothing) = statementDoc c NoLoop (valStmt $ f$.(Func "nextLine" []))
complexDoc' c (ReadLine f (Just v)) = statementDoc c NoLoop (v &= f$.(Func "nextLine" []))
complexDoc' c (ReadAll f v) = statementDoc c NoLoop $
  while (f$.(Func "hasNextLine" [])) (oneLiner $ valStmt $ v$.(listAppend $ f$.(Func "nextLine" [])))    
complexDoc' c (ListSlice st vnew vold b e s) = let l_temp = "temp"
                                                   v_temp = var l_temp
                                                   l_i = "i_temp"
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
complexDoc' c (StringSplit vnew s d) = valueDoc c vnew <+> equals <+> new 
  <+> stateType c (List Dynamic string) Dec <> parens (funcAppDoc c "Arrays.asList" [s$.(Func "split" [litString [d]])]) 
  <> semi

--helpers
inputFn :: Config -> IOType -> Doc
inputFn c Console = inputFunc c
inputFn c (File f) = valueDoc c f

-- check if method throws
-- FIXME: The following code calls itself recursively, and isn't used at all or exported.
{-checkExceptions :: Body -> Bool
checkExceptions b = foldl (||) False $ map checkExcBlock b

checkExcBlock :: Block -> Bool
checkExcBlock (Block s) = checkExc s

checkExc :: [Statement] -> Bool
checkExc s = foldl (||) False $ map checkExc' s

checkExc' :: Statement -> Bool
checkExc' (CondState (If vb b)) = checkExcVB vb || checkExceptions b
checkExc' (CondState (Switch _ vb b)) = checkExcVB vb || checkExceptions b
checkExc' (IterState (For _ _ _ b)) = checkExceptions b
checkExc' (IterState (ForEach _ _ b)) = checkExceptions b
checkExc' (IterState (While _ b)) = checkExceptions b
checkExc' (ExceptState _) = True
checkExc' (IOState _) = True
checkExc' (MultiState s) = checkExc s 
checkExc' _ = False
               
checkExcVB :: [(a, Body)] -> Bool
checkExcVB vb = foldl (||) False $ map (\(_, b) -> checkExceptions b) vb-}
