-- | The logic to render C# code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer (
    -- * C# Code Configuration -- defines syntax of all C# code
    csharpConfig
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST hiding (body,comment,bool,int,float,char)
import Language.Drasil.Code.Imperative.LanguageRenderer
import Language.Drasil.Code.Imperative.Helpers (oneTab, vibmap)

import Prelude hiding (print)
import Text.PrettyPrint.HughesPJ

csharpConfig :: Options -> Config -> Config
csharpConfig _ c = 
    Config {
        renderCode = renderCode' c,
        
        argsList         = text "args",
        bitArray         = text "BitArray",
        commentStart     = doubleSlash,
        endStatement     = semi,
        enumsEqualInts   = False,
        ext              = ".cs",
        dir              = "csharp",
        fileName         = fileNameD c,
        include          = includeD "using",
        includeScope     = scopeDoc c,
        inherit          = colon,
        inputFunc        = text "Console.ReadLine()",
        iterForEachLabel = text "foreach",
        iterInLabel      = text "in",
        list             = \_ -> text "List",
        listObj          = new,
        clsDec           = classDec,
        package          = namespaceD,
        printFunc        = text "Console.Write",
        printLnFunc      = text "Console.WriteLine",
        printFileFunc    = \f -> valueDoc c f <> dot <> text "Write",
        printFileLnFunc  = \f -> valueDoc c f <> dot <> text "WriteLine",
        stateType        = csstateType c,
        
        blockStart = lbrace, blockEnd = rbrace, 
        ifBodyStart = blockStart c, elseIf = text "else if",
        
        top    = cstop c,
        body   = csbody c,
        bottom = \_ -> empty,
        
        assignDoc = assignDocD c, binOpDoc = binOpDoc', bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamListD c,
        conditionalDoc = conditionalDocD c, declarationDoc = declarationDocD c, enumElementsDoc = enumElementsDocD c, exceptionDoc = exceptionDocD c, exprDoc = exprDocD'' c, funcAppDoc = funcAppDocD c,
        funcDoc = funcDoc' c, iterationDoc = iterationDocD c, litDoc = litDocD,
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDocD c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDocD c, paramDoc = paramDocD c, paramListDoc = paramListDocD c, patternDoc = patternDocD c, printDoc = printDocD c, retDoc = retDocD c, scopeDoc = scopeDocD,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDocD c,
        methodListDoc = methodListDocD c, methodTypeDoc = methodTypeDocD c, unOpDoc = unOpDoc', valueDoc = valueDoc' c,
        ioDoc = ioDoc' c,inputDoc = inputDoc' c,
        functionDoc = functionDocD c, functionListDoc = functionListDocD c,
        complexDoc = complexDoc' c,
        getEnv = \_ -> error "getEnv not implemented in CSharp (yet)"
    }

-- short names, packaged up above (and used below)
renderCode' :: Config -> AbstractCode -> Code
renderCode' c (AbsCode p) = Code $ fileCode c p Source (ext c)

cstop :: Config -> FileType -> Label -> Module -> Doc
cstop c _ _ _ = vcat [
    include c "System" <> endStatement c,
    include c "System.IO" <> endStatement c,
    include c "System.Collections" <> endStatement c,
    include c "System.Collections.Generic" <> endStatement c]

csbody :: Config -> a -> Label -> Module -> Doc
csbody c _ p m = let cs = classes (convToClass m)
  in
    vcat [
    package c p <+> lbrace,
    oneTab $ vibmap (classDoc c Source p) cs,
    rbrace]

csstateType :: Config -> StateType -> DecDef -> Doc 
csstateType _ (Base (FileType Read)) _ = text "StreamReader"
csstateType _ (Base (FileType Write)) _ = text "StreamWriter"
csstateType _ (Base Float) _ = text "double"
csstateType c s d  = stateTypeD c s d
   
    
-- code doc functions
valueDoc' :: Config -> Value -> Doc
valueDoc' c (FuncApp (Just l) n vs) = funcAppDoc c (l ++ "." ++ n) vs
valueDoc' c v = valueDocD c v

funcDoc' :: Config -> Function -> Doc
funcDoc' c (ListAppend v) = dot <> funcAppDoc c "Add" [v]
funcDoc' c f = funcDocD c f

binOpDoc' :: BinaryOp -> Doc
binOpDoc' Power = text "Math.Pow"
binOpDoc' op = binOpDocD op

unOpDoc' :: UnaryOp -> Doc
unOpDoc' SquareRoot = text "Math.Sqrt"
unOpDoc' Abs = text "Math.Abs"
unOpDoc' Exp = text "Math.Exp"
unOpDoc' Log = text "Math.Log"
unOpDoc' op = unOpDocD op

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v (Cast (Base Float) (Base String)) = funcAppDoc c "Double.Parse" [v]
objAccessDoc' c v (ListExtend t) = valueDoc c v <> dot <> text "Add" <> parens (dftVal)
    where dftVal = case t of Base bt     -> valueDoc c (defaultValue bt)
                             List lt t'  -> new <+> stateType c (List lt t') Dec <> parens (empty)
                             _           -> error $ "ListExtend does not yet support list type " ++ render (doubleQuotes $ stateType c t Def)
objAccessDoc' c v f = objAccessDocD c v f

ioDoc' :: Config -> IOSt -> Doc
ioDoc' c (OpenFile f n m) = valueDoc c f <+> equals <+> new <+> modeStr m <> parens (valueDoc c n) <> semi
  where modeStr Read = text "StreamReader"
        modeStr Write = text "StreamWriter" 
ioDoc' c (CloseFile f) = statementDoc c NoLoop (valStmt $ objMethodCall f "Close" [])        
ioDoc' c io = ioDocD c io

inputDoc' :: Config -> IOType -> StateType -> Maybe Value -> Doc
inputDoc' c io _ Nothing = inputFn c io
inputDoc' c io (Base Integer) (Just v) = valueDoc c v <+> equals <+> text "Int32.Parse" <> parens (inputFn c io)
inputDoc' c io (Base Float) (Just v) = valueDoc c v <+> equals <+> text "Double.Parse" <> parens (inputFn c io)
inputDoc' c io (Base String) (Just v) = valueDoc c v <+> equals <+> parens (inputFn c io)
inputDoc' _ _ (Base (FileType _)) _ = error "File type is not valid input"
inputDoc' c io s v = inputDocD c io s v 

complexDoc' :: Config -> Complex -> Doc
complexDoc' c (ReadLine f Nothing) = statementDoc c NoLoop (valStmt $ f$.(Func "ReadLine" []))
complexDoc' c (ReadLine f (Just v)) = statementDoc c NoLoop (v &= f$.(Func "ReadLine" []))
complexDoc' c (ReadAll f v) = 
  bodyDoc c $ oneLiner $
    while ((?!) f$->(var "EndOfStream")) (oneLiner $ valStmt $ v$.(listAppend $ f$.(Func "ReadLine" [])))

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
complexDoc' c (StringSplit vnew s d) =  valueDoc c vnew <+> equals <+> new 
  <+> stateType c (List Dynamic string) Dec <> parens (valueDoc c s <> dot <> funcAppDoc c "Split" [litChar d]) <> semi

-- helpers

inputFn :: Config -> IOType -> Doc
inputFn c Console = inputFunc c
inputFn c (File f) = valueDoc c f <> dot <> text "ReadLine()"