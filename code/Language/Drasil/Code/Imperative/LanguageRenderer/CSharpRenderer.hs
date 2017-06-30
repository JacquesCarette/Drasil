-- | The logic to render C# code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer (
    -- * C# Code Configuration -- defines syntax of all C# code
    csharpConfig
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST hiding (comment,bool,int,float,char)
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
        fileName         = \_ ns -> head ns,
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
        printFileFunc    = \_ -> error "not implemented",
        printFileLnFunc  = \_ -> error "not implemented",
        stateType        = stateTypeD c,
        
        blockStart = lbrace, blockEnd = rbrace, 
        ifBodyStart = blockStart c, elseIf = text "else if",
        
        top    = cstop c,
        body   = csbody c,
        bottom = \_ -> empty,
        
        assignDoc = assignDocD c, binOpDoc = binOpDoc', bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamListD c,
        conditionalDoc = conditionalDocD c, declarationDoc = declarationDocD c, enumElementsDoc = enumElementsDocD c, exceptionDoc = exceptionDocD c, exprDoc = exprDocD'' c, funcAppDoc = funcAppDocD c,
        funcDoc = funcDocD c, iterationDoc = iterationDocD c, litDoc = litDocD,
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDocD c, objAccessDoc = objAccessDocD c,
        objVarDoc = objVarDocD c, paramDoc = paramDocD c, paramListDoc = paramListDocD c, patternDoc = patternDocD c, printDoc = printDocD c, retDoc = retDocD c, scopeDoc = scopeDocD,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDocD c,
        methodListDoc = methodListDocD c, methodTypeDoc = methodTypeDocD c, unOpDoc = unOpDoc', valueDoc = valueDocD c,
        ioDoc = ioDocD c,inputDoc = inputDocD c,
        functionDoc = functionDocD c, functionListDoc = functionListDocD c,
        complexDoc = complexDocD c,
        getEnv = \_ -> error "getEnv not implemented in CSharp (yet)"
    }

-- short names, packaged up above (and used below)
renderCode' :: Config -> [Label] -> AbstractCode -> Code
renderCode' c ms (AbsCode p) = Code $ fileCode c p ms Source (ext c)

cstop :: Config -> FileType -> Label -> Module -> Doc
cstop c _ _ _ = vcat [
    include c "System" <> endStatement c,
    include c "System.Collections" <> endStatement c,
    include c "System.Collections.Generic" <> endStatement c]

csbody :: Config -> a -> Label -> Module -> Doc
csbody c _ p (Mod _ _ _ _ cs) =
    vcat [
    package c p <+> lbrace,
    oneTab $ vibmap (classDoc c Source p) cs,
    rbrace]

-- code doc functions
binOpDoc' :: BinaryOp -> Doc
binOpDoc' Power = text "Math.Pow"
binOpDoc' op = binOpDocD op

unOpDoc' :: UnaryOp -> Doc
unOpDoc' SquareRoot = text "Math.Sqrt"
unOpDoc' Abs = text "Math.Abs"
unOpDoc' op = unOpDocD op
