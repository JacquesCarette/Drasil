-- | The logic to render C# code from an 'AbstractCode' is contained in this module
module GOOL.CodeGeneration.LanguageRenderer.CSharpRenderer (
    -- * C# Code Configuration -- defines syntax of all C# code
    csharpConfig
) where

import GOOL.Code (Code(..))
import GOOL.CodeGeneration.AbstractCode hiding (comment,bool,int,float,char)
import GOOL.CodeGeneration.LanguageRenderer
import GOOL.Auxil.Printing (angles,blank,oneTab,oneTabbed)
import GOOL.Auxil.DataTypes (Label,Options)
import GOOL.Auxil.Helper (vimap,vibmap)

import Data.List (intersperse)
import Prelude hiding (print)
import Text.PrettyPrint.HughesPJ

csharpConfig :: Options -> Config -> Config
csharpConfig options c = 
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
        methodListDoc = methodListDocD c, methodTypeDoc = methodTypeDocD c, unOpDoc = unOpDoc', valueDoc = valueDocD c
    }

-- short names, packaged up above (and used below)
renderCode' :: Config -> [Label] -> AbstractCode -> Code
renderCode' c ms (AbsCode p) = Code [fileCode c p [m] Source (ext c) | m <- ms]

cstop :: Config -> a -> b -> Doc
cstop c _ _ = vcat [
    include c "System" <> endStatement c,
    include c "System.Collections" <> endStatement c,
    include c "System.Collections.Generic" <> endStatement c]

csbody :: Config -> a -> Label -> [Class] -> Doc
csbody c _ p ms = vcat [
    package c p <+> lbrace,
    oneTab $ vibmap (classDoc c Source p) ms,
    rbrace]

-- code doc functions
binOpDoc' :: BinaryOp -> Doc
binOpDoc' Power = text "Math.Pow"
binOpDoc' op = binOpDocD op

unOpDoc' :: UnaryOp -> Doc
unOpDoc' SquareRoot = text "Math.Sqrt"
unOpDoc' Abs = text "Math.Abs"
unOpDoc' op = unOpDocD op
