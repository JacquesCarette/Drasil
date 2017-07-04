-- | The logic to render Java code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    javaConfig
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST hiding (comment,bool,int,float,char)
import Language.Drasil.Code.Imperative.LanguageRenderer
import Language.Drasil.Code.Imperative.Helpers (angles,oneTab,vibmap)

import Prelude hiding (break,print)
import Text.PrettyPrint.HughesPJ

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
        fileName         = fileNameD c,
        include          = includeD "import",
        includeScope     = (scopeDoc c),
        inherit          = text "extends",
        inputFunc        = parens(text "new Scanner(System.in)") <> dot <> text "nextLine()",
        iterForEachLabel = forLabel,
        iterInLabel      = colon,
        list             = \_ -> text listType,
        listObj          = new,
        clsDec           = classDec,
        package          = package',
        printFunc        = text "System.out.print",
        printLnFunc      = text "System.out.println",
        printFileFunc    = \_ -> error "not implemented",
        printFileLnFunc  = \_ -> error "not implemented",
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
        methodListDoc = methodListDocD c, methodTypeDoc = methodTypeDocD c, unOpDoc = unOpDoc', valueDoc = valueDocD c,
        ioDoc = ioDocD c,inputDoc = inputDocD c,
        functionDoc = functionDocD c, functionListDoc = functionListDocD c,
        complexDoc = complexDocD c,
        getEnv = \_ -> error "no environment has been set"
    }

-- short names, packaged up above (and used below)
renderCode' :: Config -> AbstractCode -> Code
renderCode' c (AbsCode p) = Code $ fileCode c p Source (ext c)

package' :: Label -> Doc
package' n = text "package" <+> text n

jstateType :: Config -> StateType -> DecDef -> Doc
jstateType c s@(List lt t) d = case t of Base Integer -> list c lt <> angles (text "Integer")
                                         _            -> stateTypeD c s d
jstateType _ (Base String) _ = text "String"
jstateType c s d = stateTypeD c s d

jtop :: Config -> FileType -> Label -> Module -> Doc
jtop c _ p _ = vcat [
    package c p <> (endStatement c)
    -- blank,
    -- include c "java.util.Arrays" <> endStatement c,
    -- include c "java.util.BitSet" <> endStatement c,     --TODO: only include these if they are used in the code?
    -- include c "java.util.Scanner" <> endStatement c,
    -- include c ("java.util." ++ render (list c Dynamic)) <> endStatement c
    ]

jbody :: Config -> a -> Label -> Module -> Doc
jbody c _ p (Mod _ _ _ _ cs) =
  vibmap (classDoc c Source p) cs

-- code doc functions
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
exprDoc' c (Exists (ObjAccess v (ListAccess i))) = exprDoc c $ BinaryExpr (Var $ render (valueDoc c v) ++ ".length") Greater i
exprDoc' c e = exprDocD'' c e

funcDoc' :: Config -> Function -> Doc
funcDoc' c (IndexOf v) = dot <> funcAppDoc c "indexOf" [v]
funcDoc' c ListSize = dot <> funcAppDoc c "size" []
funcDoc' c f@(ListAccess (EnumVar _)) = funcDocD c f
funcDoc' c f@(ListAccess (EnumElement _ _)) = funcDocD c f
funcDoc' c f@(ListAccess (ObjAccess (ListVar _ (EnumType _)) (ListAccess _))) = funcDocD c f
funcDoc' c (ListAccess i) = dot <> funcAppDoc c "get" [i]
funcDoc' c (ListAdd i v) = dot <> funcAppDoc c "add" [i, v]
funcDoc' c f@(ListSet (EnumVar _) _) = funcDocD c f
funcDoc' c f@(ListSet (EnumElement _ _) _) = funcDocD c f
funcDoc' c (ListSet i v) = dot <> funcAppDoc c "set" [i, v]
funcDoc' c f = funcDocD c f

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v@(EnumVar _) (Cast (Base Integer)) = valueDoc c v <> funcDoc c (Func "ordinal" [])
objAccessDoc' c v@(EnumElement _ _) (Cast (Base Integer)) = valueDoc c v <> funcDoc c (Func "ordinal" [])
objAccessDoc' c v@(ObjAccess (ListVar _ (EnumType _)) (ListAccess _)) (Cast (Base Integer)) = valueDoc c v <> funcDoc c (Func "ordinal" [])
objAccessDoc' c v Floor = funcAppDoc c "Math.floor" [v]
objAccessDoc' c v Ceiling = funcAppDoc c "Math.ceil" [v]
objAccessDoc' c v f = objAccessDocD c v f

methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c _ _ (MainMethod b) = vcat [
    scopeDoc c Public <+> text "static" <+> methodTypeDoc c Void <+> text "main" <> parens (text "String[] args") <+> lbrace,
    oneTab $ bodyDoc c b,
    rbrace]
methodDoc' c f m t = methodDocD c f m t

unOpDoc' :: UnaryOp -> Doc
unOpDoc' SquareRoot = text "Math.sqrt"
unOpDoc' Abs = text "Math.abs"
unOpDoc' op = unOpDocD op
