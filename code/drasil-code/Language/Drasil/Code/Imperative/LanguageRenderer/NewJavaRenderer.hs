import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST hiding (body,comment,bool,int,float,char)
import Language.Drasil.Code.Imperative.LanguageRenderer (Config(Config), FileType(Source),
  DecDef(Dec, Def), getEnv, complexDoc, inputDoc, ioDoc, functionListDoc, functionDoc, unOpDoc,
  valueDoc, methodTypeDoc, methodDoc, methodListDoc, statementDoc, stateDoc, stateListDoc,
  scopeDoc, retDoc, printDoc, patternDoc, paramDoc, paramListDoc, classDoc, objAccessDoc,
  objVarDoc, clsDecListDoc, clsDecDoc, litDoc, iterationDoc, funcDoc, funcAppDoc, exprDoc,
  exceptionDoc, declarationDoc, enumElementsDoc, conditionalDoc, callFuncParamList,
  blockDoc, bodyDoc, binOpDoc, body, bottom, top, assignDoc, elseIf, ifBodyStart, blockEnd,
  printFunc, printFileFunc, printFileLnFunc, printLnFunc, stateType, blockStart, clsDec,
  listObj, package, list, iterInLabel, iterForEachLabel, inherit, inputFunc, include,
  includeScope, fileName, ext, dir, enumsEqualInts, commentStart, endStatement, bitArray,
  renderCode, argsList, Options, ioDocD, StatementLocation(NoLoop), dot, inputDocD,
  valueDocD, methodDocD, methodListDocD, paramDocD, paramListDocD,
  objAccessDocD, iterationDocD, funcDocD, assignDocD, stateTypeD, fileCode,
  functionListDocD, methodTypeDocD, unOpDocD, statementDocD, scopeDocD, stateDocD, stateListDocD,
  doubleSlash, retDocD, patternDocD, clsDecListDocD, clsDecDocD, funcAppDocD, enumElementsDocD,
  litDocD, conditionalDocD'', callFuncParamListD, bodyDocD, blockDocD, binOpDocD,
  classDec, includeD, fileNameD, new, exprDocD'', declarationDocD,
  typeOfLit, functionDocD, printDocD, objVarDocD, classDocD, forLabel, javalist)
import Language.Drasil.Code.Imperative.Helpers (blank,angles,oneTab,vibmap)

import Prelude hiding (break,print)
import Control.Monad.Reader
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals, 
  semi, vcat, lbrace, rbrace, doubleQuotes, render, colon)

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

data JavaCode a = JC {unJC :: Reader Config Doc}

instance StateTypeSym JavaCode

instance ValueSym JavaCode

instance IOSym JavaCode

instance StatementSym JavaCode where
    printStrLn s = JC $ printDocD 

instance Symantics JavaCode

instance BodySym JavaCode where
    body sts = JC $ do
        c <- ask
        return (statementDoc c sts)

--

-- eval options = runReader unJC options

-- what is options? Config? Options? 
-- where does this belong?
-- flow:
-- module(s) in GOOL
    -- passed to toAbsCode, yielding AbstractCode
    -- passed to makeCode
        -- uses renderCode from Config 
        -- this is where config gets passed. 
            -- passes config to fileCode
            -- fileCode returns [(filePath, Doc)]
            -- Doc comes from call to fileDoc on each module
            -- should fileDoc be runReader instead?
                -- fileDoc has top, body, bottom
                -- maybe body should be where runReader is called?
            -- converts modules to classes
                -- classDocD is the top-level function for converting to Doc, works its way to lower-level stuff