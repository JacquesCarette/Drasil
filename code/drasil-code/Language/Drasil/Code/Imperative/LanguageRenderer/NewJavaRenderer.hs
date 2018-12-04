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
        
        getEnv = \_ -> error "no environment has been set"
    }

newtype JavaCode a = JC {unJC :: Doc}

instance Monad JavaCode where
    return = JC
    JC a >>= f = f a
    
instance ClassSym JavaCode where
    buildClass n p s vs fs = return $ (classDocD n p s vs fs)

-- instance ClassSym JavaCode where
--     buildClass n p s vs fs = JC $ do
--         c <- ask -- not needed, only ask for config when you need to actually extract from it
--         return (classDoc c n p s vs fs)

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

-- Monad instance for javacode, return wraps in JC

-- runReader unJC jc options

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