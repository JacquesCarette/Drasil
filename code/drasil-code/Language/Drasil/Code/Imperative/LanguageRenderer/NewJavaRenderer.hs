-- | The logic to render Java code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.NewJavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    JavaCode(..)
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New
import Language.Drasil.Code.Imperative.NewLanguageRenderer (renderCode', fileDoc',
    ioDocOutD, litStringD, includeD, dot)
import Language.Drasil.Code.Imperative.Helpers (blank,angles,oneTab,vibmap)

import Prelude hiding (break,print)
import Control.Applicative (Applicative, liftA, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals, 
  semi, vcat, lbrace, rbrace, doubleQuotes, render, colon)

-- javaConfig :: Options -> Config -> Config
-- javaConfig options c = 
--     let listType = case (javalist options) of Nothing -> "Vector"
--                                             Just lt -> if lt `elem` validListTypes then lt
--                                                         else error $ "Unsupported Java list type specified in config file: " ++ lt ++ "\nSupported types are: " ++ show validListTypes
--     in Config {
--         renderCode = renderCode' c,
        
--         argsList         = text "args",
--         bitArray         = text "BitSet",
--         commentStart     = doubleSlash,
--         enumsEqualInts   = False,
--         ext              = ".java",
--         dir              = "java",
--         fileName         = fileNameD c,
--         includeScope     = (scopeDoc c),
--         inherit          = text "extends",
--         inputFunc        = parens(text "new Scanner(System.in)"),
--         iterForEachLabel = forLabel,
--         iterInLabel      = colon,
--         listObj          = new,
--         clsDec           = classDec,
--         package          = package',
--         stateType        = jstateType c,
        
--         blockStart = lbrace, blockEnd = rbrace, 
--         ifBodyStart = blockStart c, elseIf = text "else if",
        
--         getEnv = \_ -> error "no environment has been set"
--     }

newtype JavaCode a = JC {unJC :: a}

instance Functor JavaCode where
    fmap f (JC x) = JC (f x)

instance Applicative JavaCode where
    pure = JC
    (JC f) <*> (JC x) = JC (f x)

instance Monad JavaCode where
    return = JC
    JC x >>= f = f x

instance RenderSym JavaCode where
    fileDoc code = liftA3 fileDoc' top code bottom
    top = do
        end <- endStatement 
        inc <- include
        lst <- list
        return $ vcat [
            inc <+> text "java.util.Arrays" <> end,
            inc <+> text "java.util.BitSet" <> end,     --TODO: only include these if they are used in the code?
            inc <+> text "java.util.Scanner" <> end,
            inc <+> text "java.io.PrintWriter" <> end,
            inc <+> text "java.io.File" <> end,
            inc <+> text ("java.util." ++ render (lst)) <> end]
    codeBody m = m -- don't need right now
    bottom = return empty

instance KeywordSym JavaCode where
    endStatement = return semi
    include = return $ includeD "import"
    list = return $ text "Vector"
    printFunc = return $ text "System.out.print"
    printLnFunc = return $ text "System.out.println"
    printFileFunc f = do 
        fname <- f
        return $ fname <> dot <> text "print"
    printFileLnFunc f = do 
        fname <- f
        return $ fname <> dot <> text "println"
    
instance ClassSym JavaCode where
    -- buildClass n p s vs fs = liftA3 (classDocD n p) s vs fs -- vs and fs are actually lists... should 

-- instance ClassSym JavaCode where
--     buildClass n p s vs fs = do
--         scope <- s
--         vars <- vs
--         funcs <- fs --ignore for the moment that fs and vs are lists
--         return (classDocD n p scope vars funcs)

-- instance ClassSym JavaCode where
--     buildClass n p s vs fs = JC $ do
--         c <- ask -- not needed, only ask for config when you need to actually extract from it
--         return (classDoc c n p s vs fs)

instance StateTypeSym JavaCode

instance ValueSym JavaCode where
    litString s = return $ litStringD s

instance IOTypeSym JavaCode

instance IOStSym JavaCode where
    out prf v = liftA3 ioDocOutD prf v endStatement

instance StatementSym JavaCode where
    print _ v = ioState (out printFunc v)
    printLn _ v = ioState (out printLnFunc v)
    printStr s = ioState (out printFunc (litString s))
    printStrLn s = ioState (out printLnFunc (litString s))

    printFile f _ v = ioState (out (printFileFunc f) v)
    printFileLn f _ v = ioState (out (printFileLnFunc f) v)
    printFileStr f s = ioState (out (printFileFunc f) (litString s))
    printFileStrLn f s = ioState (out (printFileLnFunc f) (litString s))

    ioState = id -- Now that types are Doc synonyms, I think this will work

instance Symantics JavaCode

instance BodySym JavaCode where
    -- body sts = JC $ do
    --     c <- ask
    --     return (statementDoc c sts)


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