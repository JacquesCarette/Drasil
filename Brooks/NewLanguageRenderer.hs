-- | The structure for a class of renderers is defined here.
module NewLanguageRenderer (
    -- * Code Generation Funcitons
    makeCode, createCodeFiles,
    
    -- * Common Syntax
    classDec, dot, doubleSlash, forLabel, new,
    
    -- * Default Functions available for use in renderers
    fileDoc', ioDocOutD, litStringD, includeD
) where

import New (RenderSym(..), Symantics(..), Label, fileDoc)
import Helpers (angles,blank,doubleQuotedText,oneTab,
                            oneTabbed,himap,vibcat,vmap,vibmap)

import Data.List (find)
import Prelude hiding (break,print,return,last,mod,(<>))
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), brackets, parens,
  isEmpty, rbrace, lbrace, vcat, space, char, double, quotes, integer, semi, equals, braces,
  int, comma, colon)


-- | Takes code, filenames, and extensions
makeCode :: [a] -> [Label] -> [Label] -> [(FilePath, a)]
makeCode files names exts =
    [(name ++ ext, file) | (name, (file, ext)) <- zip (duplicateListElems names) (zip files (cycle exts))]

duplicateListElems :: [a] -> [a]
duplicateListElems [] = []
duplicateListElems (x:xs) = x:x:duplicateListElems xs

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files
createCodeFiles :: [(FilePath, Doc)] -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles cs = mapM_ createCodeFile cs

createCodeFile :: (FilePath, Doc) -> IO ()
createCodeFile (path, code) = do
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

classDec,dot,doubleSlash,forLabel,new :: Doc
classDec = text "class"
dot = text "."
doubleSlash = text "//"
forLabel = text "for"
new = text "new"

----------------------------------
-- Functions for rendering code --
----------------------------------

fileDoc' :: Doc -> Doc -> Doc -> Doc
fileDoc' t m b = vibcat [
    t,
    m,
    b]
    
-- fileNameD :: Module -> String
-- fileNameD _ = moduleName

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

-- Uncomment classDocD when inherit works

-- classDocD :: Label -> Maybe Label -> Doc -> Doc -> Doc -> Doc
-- classDocD n p s vs fs = vcat [
--     s <+> clsDec c <+> text n <+> baseClass <+> lbrace, 
--     oneTabbed [
--         vs,
--         blank,
--         fs],
--     rbrace]
--     where baseClass = case p of Nothing -> empty
--                                 Just pn -> inherit c <+> text pn

-- ioState just returns itself. don't need a function for this.
-- statementDocD :: Config -> StatementLocation -> Statement -> Doc
-- statementDocD c _ (IOState io) = ioDoc c io

ioDocOutD :: Doc -> Doc -> Doc -> Doc
ioDocOutD printFn v endSt = printFn <> parens (v) <> endSt

-- Replaced by ioDocOutD
-- printDocConsoleD :: Bool -> Doc -> Doc
-- printDocConsoleD newLn v = printFn <> parens (valueDoc c v)
--     where printFn = if newLn then printLnFunc c else printFunc c

-- valueDocD :: Config -> Value -> Doc
-- valueDocD c (Lit v) = litDoc c v

litStringD :: String -> Doc
litStringD s = doubleQuotedText s

includeD :: Label -> Doc
includeD incl = text incl
