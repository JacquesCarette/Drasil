-- | The structure for a class of renderers is defined here.
module Language.Drasil.Code.Imperative.NewLanguageRenderer (
    -- * Language Parametric Functions
    fileCode,
    
    -- * Common Syntax
    classDec, dot, doubleSlash, forLabel, new,
    
    -- * Default Functions available for use in renderers
    renderCode', fileDoc', ioDocOutD, litStringD, includeD
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New (RenderSym(..), Symantics(..), Label, fileDoc)
import Language.Drasil.Code.Imperative.Helpers (angles,blank,doubleQuotedText,oneTab,
                            oneTabbed,himap,vibcat,vmap,vibmap)

import qualified Data.Map as Map (fromList,lookup)
import Data.List (find)
import Prelude hiding (break,print,return,last,mod)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), brackets, parens,
  isEmpty, rbrace, lbrace, vcat, space, char, double, quotes, integer, semi, equals, braces,
  int, comma, colon)

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
