{-# OPTIONS -Wall #-} 
module PrintHTML where

import Prelude hiding (print)
import Data.List (intersperse)
import Text.PrettyPrint hiding (render)

import Control.Monad.Reader

import ASTHTML
import ToHTML (makeDocument)
import qualified ASTInternal as A
import qualified Spec as S
-- import Config (srsTeXParams, lpmTeXParams, tableWidth, colAwidth, colBwidth)
import HTMLHelpers
import Helpers (brace,sec)
import Unicode
import Format (Format(HTML))
import Unit (USymb(..))
import Symbol (Symbol(..))
import PrintC (printCode)

genHTML :: A.DocType -> S.Document -> Doc
genHTML (A.Website fn) doc = build fn $ makeDocument doc
genHTML _ _ = error "Cannot generate HTML for non-Website doctype"

build :: String -> Document -> Doc
build fn (Document t a c) = 
  text ( "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""++
          " \"http://www.w3.org/TR/html4/loose.dtd\">") $$ 
  html (head_tag ((linkCSS fn) $$ (title (text (title_spec t)))) $$
  body (article_title (text (p_spec t)) $$ author (text (p_spec a))
  $$ print c
  ))
  
printLO :: LayoutObj -> Doc
printLO (Section t contents)  = div_tag ["section"] (h 3 ["section"] (text (p_spec t)) $$ print contents)
printLO (Paragraph contents)  = wrap "p" ["paragraph"] $ text (p_spec contents)
printLO (EqnBlock contents)   = div_tag ["equation"] (text $ p_spec contents)
printLO (Table rows) = makeTable rows
printLO (CodeBlock c) = wrap "code" ["code"] (printCode c)
printLO (Definition dtype ssPairs) = makeDDefn dtype ssPairs

print :: [LayoutObj] -> Doc
print l = foldr ($$) empty $ map printLO l

-- -------------------------------------------------------------------
-- ------------------BEGIN SPEC PRINTING------------------------------
-- -------------------------------------------------------------------
title_spec :: Spec -> String
title_spec (N s) = t_symbol s
title_spec (a :+: b)  = title_spec a ++ title_spec b
title_spec (a :^: b)  = title_spec a ++ "^" ++ brace (title_spec b)
title_spec (a :-: b) = title_spec a ++ "_" ++ title_spec b
title_spec (a :/: b)  = brace (p_spec a) ++ "/" ++ brace (p_spec b)
title_spec HARDNL = ""
title_spec s = p_spec s


p_spec :: Spec -> String
p_spec (E e)      = p_expr e
p_spec (a :+: b)  = p_spec a ++ p_spec b
p_spec (a :-: b)  = p_spec a ++ sub (brace (p_spec b))
p_spec (a :^: b)  = p_spec a ++ sup (p_spec b)
p_spec (a :/: b)  = fraction (p_spec a) (p_spec b)
p_spec (S s)      = s
p_spec (N s)      = symbol s
p_spec (Sy s)     = uSymb s
p_spec HARDNL     = "<br />"

t_symbol :: Symbol -> String
t_symbol (Corners [] [] [] [x] s) = t_symbol s ++ "_" ++ t_symbol x
t_symbol (Corners [] [] [x] [] s) = t_symbol s ++ "^" ++ t_symbol x
t_symbol s = symbol s

symbol :: Symbol -> String
symbol (Atomic s) = s
symbol (Special s) = render HTML s
symbol (Catenate s1 s2) = (symbol s1) ++ (symbol s2)
--
-- handle the special cases first, then general case
-- symbol (Corners [] [] [x] [] s) = (symbol s) ++"^"++ (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) ++ sub (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _) = error "rendering of Corners (general)"

uSymb :: USymb -> String
uSymb (UName s) = symbol s
uSymb (UProd l) = foldr1 (++) (map uSymb l)
uSymb (UPow s i) = uSymb s ++ sup (show i)
-------------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING------------------------
-------------------------------------------------------------------
p_expr :: Expr -> String
p_expr (Var v)    = v
p_expr (Dbl d)    = show d
p_expr (Int i)    = show i
p_expr (Add a b)  = p_expr a ++ "+" ++ p_expr b
p_expr (Sub a b)  = p_expr a ++ "-" ++ p_expr b
p_expr (Mul a b)  = mul a b
p_expr (Frac a b) = fraction (p_expr a) (p_expr b) --Found in HTMLHelpers
p_expr (Div a b)  = p_expr a ++ "/" ++ p_expr b
p_expr (Pow a b)  = p_expr a ++ sup (p_expr b)
p_expr (Sym s)    = symbol s

mul :: Expr -> Expr -> String
mul a b@(Dbl _) = p_expr a ++ "*" ++ p_expr b
mul a b@(Int _) = p_expr a ++ "*" ++ p_expr b
mul a b         = p_expr a ++ p_expr b

-------------------------------------------------------------------
------------------BEGIN TABLE PRINTING-----------------------------
-------------------------------------------------------------------
  
makeTable :: [[Spec]] -> Doc
makeTable (l:lls)  = wrap "table" ["table"] (
    wrap "tr" [] (makeHeaderCols l) $$
    makeRows lls
  )

makeRows :: [[Spec]] -> Doc
makeRows [] = empty
makeRows (c:cs) = wrap "tr" [] (makeColumns c) $$ makeRows cs


makeColumns, makeHeaderCols :: [Spec] -> Doc
makeHeaderCols ls = vcat $ map (wrap "th" [] . text . p_spec) ls

makeColumns ls = vcat $ map (wrap "td" [] . text . p_spec) ls

-------------------------------------------------------------------
------------------BEGIN DATA DEFINITION PRINTING-------------------
-------------------------------------------------------------------

makeDDefn :: S.DType -> [(String,LayoutObj)] -> Doc
makeDDefn _ []  = error "Empty definition"
makeDDefn S.Data ps = wrap "table" ["ddefn"] (makeDDRows ps)

makeDDRows :: [(String,LayoutObj)] -> Doc
makeDDRows [] = error "No fields to create DD table"
makeDDRows ((f,d):[]) = wrap "tr" [] (
  wrap "th" [] (text f) $$ wrap "td" [] (printLO d))
makeDDRows ((f,d):ps) = wrap "tr" [] (
  wrap "th" [] (text f) $$ wrap "td" [] (printLO d)
  ) $$ makeDDRows ps


-------------------------------------------------------------------
------------------BEGIN CODE BLOCK PRINTING------------------------
-------------------------------------------------------------------

-- codeHeader,codeFooter :: Doc
-- codeHeader = bslash <> text "begin" <> br "lstlisting"
-- codeFooter = bslash <> text "end" <> br "lstlisting"

-------------------------------------------------------------------
------------------BEGIN EQUATION PRINTING--------------------------
-------------------------------------------------------------------

-- makeEquation :: Spec -> String
-- makeEquation contents = 
  -- ("\\begin{equation}" ++ p_spec contents ++ "\\end{equation}")
  -- --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  -- --  on chunk (i.e. "eq:h_g" for h_g = ...