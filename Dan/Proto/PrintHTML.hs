{-# OPTIONS -Wall #-} 
module PrintHTML where

import Prelude hiding (print)
import Text.PrettyPrint hiding (render)

import ASTHTML
import ToHTML (makeDocument)
import qualified ASTInternal as A
import Spec (USymb(..))
-- import Config (srsTeXParams, lpmTeXParams, tableWidth, colAwidth, colBwidth)
import HTMLHelpers
import Helpers (brace)
import Unicode
import Format (Format(HTML),FormatC(..))
import Symbol (Symbol(..))
import PrintC (printCode)
import qualified LayoutObjs as L

genHTML :: A.DocType -> L.Document -> Doc
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
printLO (HDiv ts layoutObs)     = div_tag ts (vcat (map printLO layoutObs))
printLO (Paragraph contents)    = paragraph $ text (p_spec contents)
printLO (Tagless contents)      = text $ p_spec contents
printLO (Table ts rows)         = makeTable ts rows
printLO (CodeBlock c)           = code $ printCode c
printLO (Definition dtype ssPs) = makeDefn dtype ssPs
printLO (Header n contents)     = h n $ text (p_spec contents)
printLO (List t items)          = makeList t items

print :: [LayoutObj] -> Doc
print l = foldr ($$) empty $ map printLO l

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------
title_spec :: Spec -> String
title_spec (N s)      = t_symbol s
title_spec (a :+: b)  = title_spec a ++ title_spec b
title_spec (a :^: b)  = title_spec a ++ "^" ++ brace (title_spec b)
title_spec (a :-: b)  = title_spec a ++ "_" ++ title_spec b
title_spec (a :/: b)  = brace (p_spec a) ++ "/" ++ brace (p_spec b)
title_spec HARDNL     = ""
title_spec s          = p_spec s


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
t_symbol s                        = symbol s

symbol :: Symbol -> String
symbol NA               = ""
symbol (Atomic s)       = s
symbol (Special s)      = render HTML s
symbol (Catenate s1 s2) = (symbol s1) ++ (symbol s2)
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = (symbol s) ++ sup (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) ++ sub (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (FormatS Vector s)       = "<b>" ++ symbol s ++ "</b>"
symbol (FormatS Hat s)          = symbol s ++ "&#770;"
symbol (FormatS _ _) = error $ "Cannot use special formatting other than " ++ 
                        "Vector or Hat on Symbols in HTML (see PrintHTML)"

uSymb :: USymb -> String
uSymb Unitless            = "unitless"
uSymb (UName s)           = symbol s
uSymb (UProd l)           = foldr1 
  (\x -> (if (x == "unitless") then (""++) else (++x))) (map uSymb l)
uSymb (UPow Unitless _)   = uSymb Unitless
uSymb (UPow s i)          = uSymb s ++ sup (show i)
uSymb (UDiv n Unitless)   = uSymb n
uSymb (UDiv Unitless d)   = uSymb (UDiv (UName (Atomic "1")) d)
uSymb (UDiv n (UName d))  = uSymb n ++ "/" ++ uSymb (UName d)
uSymb (UDiv n d)          = uSymb n ++ "/(" ++ (uSymb d) ++ ")"

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
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
p_expr (Eq a b)   = p_expr a ++ "=" ++ p_expr b

mul :: Expr -> Expr -> String
mul a b@(Dbl _) = p_expr a ++ "*" ++ p_expr b
mul a b@(Int _) = p_expr a ++ "*" ++ p_expr b
mul a b         = p_expr a ++ p_expr b

-----------------------------------------------------------------
------------------BEGIN TABLE PRINTING---------------------------
-----------------------------------------------------------------
  
makeTable :: Tags -> [[Spec]] -> Doc
makeTable _ []       = error "No table to print (see PrintHTML)"
makeTable ts (l:lls) = wrap "table" ts (
    tr (makeHeaderCols l) $$ makeRows lls)

makeRows :: [[Spec]] -> Doc
makeRows []     = empty
makeRows (c:cs) = tr (makeColumns c) $$ makeRows cs


makeColumns, makeHeaderCols :: [Spec] -> Doc
makeHeaderCols ls = vcat $ map (th . text . p_spec) ls

makeColumns ls = vcat $ map (td . text . p_spec) ls

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

makeDefn :: L.DType -> [(String,LayoutObj)] -> Doc
makeDefn _ []     = error "Empty definition"
makeDefn dt ps    = wrap "table" [dtag dt] (makeDRows ps)
  where dtag L.Data = "ddefn"
        dtag L.Theory = "tdefn"
        dtag L.General = "gdefn"

makeDRows :: [(String,LayoutObj)] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows ((f,d):[]) = tr (th (text f) $$ td (printLO d))
makeDRows ((f,d):ps) = tr (th (text f) $$ td (printLO d)) $$ makeDRows ps

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

makeList :: ListType -> [Spec] -> Doc
makeList t items = wrap (show t ++ "l") ["list"] (vcat $ map
  (wrap "li" [] . text . p_spec) items)