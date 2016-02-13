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
printLO (Section t contents)  = sec (pCon Plain t) $$ print contents
printLO (Paragraph contents)  = text (pCon Plain contents)
printLO (EqnBlock contents)   = text $ makeEquation contents
printLO (Table rows) = makeTable rows
printLO (CodeBlock c) = codeHeader $$ printCode c $$ codeFooter
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
--p_spec (E e)      = p_expr e
p_spec (a :+: b)  = p_spec a ++ p_spec b
p_spec (a :-: b)  = p_spec a ++ sub (brace (p_spec b))
-- p_spec (a :^: b)  = p_spec a ++ "^" ++ brace (p_spec b)
-- p_spec (a :/: b)  = "\\frac" ++ brace (p_spec a) ++ brace (p_spec b)
p_spec (S s)      = s
p_spec (N s)      = symbol s
-- p_spec (Sy s)     = runReader (uSymbPrint s) Plain
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
p_expr (Pow a b)  = p_expr a ++ "^" ++ brace (p_expr b)
p_expr (Sym s)    = symbol s

mul :: Expr -> Expr -> String
mul a b@(Dbl _) = p_expr a ++ "*" ++ p_expr b
mul a b@(Int _) = p_expr a ++ "*" ++ p_expr b
mul a b         = p_expr a ++ p_expr b

-------------------------------------------------------------------
------------------BEGIN TABLE PRINTING-----------------------------
-------------------------------------------------------------------
  
makeTable :: [[Spec]] -> Doc
makeTable lls  = text ("~\\newline \\begin{longtable}" ++ brace (header lls)) 
  $$ makeRows lls $$ text "\\end{longtable}"
  where header l = concat (replicate ((length (head l))-1) "l ") ++ "p" ++ 
                        brace (show tableWidth ++ "cm")

makeRows :: [[Spec]] -> Doc
makeRows [] = empty
makeRows (c:cs) = text (makeColumns c) $$ dbs $$ makeRows cs

makeColumns :: [Spec] -> String
makeColumns ls = (concat $ intersperse " & " $ map (pCon Plain) ls) ++ "\\"

-------------------------------------------------------------------
------------------BEGIN READER-------------------------------------
-------------------------------------------------------------------

data Context = Equation | EqnB | Plain deriving (Show, Eq)

getCon :: Spec -> Context
getCon (a :+: _) = getCon a
getCon (S _) = Plain
--Not using a catchall for now.
getCon (E _) = Equation
getCon (_ :-: _) = Equation --Subscripts and superscripts must be in Equation ctxt.
getCon (_ :^: _) = Equation
getCon (_ :/: _) = Equation -- Fractions are always equations.
getCon (Sy _) = Plain
getCon (N _) = Equation
getCon HARDNL = Plain


lPrint :: Spec -> Reader Context String
lPrint t@(a :+: b) = do
  c <- ask
  let ca = getCon a
  let cb = getCon b
  case c of
    EqnB -> return $ makeEquation t
    _ -> return $ pCon ca a ++ pCon cb b
    
lPrint t = do
  c <- ask
  let ct = getCon t
  case c of
    EqnB -> return $ makeEquation t
    _ ->
      case ct of
        Equation -> return $ dollar (p_spec t)
        Plain    -> return $ p_spec t
        EqnB     -> return $ makeEquation t 
          --This will never run right now, but maybe eventually.

bEq, eEq :: String    
bEq = "\\begin{equation} " 
eEq = "\\end{equation}"

pCon :: Context -> Spec -> String
pCon = \c t -> runReader (lPrint t) c

uSymbPrint :: USymb -> Reader Context String --To fix unit printing will need this.
uSymbPrint (UName n) = do
  c <- ask
  let cn = getSyCon n
  if c == cn then
    return $ symbol n
  else
    case cn of
      Equation -> return $ dollar $ symbol n 
      _ -> return $ symbol n
uSymbPrint (UProd l) = do
  c <- ask
  return $ foldr1 (++) (map ((\ctxt t -> runReader t ctxt) c) (map uSymbPrint l))
uSymbPrint (UPow n p) = do
  c <- ask
  case c of
    Plain -> return $ runReader (uSymbPrint n) c ++ dollar ("^" ++ brace (show p))
    _ -> return $ runReader (uSymbPrint n) c ++ "^" ++ brace (show p)

getSyCon :: Symbol -> Context
getSyCon (Atomic _) = Plain
--getSyCon (Special Circle) = Equation
  -- TODO: Need to figure this out, or figure out how to print catenations in a 
  --       better way.
getSyCon (Special _) = Plain
getSyCon (Catenate s1 _) = getSyCon s1
getSyCon (Corners _ _ _ _ s) = getSyCon s

-------------------------------------------------------------------
------------------BEGIN DATA DEFINITION PRINTING-------------------
-------------------------------------------------------------------

makeDDefn :: S.DType -> [(String,LayoutObj)] -> Doc
makeDDefn _ []  = error "Empty definition"
makeDDefn S.Data ps = beginDataDefn $$ makeDDTable ps $$ endDataDefn

beginDataDefn :: Doc
beginDataDefn = text "~" <>newline<+> text "\\noindent \\begin{minipage}{\\textwidth}"

endDataDefn :: Doc  
endDataDefn = text "\\end{minipage}" <> dbs

makeDDTable :: [(String,LayoutObj)] -> Doc
makeDDTable [] = error "Trying to make empty Data Defn"
makeDDTable ps@((_,d):_) = vcat [
  text $ "\\begin{tabular}{p{"++show colAwidth++"\\textwidth} p{"++show colBwidth++"\\textwidth}}",
  text "\\toprule \\textbf{Refname} & \\textbf{DD:" <> printLO d <> text "}",
  text "\\label{DD:" <> (printLO d) <> text "}",
  makeDDRows ps, dbs <+> text ("\\bottomrule \\end{tabular}")
  ]

makeDDRows :: [(String,LayoutObj)] -> Doc
makeDDRows [] = error "No fields to create DD table"
makeDDRows ((f,d):[]) = ddBoilerplate $$ text (f ++ " & ") <> printLO d
makeDDRows ((f,d):ps) = ddBoilerplate $$ text (f ++ " & ") <> printLO d $$ 
                        makeDDRows ps
ddBoilerplate :: Doc
ddBoilerplate = dbs <+> text "\\midrule" <+> dbs 

-------------------------------------------------------------------
------------------BEGIN CODE BLOCK PRINTING------------------------
-------------------------------------------------------------------

codeHeader,codeFooter :: Doc
codeHeader = bslash <> text "begin" <> br "lstlisting"
codeFooter = bslash <> text "end" <> br "lstlisting"

-------------------------------------------------------------------
------------------BEGIN EQUATION PRINTING--------------------------
-------------------------------------------------------------------

makeEquation :: Spec -> String
makeEquation contents = 
  ("\\begin{equation}" ++ p_spec contents ++ "\\end{equation}")
  --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  --  on chunk (i.e. "eq:h_g" for h_g = ...