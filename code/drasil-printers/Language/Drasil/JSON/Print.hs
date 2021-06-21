module Language.Drasil.JSON.Print(genJSON) where

import Prelude hiding (print, (<>))
import Text.PrettyPrint hiding (Str)
import Numeric (showEFloat)
import Utils.Drasil (checkValidStr)

import qualified Language.Drasil as L (DType(Data, Theory, Instance, General), MaxWidthPercent,
  Document, special)
import qualified Data.Drasil.Concepts.Documentation as Doc (notebook)

import Language.Drasil.Printing.Import (makeNotebook)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, 
  Ops(Prod, Inte, Mul, Summ, Or, Add, And, Subt, Iff, Impl, GEq, LEq, Lt, Gt, NEq, Eq,
  Dot, Cross, Neg, Exp, Not, Dim, Arctan, Arccos, Arcsin, Cot, Csc, Sec, Tan, 
  Cos, Sin, Log, Ln, Prime, Comma, Boolean, Real, Rational, Natural, Integer, IsIn, Point, Perc),
  Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, Sp, S, E, (:+:)),
  Spacing(Thin), Fonts(Bold, Emph), OverSymb(Hat), Label,
  LinkType(Internal, Cite2, External))
import Language.Drasil.Printing.Citation (BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import Language.Drasil.Printing.Helpers (sqbrac)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import qualified Language.Drasil.TeX.Print as TeX (spec, pExpr)
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, PrintLaTeX(PL))
import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Helpers (th, em, bold, sub, sup, reflinkInfo, divTag)
import Language.Drasil.HTML.Print(renderCite, OpenClose(Open, Close), fence)

import Language.Drasil.JSON.Helpers (makeMetadata, h, jf, formatter, stripnewLine, cell,
 tr, td, image, li, pa, ba, ul, table, quote, refwrap, refID, reflink, reflinkURI)

genJSON :: PrintingInformation -> L.Document -> Doc
genJSON sm doc = build (makeNotebook sm doc)

-- | Build the JSON Document, called by genJSON
build :: Document -> Doc
build (Document t a c) = 
  text "{" $$
  text " \"cells\": [" $$
  text "  {" $$
  text "   \"cell_type\": \"markdown\"," $$
  text "   \"metadata\": {}," $$
  text "   \"source\": [" $$
  quote (text "# " <> pSpec t) $$
  quote (text "## " <> pSpec a) $$
  markdownE $$
  print c $$
 --(before) text "    \"\\n\"" $$
  --text ("    \"" ++      
  --jf (join(conHTMLformat fn (Document t a c))) ++ "\"") $$
  markdownB $$
  text "   ]" $$
  text "  }" $$
  text " ]," $$
  makeMetadata $$
  text "}"

-- Helper for building markdown cells
markdownB, markdownE :: Doc
markdownB = text $ "  {\n   \"cell_type\": \"markdown\",\n   \"metadata\": {},\n   \"source\": [" 
markdownE = text $ "    \"\\n\"\n   ]\n  },"

-- Helper for rendering a D from Latex print
printMath :: D -> Doc
printMath = (`runPrint` Math)

-- | Helper for rendering LayoutObjects into JSON
printLO :: LayoutObj -> Doc
printLO (Header n contents l)            = quote empty $$ quote (h (n + 1) <> pSpec contents) $$ refID (pSpec l)
printLO (Cell layoutObs l)               = markdownB $$ refID (pSpec l) $$ vcat (map printLO layoutObs) $$ markdownE
printLO (HDiv _ layoutObs EmptyS)        = vcat (map printLO layoutObs)
printLO (HDiv _ layoutObs l)             = refID (pSpec l) $$ vcat (map printLO layoutObs)
printLO (Paragraph contents)             = quote empty $$ quote (stripnewLine (show(pSpec contents)))
printLO (EqnBlock contents)              = quote (jf (show mathEqn)) 
  where
    toMathHelper (PL g) = PL (\_ -> g Math)
    mjDelimDisp d  = text "$$" <> stripnewLine (show d) <> text "$$" 
    mathEqn = mjDelimDisp $ printMath $ toMathHelper $ TeX.spec contents
printLO (Table _ rows r _ _)            = quote empty $$ makeTable rows (pSpec r)
printLO (Definition dt ssPs l)          = quote (text "<br>") $$ makeDefn dt ssPs (pSpec l)
printLO (List t)                        = quote empty $$ makeList t False
printLO (Figure r c f wp)               = makeFigure (pSpec r) (pSpec c) (text f) wp
printLO (Bib bib)                       = makeBib bib
printLO Graph{}                         = empty 

-- | Called by build, uses 'printLO' to render the layout
-- objects in Doc format.
print :: [LayoutObj] -> Doc
print = foldr (($$) . printLO) empty

makeSec :: [LayoutObj] -> Doc
makeSec (l:[ls]) = printLO l
makeSec (x:xs)   = printLO x $$ makeSec xs

--makeDRows [(f,d)]    = tr (quote (th (text f)) $$ td (vcat $ map printLO d))
--makeDRows ((f,d):ps) = tr (quote (th (text f)) $$ td (vcat $ map printLO d)) $$ makeDRows ps


pSpec :: Spec -> Doc
pSpec (E e)  = text "$" <> pExpr e <> text "$" -- symbols used
pSpec (a :+: b) = pSpec a <> pSpec b
pSpec (S s)     = either error (text . concatMap escapeChars) $ checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Sp s)    = text $ unPH $ L.special s
pSpec HARDNL    = empty
pSpec (Ref Internal r a)      = reflink     r $ pSpec a
--pSpec (Ref Cite2    r EmptyS) = reflink     r $ text r -- no difference for citations?
pSpec (Ref Cite2    r a)      = reflinkInfo r (text r) (pSpec a) -- no difference for citations?
pSpec (Ref External r a)      = reflinkURI  r $ pSpec a
pSpec EmptyS    = text "" -- Expected in the output
pSpec (Quote q) = jf $ show $ doubleQuotes $ pSpec q


-- | Renders expressions in the HTML (called by multiple functions)
pExpr :: Expr -> Doc
pExpr (Dbl d)        = text $ showEFloat Nothing d ""
pExpr (Int i)        = text $ show i
pExpr (Str s)        = jf $ show $ doubleQuotes $ text s
pExpr (Row l)        = hcat $ map pExpr l
pExpr (Ident s)      = text s
pExpr (Label s)      = text s
pExpr (Spec s)       = text $ unPH $ L.special s
pExpr (Sub e)        = text "_" <> pExpr e
pExpr (Sup e)        = text "^" <> pExpr e
pExpr (Over Hat s)   = pExpr s <> text "&#770;"
pExpr (MO o)         = text $ pOps o
pExpr (Fenced l r e) = text (fence Open l) <> pExpr e <> text (fence Close r)
pExpr (Font Bold e)  = pExpr e 
--pExpr (Font Bold e)  = bold $ pExpr e -- **used before
--pExpr (Font Emph e)  = text "<em>" <> pExpr e <> text "</em>" -- **HTML used
--pExpr (Spc Thin)     = text "&#8239;" -- **HTML used
-- Uses TeX for Mathjax for all other exprs 
pExpr e              = jf (show (mathEqn))
  where mathEqn = printMath $ toMath $ TeX.pExpr e
  -- **before
  --where mjDelimDisp d = text "$" <> d <> text "$"
  --      mathEqn = mjDelimDisp $ printMath $ toMath $ TeX.pExpr e


-- **TODO: edit all operations in markdown format
pOps :: Ops -> String
pOps IsIn     = "&thinsp;&isin;&thinsp;"
pOps Integer  = "&#8484;"
pOps Rational = "&#8474;"
pOps Real     = "&#8477;"
pOps Natural  = "&#8469;"
pOps Boolean  = "&#120121;"
pOps Comma    = ","
pOps Prime    = "&prime;"
pOps Log      = "log"
pOps Ln       = "ln"
pOps Sin      = "sin"
pOps Cos      = "cos"
pOps Tan      = "tan"
pOps Sec      = "sec"
pOps Csc      = "csc"
pOps Cot      = "cot"
pOps Arcsin   = "arcsin"
pOps Arccos   = "arccos"
pOps Arctan   = "arctan"
pOps Not      = "&not;"
pOps Dim      = "dim"
pOps Exp      = "e"
pOps Neg      = "-"
pOps Cross    = "&#10799;"
pOps Dot      = "&sdot;"
pOps Eq       = " = " -- with spaces?
pOps NEq      = "&ne;"
pOps Lt       = "&thinsp;&lt;&thinsp;" --thin spaces make these more readable
pOps Gt       = "&thinsp;&gt;&thinsp;"
pOps LEq      = "&thinsp;&le;&thinsp;"
pOps GEq      = "&thinsp;&ge;&thinsp;"
pOps Impl     = " &rArr; "
pOps Iff      = " &hArr; "
pOps Subt     = " - "
pOps And      = " &and; "
pOps Or       = " &or; "
pOps Add      = " + "
pOps Mul      = ""
pOps Summ     = "&sum"
pOps Inte     = "&int;"
pOps Prod     = "&prod;"
pOps Point    = "."
pOps Perc     = "%"


-- | Renders HTML table, called by 'printLO'
makeTable :: [[Spec]] -> Doc -> Doc
makeTable [] _      = error "No table to print"
makeTable (l:lls) r = refID r $$ quote empty $$ (makeHeaderCols l $$ makeRows lls) $$ quote empty
--if b then t else empty
--htmlway
--makeTable (l:lls) r = refwrap r (table [] (tr (makeHeaderCols l) $$ makeRows lls)) $$ quote empty


-- | Helper for creating table rows
makeRows :: [[Spec]] -> Doc
makeRows = foldr (($$) . makeColumns) empty
--htmlway
--makeRows = foldr (($$) . tr . makeColumns) empty

makeColumns, makeHeaderCols :: [Spec] -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols l = quote (text $ header) $$ quote (text $ genMDtable ++ "|")
  where header = show(text "|" <> hcat(punctuate (text "|") (map pSpec l)) <> text "|")        
        c = count '|' header
        hl = ""
        genMDtable = concat [hl ++ "|:--- " | i <- [1..c-1]]

-- | Helper for creating table columns
makeColumns ls = quote (text "|" <> hcat(punctuate (text "|") (map pSpec ls)) <> text "|")

count :: Char -> String -> Int
count c [] = 0
count c (x:xs) 
  | c == x = 1 + (count c xs)
  | otherwise = count c xs

{-htmlway
makeColumns, makeHeaderCols :: [Spec] -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols = vcat . map (stripnewLine . show . quote . th . pSpec)
--makeHeaderCols = vcat . map (th . pSpec)

-- | Helper for creating table columns
makeColumns = vcat . map (td . quote . jf . show . pSpec)
-}

{- markdown way
-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: L.DType -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _ [] _  = error "L.Empty definition"
makeDefn dt ps l = refwrap l $ (quote (text "|Refname|" <> l <> text "|") $$ makeDRows ps)

-- | Helper for making the definition table rows
makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows [(f,d)] = text "|" <> text "**" <> text f <> text "**|" <> (vcat $ map printLO d) <> text "|"
makeDRows ((f,d):ps) = text "|" <> text "**" <> text f <> text "**|" <> (vcat $ map printLO d) $$ makeDRows ps
-}


-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: L.DType -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _ [] _  = error "L.Empty definition"
makeDefn dt ps l = refID l $$ table [dtag dt]
  (tr (quote (th (text "Refname")) $$ td (quote(bold l))) $$ makeDRows ps)
  where dtag L.General  = "gdefn"
        dtag L.Instance = "idefn"
        dtag L.Theory   = "tdefn"
        dtag L.Data     = "ddefn"

-- | Helper for making the definition table rows
makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows [(f,d)]    = tr (quote (th (text f)) $$ td (vcat $ map printLO d))
makeDRows ((f,d):ps) = tr (quote (th (text f)) $$ td (vcat $ map printLO d)) $$ makeDRows ps


-- | Renders lists
makeList :: ListType -> Bool -> Doc -- FIXME: ref id's should be folded into the li
makeList (Simple items) _      = vcat $ 
  map (\(b,e,l) -> mlref l $ (quote $ pSpec b <> text ": " <> sItem e) $$ quote empty) items
makeList (Desc items) bl       = vcat $ 
  map (\(b,e,l) -> pa $ mlref l $ ba $ pSpec b <> text ": " <> pItem e bl) items
makeList (Ordered items) bl    = vcat $ map (\(i,l) -> mlref l $ pItem i bl) items
makeList (Unordered items) bl  = vcat $ map (\(i,l) -> mlref l $ pItem i bl) items
--makeList (Definitions items) _ = ul ["hide-list-style-no-indent"] $ vcat $ 
  --map (\(b,e,l) -> li $ mlref l $ quote(pSpec b <> text " is the" <+> sItem e)) items
makeList (Definitions items) _ = vcat $ map (\(b,e,l) -> quote $ li $ mlref l $ pSpec b <> text " is the" <+> sItem e) items

-- | Helper for setting up references
mlref :: Maybe Label -> Doc -> Doc
mlref = maybe id $ refwrap . pSpec

-- | Helper for rendering list items
pItem :: ItemType ->  Bool -> Doc
pItem (Flat s)     b = quote $ (if b then text " - " else text "- ") <> pSpec s
pItem (Nested s l) _ = vcat [quote $ text "- " <> pSpec s, makeList l True]
  --where listIndent = strBreak "\"" (show $ makeList l)
--indent <> text "\"- " <> pSpec s <> text "\\n\","

sItem :: ItemType -> Doc
sItem (Flat s)     = pSpec s
--sItem (Flat s)     = jf (show (pSpec s)) 
sItem (Nested s l) = vcat [pSpec s, makeList l False]

-- | Renders figures in HTML
makeFigure :: Doc -> Doc -> Doc -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refID r $$ (image f c wp)

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = refID l $$ (quote $ jf $ show (i <> text ": " <> a))

makeBib :: BibRef -> Doc
makeBib = vcat .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map renderCite


