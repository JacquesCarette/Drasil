-- | Defines .json printers to generate jupyter notebooks. For more information on each of the helper functions, please view the [source files](https://jacquescarette.github.io/Drasil/docs/full/drasil-printers-0.1.10.0/src/Language.Drasil.JSON.Print.html).
module Language.Drasil.Markdown.Print(genMD) where

import Prelude hiding (print, (<>))
import Text.PrettyPrint hiding (Str)
import Numeric (showEFloat)

import qualified Language.Drasil as L

import Language.Drasil.Format (DocType(Jupyter))

import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, 
  Ops(..), Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, Sp, S, E, (:+:)),
  Fonts(Bold), OverSymb(Hat), Label, LinkType(Internal, Cite2, External))
import Language.Drasil.Printing.Citation (BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import Language.Drasil.Printing.Helpers (sqbrac, unders, hat, pipe, dbs, sq)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import qualified Language.Drasil.TeX.Print as TeX (spec, pExpr)
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, PrintLaTeX(PL))
import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Helpers (th, bold)
import Language.Drasil.HTML.Print(renderCite, OpenClose(Open, Close), fence)

import Language.Drasil.Markdown.Helpers (makeMetadata, h, stripnewLine, nbformat, codeformat,
 tr, td, image, li, pa, ba, table, refwrap, refID, reflink, reflinkURI, reflinkInfo, mkDiv, 
 markdownB, markdownB', markdownE, markdownE', markdownCell, codeCell)

-- | Generate a python notebook document (using json).
-- build : build the SRS document in JSON format
-- build': build the general Jupyter Notbook document
genMD :: PrintingInformation -> L.Document -> Doc
genMD sm doc = build (makeDocument sm doc)

-- | Build the JSON Document, called by genJSON
build :: Document -> Doc
build (Document t a c) = 
  text "# " <> pSpec t $$
  text "## " <> pSpec a $$
  print c 

-- | Helper for rendering a D from Latex print
printMath :: D -> Doc
printMath = (`runPrint` Math)

-- | Helper for rendering LayoutObjects into JSON
-- printLO is used for generating SRS
printLO :: LayoutObj -> Doc
printLO (Header n contents l)            = empty $$ (h (n + 1) <> pSpec contents)
-- printLO (Cell layoutObs)                 = markdownCell $ vcat (map printLO layoutObs)
printLO (HDiv _ layoutObs _)             = vcat (map printLO layoutObs)
printLO (Paragraph contents)             = empty $$ (stripnewLine (show(pSpec contents)))
printLO (EqnBlock contents)              = mathEqn
  where
    toMathHelper (PL g) = PL (\_ -> g Math)
    mjDelimDisp d  = text "$$" <> stripnewLine (show d) <> text "$$" 
    mathEqn = mjDelimDisp $ printMath $ toMathHelper $ TeX.spec contents
printLO (Table _ rows r _ _)            = empty $$ makeTable rows (pSpec r)
printLO (Definition dt ssPs l)          = (text "<br>\n") $$ makeDefn dt ssPs (pSpec l)
printLO (List t)                        = empty $$ makeList t 0
printLO (Figure r c f wp)               = makeFigure (pSpec r) (pSpec c) (text f) wp
printLO (Bib bib)                       = makeBib bib
printLO Graph{}                         = empty 
printLO CodeBlock {}                    = empty

-- printLO' is used for generating general notebook (lesson plans)
printLO' :: LayoutObj -> Doc
printLO' (HDiv ["equation"] layoutObs _)  = markdownCell $ vcat (map printLO' layoutObs)
printLO' (Header n contents l)            = markdownCell $ (h (n + 1) <> pSpec contents)
printLO' (Cell layoutObs)                 = vcat (map printLO' layoutObs)
printLO' HDiv {}                          = empty
printLO' (Paragraph contents)             = markdownCell $ (stripnewLine (show(pSpec contents)))
printLO' (EqnBlock contents)              = mathEqn
  where
    toMathHelper (PL g) = PL (\_ -> g Math)
    mjDelimDisp d  = text "$$" <> stripnewLine (show d) <> text "$$" 
    mathEqn = mjDelimDisp $ printMath $ toMathHelper $ TeX.spec contents
printLO' (Table _ rows r _ _)            = markdownCell $ makeTable rows (pSpec r)
printLO' Definition {}                   = empty
printLO' (List t)                        = markdownCell $ makeList t 0
printLO' (Figure r c f wp)               = markdownCell $ makeFigure (pSpec r) (pSpec c) (text f) wp
printLO' (Bib bib)                       = markdownCell $ makeBib bib
printLO' Graph{}                         = empty 
printLO' (CodeBlock contents)            = codeCell $ codeformat $ cSpec contents


-- | Called by build, uses 'printLO' to render the layout
-- objects in Doc format.
print :: [LayoutObj] -> Doc
print = foldr (($$) . printLO) empty

-- | Called by build', uses 'printLO'' to render the layout
-- objects in Doc format.
print' :: [LayoutObj] -> Doc
print' = foldr (($$) . printLO') empty

pSpec :: Spec -> Doc
pSpec (E e)  = text "$" <> pExpr e <> text "$" -- symbols used
pSpec (a :+: b) = pSpec a <> pSpec b
pSpec (S s)     = either error (text . concatMap escapeChars) $ L.checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Sp s)    = text $ unPH $ L.special s
pSpec HARDNL    = empty
pSpec (Ref Internal r a)      = reflink     (text r) (pSpec a)
pSpec (Ref (Cite2 n)   r a)    = reflinkInfo (text r) (pSpec a) (pSpec n)
pSpec (Ref External r a)      = reflinkURI  (text r) (pSpec a)
pSpec EmptyS    = text "" -- Expected in the output
pSpec (Quote q) = doubleQuotes $ pSpec q

cSpec :: Spec -> Doc
cSpec (E e)  = pExpr e 
cSpec _      = empty


-- | Renders expressions in JSON (called by multiple functions)
pExpr :: Expr -> Doc
pExpr e = printMath $ toMath $ TeX.pExpr e



-- TODO: edit all operations in markdown format
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
pOps VAdd     = " + "
pOps VSub     = " - "
pOps Dot      = "&sdot;"
pOps Scale    = "" -- same as Mul
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
pOps LArrow   = " &larr; "
pOps RArrow   = " &rarr; "
pOps ForAll   = " ForAll "
pOps Partial  = "&part;"


-- | Renders Markdown table, called by 'printLO'
makeTable :: [[Spec]] -> Doc -> Doc
makeTable [] _      = error "No table to print"
makeTable (l:lls) r = refwrap r (empty $$ (makeHeaderCols l $$ makeRows lls) $$ empty)

-- | Helper for creating table rows
makeRows :: [[Spec]] -> Doc
makeRows = foldr (($$) . makeColumns) empty

-- | makeHeaderCols: Helper for creating table header row (each of the column header cells)
-- | makeColumns: Helper for creating table columns
makeHeaderCols, makeColumns :: [Spec] -> Doc
makeHeaderCols l = (text header) $$ (text genMDtable <> pipe)
  where header = show(pipe <> hcat(punctuate pipe (map pSpec l)) <> pipe)        
        c = count '|' header
        genMDtable = concat (replicate (c-1) "|:--- ")

makeColumns ls = (pipe <> hcat(punctuate pipe (map pSpec ls)) <> pipe)

count :: Char -> String -> Int
count _ [] = 0
count c (x:xs) 
  | c == x = 1 + count c xs
  | otherwise = count c xs

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: L.DType -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _ [] _  = error "L.Empty definition"
makeDefn dt ps l = refwrap l $ makeDHeader (text "Refname") l $$ makeDRows ps
-- makeDefn dt ps l = refID l $$ table [dtag dt]
--   (tr ((th (text "Refname")) $$ td ((bold l))) $$ makeDRows ps)
--   where dtag L.General  = "gdefn"
--         dtag L.Instance = "idefn"
--         dtag L.Theory   = "tdefn"
--         dtag L.Data     = "ddefn"

-- | Helper for making the definition table rows
-- makeDRows :: [(String,[LayoutObj])] -> Doc
-- makeDRows []         = error "No fields to create defn table"
-- makeDRows [(f,d)]    = tr ((th (text f)) $$ td (vcat $ map printLO d))
-- makeDRows ((f,d):ps) = tr ((th (text f)) $$ td (vcat $ map printLO d)) $$ makeDRows ps

makeDHeader :: Doc -> Doc -> Doc
makeDHeader lbl txt = pipe <> lbl <> pipe <> txt <> pipe $$ text "|:--- |:--- |" 

makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows [(f,d)]    = pipe <> text f <> pipe <> (hcat $ map printLO d) <> pipe
makeDRows ((f,d):ps) = pipe <> text f <> pipe <> (hcat $ map printLO d) <> pipe $$ makeDRows ps

-- | Renders lists
makeList :: ListType -> Int -> Doc -- FIXME: ref id's should be folded into the li
makeList (Simple items) _      = vcat $ 
  map (\(b,e,l) -> mlref l $ (pSpec b <> text ": " <> sItem e) $$ empty) items
makeList (Desc items) bl       = vcat $ 
  map (\(b,e,l) -> pa $ mlref l $ ba $ pSpec b <> text ": " <> pItem e bl) items
makeList (Ordered items) bl    = vcat $ map (\(i,l) -> mlref l $ pItem i bl) items
makeList (Unordered items) bl  = vcat $ map (\(i,l) -> mlref l $ pItem i bl) items
--makeList (Definitions items) _ = ul ["hide-list-style-no-indent"] $ vcat $ 
  --map (\(b,e,l) -> li $ mlref l $ quote(pSpec b <> text " is the" <+> sItem e)) items
makeList (Definitions items) _ = hcat $ map (\(b,e,l) -> li $ mlref l $ pSpec b <> text " is the" <+> sItem e) items

-- | Helper for setting up references
mlref :: Maybe Label -> Doc -> Doc
mlref = maybe id $ refwrap . pSpec

-- | Helper for rendering list items
pItem :: ItemType ->  Int -> Doc
pItem (Flat s)     i = text (replicate i ' ') <> text "- " <> pSpec s
pItem (Nested s l) i = vcat [text (replicate i ' ') <> text "- " <> pSpec s, makeList l (i+2)]
  --where listIndent = strBreak "\"" (show $ makeList l)
--indent <> text "\"- " <> pSpec s <> text "\\n\","

sItem :: ItemType -> Doc
sItem (Flat s)     = pSpec s
sItem (Nested s l) = vcat [pSpec s, makeList l 0]

-- | Renders figures in HTML
makeFigure :: Doc -> Doc -> Doc -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refwrap r (image f c)

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = refwrap l (i <> text ": " <> a)

makeBib :: BibRef -> Doc
makeBib = vcat .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map renderCite
