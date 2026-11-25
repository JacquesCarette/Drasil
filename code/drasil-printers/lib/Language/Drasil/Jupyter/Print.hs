{-# LANGUAGE OverloadedStrings #-}

-- | Defines .json printers to generate jupyter notebooks.
module Language.Drasil.Jupyter.Print (
  CellLayout(..), genJupyter
) where

import Prelude hiding ((<>))
import qualified Prelude as P ((<>))
import Text.PrettyPrint hiding (Str)
import Numeric (showEFloat)

import Drasil.Data.Formats.Jupyter (Notebook(..), Cell)
import Language.Drasil (checkValidStr)

import Drasil.Printers.Common
import Drasil.Document.Layout.Internal (ItemType(..), ListType(..), Spec(..),
  Expr(..), Label, Fonts(Bold), OverSymb(Hat), Fence(..), Ops(..), LinkType(..),
  Special(Circle), BibRef, Document(Document), LayoutObj(..), MaxWidthPercent)
import Language.Drasil.Printing.Helpers (unders, hat)
import qualified Language.Drasil.TeX.Print as TeX (spec, pExpr, printMath)
import Language.Drasil.Markdown.Citation (BibFormatter(..), renderCite)

import Language.Drasil.Jupyter.Helpers (notebookMetadata, h, stripnewLine,
 tr, td, th, bold, em, image, li, pa, ba, table, refwrap, refID, reflink,
 reflinkInfo, reflinkURI, mkDiv, markdownCell, codeCell)

-- | How a document's contents are split into notebook cells.
data CellLayout
  = CellPerObject -- ^ Each 'LayoutObject' gets its own cell, and code blocks
                  --   become code cells.
  | SingleCell    -- ^ All content goes into one Markdown cell.

-- | Build a Jupyter notebook from a document, splitting its contents into
-- cells according to the 'CellLayout'.
genJupyter :: CellLayout -> Document -> Notebook
genJupyter cl (Document t a c) =
  Notebook notebookMetadata (titleCell : layoutCells cl c)
  where
    titleCell = markdownCell (vcat [text "# " <> pSpec t, text "## " <> pSpec a])

-- | Lays out a document's contents as notebook cells.
layoutCells :: CellLayout -> [LayoutObj] -> [Cell]
layoutCells CellPerObject los = concatMap loToCells los
layoutCells SingleCell    los = [markdownCell (losToMarkdown los)]

-- | Internal: Rendering LayoutObjects into JSON in a single cell.
loToMarkdown :: LayoutObj -> Doc
loToMarkdown (Header n contents l) = text "" $$ h (n + 1) <> pSpec contents $$ refID (pSpec l)
loToMarkdown (Cell _)              = empty
loToMarkdown (HDiv _ layoutObs _)  = vcat (loToMarkdown <$> layoutObs)
loToMarkdown (Paragraph contents)  = text "" $$ stripnewLine (show (pSpec contents))
loToMarkdown (EqnBlock contents)   = eqnBlock contents
loToMarkdown (Table _ rows r _ _)  = text "" $$ makeTable rows (pSpec r)
loToMarkdown (Definition ssPs l)   = text "<br>" $$ makeDefn ssPs (pSpec l)
loToMarkdown (List t)              = text "" $$ makeList t False
loToMarkdown (Figure r c f wp)     = makeFigure (pSpec r) (pSpec <$> c) (text f) wp
loToMarkdown (Bib bib)             = makeBib bib
loToMarkdown Graph{}               = empty
loToMarkdown CodeBlock {}          = empty

-- | Internal: Rendering LayoutObjects into JSON into multiple cells.
loToCells :: LayoutObj -> [Cell]
loToCells (Header n contents l) = [markdownCell $ (h (n + 1) <> pSpec contents) $$ refID (pSpec l)]
loToCells (Cell layoutObs)      = concatMap loToCells layoutObs
loToCells HDiv{}                = []
loToCells (Paragraph contents)  = [markdownCell $ stripnewLine (show (pSpec contents))]
loToCells (EqnBlock contents)   = [markdownCell $ eqnBlock contents]
loToCells (Table _ rows r _ _)  = [markdownCell $ makeTable rows (pSpec r)]
loToCells Definition{}          = []
loToCells (List t)              = [markdownCell $ makeList t False]
loToCells (Figure r c f wp)     = [markdownCell $ makeFigure (pSpec r) (pSpec <$> c) (text f) wp]
loToCells (Bib bib)             = [markdownCell $ makeBib bib]
loToCells Graph{}               = []
loToCells (CodeBlock contents)  = [codeCell $ cSpec contents]
  where
    -- FIXME: CodeBlock's 'contents' type looks like it can be improved...
    cSpec :: Spec -> Doc
    cSpec (E e) = pExpr NotebookCode e
    cSpec _     = empty

-- | Internal: Renders an equation block as display math.
eqnBlock :: Spec -> Doc
eqnBlock = ddollars . stripnewLine . show . TeX.printMath . TeX.spec

-- | Renders layout objects as Markdown, one after another.
losToMarkdown :: [LayoutObj] -> Doc
losToMarkdown = foldr (($$) . loToMarkdown) empty

data ExprContext = NotebookMath | NotebookCode

pSpec :: Spec -> Doc
pSpec (E e)                    = dollar $ pMathExpr e
pSpec (a :+: b)                = pSpec a <> pSpec b
pSpec (S s)                    =
  either error (text . concatMap escapeChars) $ checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Tooltip _ s)            = pSpec s
pSpec (Sp Circle)              = text "&deg;"
pSpec (Ref Internal r a)       = reflink     r $ pSpec a
pSpec (Ref (Cite2 EmptyS) r a) = reflink     r $ pSpec a -- no difference for citations?
pSpec (Ref (Cite2 n)   r a)    = reflinkInfo r  (pSpec a) (pSpec n)
pSpec (Ref External r a)       = reflinkURI  r $ pSpec a
pSpec (Quote q)                = dquote $ pSpec q
pSpec EmptyS                   = text "" -- Expected in the output

-- | Render math where the enclosing context already delimits the expression.
pMathExpr :: Expr -> Doc
pMathExpr (Row [e]) = pMathExpr e
pMathExpr e = pExpr NotebookMath e

-- | Renders expressions in JSON.
pExpr :: ExprContext -> Expr -> Doc
pExpr _            (Dbl d)        = text $ showEFloat Nothing d ""
pExpr _            (Int i)        = text $ show i
pExpr _            (Str s)        = dquote $ text s
pExpr NotebookMath (Div n d)      = mkDiv "frac" (pMathExpr n) (pMathExpr d)
pExpr ctx          (Div n d)      = mkDiv "frac" (pExpr ctx n) (pExpr ctx d)
pExpr NotebookMath (Row [x])      = braces $ pExpr NotebookMath x
pExpr ctx          (Row l)        = hcat $ pExpr ctx <$> l
pExpr ctx          (Set l)        = hcat $ pExpr ctx <$> l
pExpr _            (Ident s)      = text s
pExpr NotebookMath (Label s)      = TeX.printMath $ TeX.pExpr (Label s)
pExpr NotebookCode (Label s)      = text s
pExpr _            (Spec Circle)  = text "&deg;"
pExpr NotebookMath (Sub e)        = unders <> braces (pMathExpr e)
pExpr NotebookCode (Sub e)        = unders <> pExpr NotebookCode e
pExpr NotebookMath (Sup e)        = hat <> braces (pMathExpr e)
pExpr NotebookCode (Sup e)        = hat <> pExpr NotebookCode e
pExpr ctx          (Over Hat s)   = pExpr ctx s <> text "&#770;"
pExpr _            (MO o)         = text $ pOps o
pExpr ctx          (Fenced l r e) =
  text (fence Open l) <> pExpr ctx e <> text (fence Close r)
pExpr ctx          (Font Bold e)  = pExpr ctx e
-- Uses TeX for Mathjax for all other exprs
pExpr _            e              = TeX.printMath $ TeX.pExpr e

-- TODO: edit all operations in markdown format
pOps :: Ops -> String
pOps IsIn      = "&thinsp;&isin;&thinsp;"
pOps Integer   = "&#8484;"
pOps Rational  = "&#8474;"
pOps Real      = "&#8477;"
pOps Natural   = "&#8469;"
pOps Boolean   = "&#120121;"
pOps Comma     = ","
pOps Prime     = "&prime;"
pOps Log       = "log"
pOps Ln        = "ln"
pOps Sin       = "sin"
pOps Cos       = "cos"
pOps Tan       = "tan"
pOps Sec       = "sec"
pOps Csc       = "csc"
pOps Cot       = "cot"
pOps Arcsin    = "arcsin"
pOps Arccos    = "arccos"
pOps Arctan    = "arctan"
pOps Not       = "&not;"
pOps Dim       = "dim"
pOps Exp       = "e"
pOps Neg       = "-"
pOps Cross     = "&#10799;"
pOps VAdd      = " + "
pOps VSub      = " - "
pOps Dot       = "&sdot;"
pOps Scale     = "" -- same as Mul
pOps Eq        = " = " -- with spaces?
pOps NEq       = "&ne;"
pOps Lt        = "&thinsp;&lt;&thinsp;" --thin spaces make these more readable
pOps Gt        = "&thinsp;&gt;&thinsp;"
pOps LEq       = "&thinsp;&le;&thinsp;"
pOps GEq       = "&thinsp;&ge;&thinsp;"
pOps Impl      = " &rArr; "
pOps Iff       = " &hArr; "
pOps Subt      = " - "
pOps And       = " &and; "
pOps Or        = " &or; "
pOps Add       = " + "
pOps Mul       = ""
pOps Summ      = "&sum"
pOps Inte      = "&int;"
pOps Prod      = "&prod;"
pOps Point     = "."
pOps Perc      = "%"
pOps LArrow    = " &larr; "
pOps RArrow    = " &rarr; "
pOps ForAll    = " ForAll "
pOps Partial   = "&part;"
pOps SAdd      = " + "
pOps SRemove   = " - "
pOps SContains = " in "
pOps SUnion    = " and "

-- | Referring to 'fence' (for parenthesis and brackets). Either opened or closed.
data OpenClose = Open | Close

-- | Allows for open/closed variants of parenthesis, curly brackets, absolute value symbols, and normal symbols.
fence :: OpenClose -> Fence -> String
fence Open  Paren = "("
fence Close Paren = ")"
fence Open  Curly = "{"
fence Close Curly = "}"
fence _     Abs   = "|"
fence _     Norm  = "||"

-- | Renders Markdown table.
makeTable :: [[Spec]] -> Doc -> Doc
makeTable [] _      = error "No table to print"
makeTable (l:lls) r = refID r $$ text "" $$ (makeHeaderCols l $$ makeRows lls) $$ text ""

-- | Helper for creating table rows
makeRows :: [[Spec]] -> Doc
makeRows = foldr (($$) . makeColumns) empty

-- | makeHeaderCols: Helper for creating table header row (each of the column header cells)
-- | makeColumns: Helper for creating table columns
makeHeaderCols, makeColumns :: [Spec] -> Doc
makeHeaderCols l = text header $$ text (genMDtable P.<> "|")
  where
    header = show (text "|" <> hcat (punctuate (text "|") (pSpec <$> l)) <> text "|")
    c = count '|' header
    genMDtable = concat (replicate (c-1) "|:--- ")

makeColumns ls = text "|" <> hcat (punctuate (text "|") (pSpec <$> ls)) <> text "|"

count :: Char -> String -> Int
count _ [] = 0
count c (x:xs)
  | c == x = 1 + count c xs
  | otherwise = count c xs

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: [(String, [LayoutObj])] -> Doc -> Doc
makeDefn [] _ = error "Definition table has no designated rows to render!"
makeDefn ps l = refID l $$ table ["defn-table"]
  (tr (th (text "Refname") $$ td (bold l)) $$ makeDRows ps)

-- | Helper for making the definition table rows
makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows [] = error "No fields to create defn table"
makeDRows ps = vcat $ (\(f, d) -> tr (th (text f) $$ td (vcat $ loToMarkdown <$> d))) <$> ps

-- | Renders lists
makeList :: ListType -> Bool -> Doc -- FIXME: ref id's should be folded into the li
makeList (Simple items) _      = vcat $
  (\(b,e,l) -> mlref l $ pSpec b <> text ": " <> sItem e $$ text "") <$> items
makeList (Desc items) bl       = vcat $
  (\(b,e,l) -> pa $ mlref l $ ba $ pSpec b <> text ": " <> pItem e bl) <$> items
makeList (Ordered items) bl    = vcat $ (\(i,l) -> mlref l $ pItem i bl) <$> items
makeList (Unordered items) bl  = vcat $ (\(i,l) -> mlref l $ pItem i bl) <$> items
makeList (Definitions items) _ = vcat $ (\(b,e,l) -> li $ mlref l $ pSpec b <> text " is the" <+> sItem e) <$> items

-- | Helper for setting up references
mlref :: Maybe Label -> Doc -> Doc
mlref = maybe id $ refwrap . pSpec

-- | Helper for rendering list items
pItem :: ItemType ->  Bool -> Doc
pItem (Flat s)     b = (if b then text " - " else text "- ") <> pSpec s
pItem (Nested s l) _ = vcat [text "- " <> pSpec s, makeList l True]

sItem :: ItemType -> Doc
sItem (Flat s)     = pSpec s
sItem (Nested s l) = vcat [pSpec s, makeList l False]

-- | Renders figures in HTML
makeFigure :: Doc -> Maybe Doc -> Doc -> MaxWidthPercent -> Doc
makeFigure r c f wp = refID r $$ image f c wp

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = refID l $$ i <> text ": " <> a

-- | JSON specific bib rendering functions
jsonBibFormatter :: BibFormatter
jsonBibFormatter = BibFormatter {
  emph = em,
  spec = pSpecBib
}

pSpecBib :: Spec -> Doc
pSpecBib (Ref External r a) = text ("<a href=\"" P.<> r P.<> "\">") <> pSpecBib a <> text "</a>"
pSpecBib s                  = pSpec s

makeBib :: BibRef -> Doc
makeBib = vcat . zipWith entry [1 :: Int ..] . fmap (renderCite jsonBibFormatter)
  where entry i (l, a) = makeRefList a l (brak $ text $ show i)
