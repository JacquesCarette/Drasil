-- | Defines all functions needed to print HTML files. For more information on each of the helper functions, please view the [source files](https://jacquescarette.github.io/Drasil/docs/full/drasil-printers-0.1.10.0/src/Language.Drasil.HTML.Print.html).
module Language.Drasil.HTML.Print(
  -- * Main Function
  genHTML,
  -- * Citation Renderer
  renderCite,
  -- * Term Fencing Helpers
  OpenClose(Open, Close), 
  fence) where

import Prelude hiding (print, (<>))
import Data.List (sortBy)
import Text.PrettyPrint hiding (Str)
import Numeric (showEFloat)

import qualified Language.Drasil as L

import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Helpers (articleTitle, author, ba, body, bold,
  caption, divTag, em, h, headTag, html, image, li, ol, pa,
  paragraph, reflink, reflinkInfo, reflinkURI, refwrap, sub, sup, table, td,
  th, title, tr, ul)
import Language.Drasil.HTML.CSS (linkCSS)

import Language.Drasil.Config (StyleGuide(APA, MLA, Chicago), bibStyleH)
import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, Fence(Curly, Paren, Abs, Norm),
  Ops(..), Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, Sp, S, E, (:+:)),
  Spacing(Thin), Fonts(Bold, Emph), OverSymb(Hat), Label,
  LinkType(Internal, Cite2, External))
import Language.Drasil.Printing.Citation (CiteField(Year, Number, Volume, Title, Author, 
  Editor, Pages, Type, Month, Organization, Institution, Chapter, HowPublished, School, Note,
  Journal, BookTitle, Publisher, Series, Address, Edition), HP(URL, Verb), 
  Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..), Tags)
import Language.Drasil.Printing.Helpers (comm, dot, paren, sufxer, sqbrac, sufxPrint)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import qualified Language.Drasil.TeX.Print as TeX (pExpr, spec)
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, PrintLaTeX(PL))

-- | Referring to 'fence' (for parenthesis and brackeds). Either opened or closed.
data OpenClose = Open | Close

-- | Generate an HTML document from a Drasil 'Document'.
genHTML :: PrintingInformation -> String -> L.Document -> Doc
genHTML sm fn doc = build fn (makeDocument sm doc)
--         ^^ -- should really be of type Filename, but that's not in scope

-- TODO: Use our JSON printer here to create this code snippet.
-- | Variable to include MathJax in our HTML files so we can render equations in LaTeX.
mathJaxScript :: Doc
mathJaxScript =
  vcat [text "<script>",
        text "MathJax = {",
        text "  loader: {load: ['[tex]/textmacros', 'output/chtml']},",
        text "  tex: {",
        text "    packages: {'[+]': ['textmacros']}",
        text "  },",
        text "  svg: {",
        text "    fontCache: 'global'",
        text "  }",
        text "};",
        text "</script>",
        text "<script type=\"text/javascript\" id=\"MathJax-script\" async",
        text " src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js\">",
        text "</script>"]

-- HTML printer doesn't need to know if there is a table of contents or not.
-- | Build the HTML Document, called by 'genHTML'.
build :: String -> Document -> Doc
build fn (Document t a c) =
  text "<!DOCTYPE html>" $$
  html (headTag (linkCSS fn $$ title (titleSpec t) $$
  text "<meta charset=\"utf-8\">" $$
  mathJaxScript) $$
  body (articleTitle (pSpec t) $$ author (pSpec a)
  $$ print c
  ))

-- | Helper for rendering a 'D' from Latex print.
printMath :: D -> Doc
printMath = (`runPrint` Math)

-- | Helper for rendering layout objects ('LayoutObj's) into HTML.
printLO :: LayoutObj -> Doc
-- FIXME: could be hacky
printLO (HDiv ["equation"] layoutObs EmptyS)  = vcat (map printLO layoutObs)
-- Creates delimeters to be used for mathjax displayed equations
-- Latex print sets up a \begin{displaymath} environment instead of this
printLO (EqnBlock contents)    = mjDelimDisp $ printMath $ toMathHelper $ TeX.spec contents
  where
    toMathHelper (PL g) = PL (\_ -> g Math)
    mjDelimDisp d = text "\\[" <> d <> text "\\]"
-- Non-mathjax
-- printLO (EqnBlock contents) = pSpec contents
printLO (HDiv ts layoutObs EmptyS)  = divTag ts (vcat (map printLO layoutObs))
printLO (HDiv ts layoutObs l)  = refwrap (pSpec l) $
                                 divTag ts (vcat (map printLO layoutObs))
printLO (Paragraph contents)   = paragraph $ pSpec contents
printLO (Table ts rows r b t)  = makeTable ts rows (pSpec r) b (pSpec t)
printLO (Definition dt ssPs l) = makeDefn dt ssPs (pSpec l)
printLO (Header n contents _)  = h (n + 1) $ pSpec contents -- FIXME
printLO (List t)               = makeList t
printLO (Figure r c f wp)      = makeFigure (pSpec r) (pSpec c) (text f) wp
printLO (Bib bib)              = makeBib bib
printLO Graph{}                = empty -- FIXME
printLO Cell{}                 = empty


-- | Called by build, uses 'printLO' to render the layout
-- objects in 'Doc' format.
print :: [LayoutObj] -> Doc
print = foldr (($$) . printLO) empty

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders the title of the document. Different than body rendering
-- because newline can't be rendered in an HTML title.
titleSpec :: Spec -> Doc
titleSpec (a :+: b) = titleSpec a <> titleSpec b
titleSpec HARDNL    = empty
titleSpec s         = pSpec s

-- | Renders the Sentences ('Spec's) in the HTML body (called by 'printLO').
pSpec :: Spec -> Doc
-- Non-mathjax
pSpec (E e)  = em $ pExpr e
-- Latex based math for expressions and units
-- pSpec (E e)     = printMath $ toMath $ TeX.pExpr e
-- pSpec (Sy s)    = printMath $ TeX.pUnit s
pSpec (a :+: b) = pSpec a <> pSpec b
pSpec (S s)     = either error (text . concatMap escapeChars) $ L.checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Sp s)    = text $ unPH $ L.special s
pSpec HARDNL    = text "<br />"
pSpec (Ref Internal r a)       = reflink     r $ pSpec a
pSpec (Ref (Cite2 EmptyS) r a) = reflink     r $ pSpec a -- no difference for citations?
pSpec (Ref (Cite2 n)   r a)    = reflinkInfo r (pSpec a) (pSpec n) -- no difference for citations?
pSpec (Ref External r a)       = reflinkURI  r $ pSpec a
pSpec EmptyS    = text "" -- Expected in the output
pSpec (Quote q) = doubleQuotes $ pSpec q
--pSpec (Acc Grave c) = text $ '&' : c : "grave;" --Only works on vowels.
--pSpec (Acc Acute c) = text $ '&' : c : "acute;" --Only works on vowels.


-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------


-- | Renders expressions in the HTML document (called by multiple functions).
pExpr :: Expr -> Doc
pExpr (Dbl d)        = text $ showEFloat Nothing d ""
pExpr (Int i)        = text $ show i
pExpr (Str s)        = doubleQuotes $ text s
pExpr (Row l)        = hcat $ map pExpr l
pExpr (Ident s)      = text s
pExpr (Label s)      = text s
pExpr (Spec s)       = text $ unPH $ L.special s
--pExpr (Gr g)         = unPH $ greek g
pExpr (Sub e)        = sub $ pExpr e
pExpr (Sup e)        = sup $ pExpr e
pExpr (Over Hat s)   = pExpr s <> text "&#770;"
pExpr (MO o)         = text $ pOps o
pExpr (Fenced l r e) = text (fence Open l) <> pExpr e <> text (fence Close r)
pExpr (Font Bold e)  = bold $ pExpr e
pExpr (Font Emph e)  = text "<em>" <> pExpr e <> text "</em>" -- FIXME
pExpr (Spc Thin)     = text "&#8239;"
-- Uses TeX for Mathjax for all other exprs
pExpr e              = mjDelimDisp $ printMath $ toMath $ TeX.pExpr e
  where mjDelimDisp d = text "\\(" <> d <> text "\\)"
-- Non-mathjax
{-
pExpr (Sqrt e)       = text "&radic;(" <> pExpr e <> text ")"
pExpr (Div a b)      = fraction (pExpr a) (pExpr b)
pExpr (Case ps)      = cases ps pExpr
pExpr (Mtx a)        = text "<table class=\"matrix\">\n" <> pMatrix a <> text "</table>"
-}

-- | Converts expression operators into HTML characters.
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
pOps Neg      = "&minus;"
pOps Cross    = "&#10799;"
pOps VAdd     = "&plus;"
pOps VSub     = "&minus;"
pOps Dot      = "&sdot;"
pOps Scale    = "&#8239;" -- same as Mul
pOps Eq       = " = " -- with spaces?
pOps NEq      = "&ne;"
pOps Lt       = "&thinsp;&lt;&thinsp;" --thin spaces make these more readable
pOps Gt       = "&thinsp;&gt;&thinsp;"
pOps LEq      = "&thinsp;&le;&thinsp;"
pOps GEq      = "&thinsp;&ge;&thinsp;"
pOps Impl     = " &rArr; "
pOps Iff      = " &hArr; "
pOps Subt     = "&minus;"
pOps And      = " &and; "
pOps Or       = " &or; "
pOps Add      = "&plus;"
pOps Mul      = "&#8239;"
pOps Summ     = "&sum;"
pOps Inte     = "&int;"
pOps Prod     = "&prod;"
pOps Point    = "."
pOps Perc     = "%"
pOps LArrow   = " &larr; "
pOps RArrow   = " &rarr; "
pOps ForAll   = " &forall; "
pOps Partial  = "&part;"

-- | Allows for open/closed variants of parenthesis, curly brackets, absolute value symbols, and normal symbols.
fence :: OpenClose -> Fence -> String
fence Open  Paren = "("
fence Close Paren = ")"
fence Open  Curly = "{"
fence Close Curly = "}"
fence _     Abs   = "|"
fence _     Norm  = "||"

-- Not used since we use MathJax handles this
-- pMatrix :: [[Expr]] -> Doc
-- pMatrix [] = text ""
-- pMatrix [x] = text "<tr>" <> pIn x <> text "</tr>\n"
-- pMatrix (x:xs) = pMatrix [x] <> pMatrix xs

-- Not used since we use MathJax handles this
-- pIn :: [Expr] -> Doc
-- pIn [] = text ""
-- pIn [x] = text "<td>" <> pExpr x <> text "</td>"
-- pIn (x:xs) = pIn [x] <> pIn xs

-----------------------------------------------------------------
------------------BEGIN TABLE PRINTING---------------------------
-----------------------------------------------------------------

-- | Renders an HTML table, called by 'printLO'.
makeTable :: Tags -> [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable _ [] _ _ _       = error "No table to print (see PrintHTML)"
makeTable ts (l:lls) r b t = refwrap r (table ts (
    tr (makeHeaderCols l) $$ makeRows lls) $$ if b then caption t else empty)

-- | Helper for creating table rows.
makeRows :: [[Spec]] -> Doc
makeRows = foldr (($$) . tr . makeColumns) empty

makeColumns, makeHeaderCols :: [Spec] -> Doc
-- | Helper for creating table header row (each of the column header cells).
makeHeaderCols = vcat . map (th . pSpec)

-- | Helper for creating table columns.
makeColumns = vcat . map (td . pSpec)

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.).
makeDefn :: L.DType -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _ [] _  = error "L.Empty definition"
makeDefn dt ps l = refwrap l $ table [dtag dt]
  (tr (th (text "Refname") $$ td (bold l)) $$ makeDRows ps)
  where dtag L.General  = "gdefn"
        dtag L.Instance = "idefn"
        dtag L.Theory   = "tdefn"
        dtag L.Data     = "ddefn"

-- | Helper for making the definition table rows.
makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows [(f,d)] = tr (th (text f) $$ td (vcat $ map printLO d))
makeDRows ((f,d):ps) = tr (th (text f) $$ td (vcat $ map printLO d)) $$ makeDRows ps

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists in HTML.
makeList :: ListType -> Doc -- FIXME: ref id's should be folded into the li
makeList (Simple items) = divTag ["list"] $
  vcat $ map (\(b,e,l) -> pa $ mlref l $ pSpec b <> text ": "
  <> pItem e) items
makeList (Desc items)   = divTag ["list"] $
  vcat $ map (\(b,e,l) -> pa $ mlref l $ ba $ pSpec b
  <> text ": " <> pItem e) items
makeList (Ordered items) = ol ["list"] (vcat $ map
  (li . \(i,l) -> mlref l $ pItem i) items)
makeList (Unordered items) = ul ["list"] (vcat $ map
  (li . \(i,l) -> mlref l $ pItem i) items)
makeList (Definitions items) = ul ["hide-list-style-no-indent"] $
  vcat $ map (\(b,e,l) -> li $ mlref l $ pSpec b <> text " is the"
  <+> pItem e) items

-- | Helper for setting up references.
mlref :: Maybe Label -> Doc -> Doc
mlref = maybe id $ refwrap . pSpec

-- | Helper for rendering list items.
pItem :: ItemType -> Doc
pItem (Flat s)     = pSpec s
pItem (Nested s l) = vcat [pSpec s, makeList l]

-----------------------------------------------------------------
------------------BEGIN FIGURE PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders figures in HTML.
makeFigure :: Doc -> Doc -> Doc -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refwrap r (image f c wp)

-- | Renders assumptions, requirements, likely changes.
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = li (refwrap l (i <> text ": " <> a))

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**

-- | Makes a bilbliography for the document.
makeBib :: BibRef -> Doc
makeBib = ul ["hide-list-style"] . vcat .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map renderCite

-- | For when we add other things to reference like website, newspaper
renderCite :: Citation -> (Doc, Doc)
renderCite (Cite e L.Book cfs)      = (text e, renderF cfs useStyleBk    <> text (sufxPrint cfs))
renderCite (Cite e L.Article cfs)   = (text e, renderF cfs useStyleArtcl <> text (sufxPrint cfs))
renderCite (Cite e L.MThesis cfs)   = (text e, renderF cfs useStyleBk    <> text (sufxPrint cfs))
renderCite (Cite e L.PhDThesis cfs) = (text e, renderF cfs useStyleBk    <> text (sufxPrint cfs))
renderCite (Cite e L.Misc cfs)      = (text e, renderF cfs useStyleBk)
renderCite (Cite e _ cfs)           = (text e, renderF cfs useStyleArtcl) --FIXME: Properly render these later.

-- | Render fields to be used in the document.
renderF :: [CiteField] -> (StyleGuide -> (CiteField -> Doc)) -> Doc
renderF fields styl = hsep $ map (styl bibStyleH) (sortBy compCiteField fields)

-- | Compares two cite fields.
compCiteField :: CiteField -> CiteField -> Ordering
compCiteField (Institution _) _ = LT
compCiteField _ (Institution _) = GT
compCiteField (Organization _) _ = LT
compCiteField _ (Organization _) = GT
compCiteField (Author     _) _ = LT
compCiteField _ (Author     _) = GT
compCiteField (Title      _) _ = LT
compCiteField _ (Title      _) = GT
compCiteField (Series     _) _ = LT
compCiteField _ (Series     _) = GT
compCiteField (BookTitle _) _  = LT
compCiteField _ (BookTitle _)  = GT
compCiteField (Editor     _) _ = LT
compCiteField _ (Editor     _) = GT
compCiteField (Journal    _) _ = LT
compCiteField _ (Journal    _) = GT
compCiteField (Volume     _) _ = LT
compCiteField _ (Volume     _) = GT
compCiteField (Number     _) _ = LT
compCiteField _ (Number     _) = GT
compCiteField (Edition    _) _ = LT
compCiteField _ (Edition    _) = GT
compCiteField (HowPublished (Verb _)) _ = LT
compCiteField _ (HowPublished (Verb _)) = GT
compCiteField (School     _) _ = LT
compCiteField _ (School     _) = GT
compCiteField (Address      _) _ = LT
compCiteField _ (Address      _) = GT
compCiteField (Publisher  _) _ = LT
compCiteField _ (Publisher  _) = GT
compCiteField (Month      _) _ = LT
compCiteField _ (Month      _) = GT
compCiteField (Year       _) _ = LT
compCiteField _ (Year       _) = GT
compCiteField (HowPublished (URL _)) _ = LT
compCiteField _ (HowPublished (URL _)) = GT
compCiteField (Chapter    _) _ = LT
compCiteField _ (Chapter    _) = GT
compCiteField (Pages      _) _ = LT
compCiteField _ (Pages      _) = GT
compCiteField (Note       _) _ = LT
compCiteField _ (Note       _) = GT
compCiteField (Type       _) _ = LT

-- Config helpers --
-- | Renders citation as a book style.
useStyleBk :: StyleGuide -> (CiteField -> Doc)
useStyleBk MLA     = bookMLA
useStyleBk APA     = bookAPA
useStyleBk Chicago = bookChicago

-- | Renders citation as an article style.
useStyleArtcl :: StyleGuide -> (CiteField -> Doc)
useStyleArtcl MLA     = artclMLA
useStyleArtcl APA     = artclAPA
useStyleArtcl Chicago = artclChicago

-- FIXME: move these show functions and use tags, combinators
-- | Cite books in MLA format.
bookMLA :: CiteField -> Doc
bookMLA (Address   s) = pSpec s <> text ":"
bookMLA (Edition   s) = comm $ text $ show s ++ sufxer s ++ " ed."
bookMLA (Series    s) = dot $ em $ pSpec s
bookMLA (Title     s) = dot $ em $ pSpec s --If there is a series or collection, this should be in quotes, not italics
bookMLA (Volume    s) = comm $ text $ "vol. " ++ show s
bookMLA (Publisher s) = comm $ pSpec s
bookMLA (Author    p) = dot $ pSpec (rendPeople' p)
bookMLA (Year      y) = dot $ text $ show y
--bookMLA (Date    d m y) = dot $ unwords [show d, show m, show y]
--bookMLA (URLdate d m y) = "Web. " ++ bookMLA (Date d m y) sm
bookMLA (BookTitle s) = dot $ em $ pSpec s
bookMLA (Journal   s) = comm $ em $ pSpec s
bookMLA (Pages   [p]) = dot $ text $ "pg. " ++ show p
bookMLA (Pages     p) = dot $ text "pp. " <> foldPages p
bookMLA (Note      s) = pSpec s
bookMLA (Number    n) = comm $ text ("no. " ++ show n)
bookMLA (School    s) = comm $ pSpec s
--bookMLA (Thesis     t)  = comm $ show t
--bookMLA (URL        s)  = dot $ pSpec s
bookMLA (HowPublished (Verb s))      = comm $ pSpec s
bookMLA (HowPublished (URL l@(S s))) = dot  $ pSpec $ Ref External s l
bookMLA (HowPublished (URL s))       = dot  $ pSpec s
bookMLA (Editor       p) = comm $ text "Edited by " <> foldPeople p
bookMLA (Chapter      _) = text ""
bookMLA (Institution  i) = comm $ pSpec i
bookMLA (Organization i) = comm $ pSpec i
bookMLA (Month        m) = comm $ text $ show m
bookMLA (Type         t) = comm $ pSpec t

-- | Cite books in APA format.
bookAPA :: CiteField -> Doc --FIXME: year needs to come after author in L.APA
bookAPA (Author   p) = pSpec (rendPeople L.rendPersLFM' p) --L.APA uses initals rather than full name
bookAPA (Year     y) = dot $ text $ paren $ show y --L.APA puts "()" around the year
--bookAPA (Date _ _ y) = bookAPA (Year y) --L.APA doesn't care about the day or month
--bookAPA (URLdate d m y) = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA (Pages    p) = dot $ foldPages p
bookAPA (Editor   p) = dot $ foldPeople p <> text " (Ed.)"
bookAPA i = bookMLA i --Most items are rendered the same as L.MLA

-- | Cite books in Chicago format.
bookChicago :: CiteField -> Doc
bookChicago (Author   p) = pSpec (rendPeople L.rendPersLFM'' p) --L.APA uses middle initals rather than full name
bookChicago (Pages    p) = dot $ foldPages p
bookChicago (Editor   p) = dot $ foldPeople p <> text (toPlural p " ed")
bookChicago i = bookMLA i --Most items are rendered the same as L.MLA

-- for article renderings
-- | Cite articles in MLA format.
artclMLA :: CiteField -> Doc
artclMLA (Title s) = doubleQuotes $ dot $ pSpec s
artclMLA i         = bookMLA i

-- | Cite articles in APA format.
artclAPA :: CiteField -> Doc
artclAPA (Title  s)  = dot $ pSpec s
artclAPA (Volume n)  = em $ text $ show n
artclAPA (Number  n) = comm $ text $ paren $ show n
artclAPA i           = bookAPA i

-- | Cite articles in Chicago format.
artclChicago :: CiteField -> Doc
artclChicago i@(Title    _) = artclMLA i
artclChicago (Volume     n) = comm $ text $ show n
artclChicago (Number      n) = text $ "no. " ++ show n
artclChicago i@(Year     _) = bookAPA i
--artclChicago i@(Date _ _ _) = bookAPA i
artclChicago i = bookChicago i

-- PEOPLE RENDERING --
-- | Render a list of people (after applying a given function).
rendPeople :: (L.Person -> String) -> L.People -> Spec
rendPeople _ []  = S "N.a." -- "No authors given"
rendPeople f people = S . foldlList $ map f people --foldlList is in drasil-utils

-- | Render a list of people (of form FirstName LastName).
rendPeople' :: L.People -> Spec
rendPeople' []  = S "N.a." -- "No authors given"
rendPeople' people = S . foldlList $ map rendPers (init people) ++  [rendPersL (last people)]

-- | Organize a list of pages.
foldPages :: [Int] -> Doc
foldPages = text . foldlList . L.numList "&ndash;"

-- | Organize a list of people.
foldPeople :: L.People -> Doc
foldPeople p = text . foldlList $ map L.nameStr p

-- | Organize a list of Strings, separated by commas and inserting "and" before the last item.
foldlList :: [String] -> String
foldlList []    = ""
foldlList [a,b] = a ++ " and " ++ b
foldlList lst   = foldle1 (\a b -> a ++ ", " ++ b) (\a b -> a ++ ", and " ++ b) lst

-- | Similar to foldl, but applies a function to two arguments at a time.
foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g (f x y : xs)

-- | Renders a 'Person' as Last, First Middle.
rendPers :: L.Person -> String
rendPers = L.rendPersLFM

-- | Renders a person's last name.
rendPersL :: L.Person -> String
rendPersL =
  (\n -> (if not (null n) && last n == '.' then init else id) n) . rendPers

-- | adds an 's' if there is more than one person in a list.
toPlural :: L.People -> String -> String
toPlural (_:_) str = str ++ "s"
toPlural _     str = str
