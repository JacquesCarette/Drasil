module Language.Drasil.HTML.Print(genHTML) where

import Prelude hiding (print, (<>))
import Data.List (intercalate, partition, sortBy)
import Text.PrettyPrint hiding (Str)
import Numeric (showEFloat)
import Control.Arrow (second)
import Utils.Drasil (checkValidStr, numList)

import qualified Language.Drasil as L (People, Person, 
  CitationKind(Misc, Book, MThesis, PhDThesis, Article), 
  Symbol(..), DType(Data, Theory, Instance, General), MaxWidthPercent,
  Decoration(Prime, Hat, Vector), Document,
  nameStr, rendPersLFM, rendPersLFM', rendPersLFM'', special, USymb(US))

import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Helpers (articleTitle, author, ba, body, bold,
  caption, divTag, em, h, headTag, html, image, li, ol, pa,
  paragraph, reflink, reflinkInfo, reflinkURI, refwrap, sub, sup, table, td,
  th, title, tr, ul)
import qualified Language.Drasil.Output.Formats as F
import Language.Drasil.HTML.CSS (linkCSS)

import Language.Drasil.Config (StyleGuide(APA, MLA, Chicago), bibStyleH)
import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, Fence(Curly, Paren, Abs, Norm),
  Ops(Prod, Inte, Mul, Summ, Or, Add, And, Subt, Iff, Impl, GEq, LEq, Lt, Gt, NEq, Eq,
  Dot, Cross, Neg, Exp, Not, Dim, Arctan, Arccos, Arcsin, Cot, Csc, Sec, Tan, 
  Cos, Sin, Log, Ln, Prime, Comma, Boolean, 
  Real, Rational, Natural, Integer, IsIn, Point, Perc), 
  Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, Sp, Sy, S, E, (:+:)),
  Spacing(Thin), Fonts(Bold, Emph), OverSymb(Hat), Label,
  LinkType(Internal, Cite2, External))
import Language.Drasil.Printing.Citation (CiteField(Year, Number, Volume, Title, Author, 
  Editor, Pages, Type, Month, Organization, Institution, Chapter, HowPublished, School, Note,
  Journal, BookTitle, Publisher, Series, Address, Edition), HP(URL, Verb), 
  Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..), Tags)
import Language.Drasil.Printing.Helpers (comm, dot, paren, sufxer, sqbrac, dollarDoc)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import qualified Language.Drasil.TeX.Print as TeX (pExpr, spec)
import Language.Drasil.TeX.Monad (runPrint, MathContext(Text), D, toMath)

data OpenClose = Open | Close

-- | Generate an HTML document from a Drasil 'Document'
genHTML :: PrintingInformation -> F.Filename -> L.Document -> Doc
genHTML sm fn doc = build fn (makeDocument sm doc)

-- | Build the HTML Document, called by genHTML
build :: String -> Document -> Doc
build fn (Document t a c) =
  text "<!DOCTYPE html>" $$
  html (headTag (linkCSS fn $$ title (titleSpec t) $$
  text "<meta charset=\"utf-8\">" $$
  text ("<script type=\"text/x-mathjax-config\">" ++
    "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}, displayMath: [['$$','$$']]});" ++
    "</script>") $$
  text ("<script type=\"text/javascript\" async " ++
  "src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/latest.js?config=TeX-MML-AM_CHTML\">" ++
  "</script>")) $$
  body (articleTitle (pSpec t) $$ author (pSpec a)
  $$ print c
  ))

-- Helper for rendering a D from Latex print
printMath :: D -> Doc
printMath = (`runPrint` Text)

-- | Helper for rendering LayoutObjects into HTML
printLO :: LayoutObj -> Doc
printLO (HDiv ["equation"] layoutObs EmptyS)  = vcat (map printLO layoutObs)
-- Dollar Doc needed to wrap in extra dollar signs.
-- Latex print sets up a \begin{displaymath} environment instead of this
printLO (EqnBlock contents)    = dollarDoc $ printMath $ TeX.spec contents
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


-- | Called by build, uses 'printLO' to render the layout
-- objects in Doc format.
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

-- | Renders the Sentences in the HTML body (called by 'printLO')
pSpec :: Spec -> Doc
-- Non-mathjax
pSpec (E e)  = em $ pExpr e
pSpec (Sy s) = text $ uSymb s
-- Latex based math for expressions and units
-- pSpec (E e)     = printMath $ toMath $ TeX.pExpr e
-- pSpec (Sy s)    = printMath $ TeX.pUnit s
pSpec (a :+: b) = pSpec a <> pSpec b
pSpec (S s)     = either error (text . concatMap escapeChars) $ checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Sp s)    = text $ unPH $ L.special s
pSpec HARDNL    = text "<br />"
pSpec (Ref Internal r a)      = reflink     r $ pSpec a
pSpec (Ref Cite2    r EmptyS) = reflink     r $ text r -- no difference for citations?
pSpec (Ref Cite2    r a)      = reflinkInfo r (text r) (pSpec a) -- no difference for citations?
pSpec (Ref External r a)      = reflinkURI  r $ pSpec a
pSpec EmptyS    = text "" -- Expected in the output
pSpec (Quote q) = doubleQuotes $ pSpec q
--pSpec (Acc Grave c) = text $ '&' : c : "grave;" --Only works on vowels.
--pSpec (Acc Acute c) = text $ '&' : c : "acute;" --Only works on vowels.


-- | Renders symbols for HTML document
symbol :: L.Symbol -> String
symbol (L.Variable s) = s
symbol (L.Label    s) = s
symbol (L.Integ    n) = show n
symbol (L.Special  s) = unPH $ L.special s
symbol (L.Concat  sl) = concatMap symbol sl
--symbol (Greek g)      = unPH $ greek g
-- handle the special cases first, then general case
symbol (L.Corners [] [] [x] [] s) = symbol s ++ (render . sup . text) (symbol x)
symbol (L.Corners [] [] [] [x] s) = symbol s ++ (render . sub . text) (symbol x)
symbol (L.Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (L.Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol L.Corners{}                = error "rendering of L.Corners (general)"
symbol (L.Atop L.Vector s)        = "<b>" ++ symbol s ++ "</b>"
symbol (L.Atop L.Hat s)           = symbol s ++ "&#770;"
symbol (L.Atop L.Prime s)         = symbol s ++ "&prime;"
symbol L.Empty                    = ""

uSymb :: L.USymb -> String
uSymb (L.US ls) = formatu t b
  where
    (t,b) = partition ((> 0) . snd) ls
    formatu :: [(L.Symbol,Integer)] -> [(L.Symbol,Integer)] -> String
    formatu [] l = line l
    formatu l [] = intercalate "&sdot;" $ map pow l
    formatu nu de = line nu ++ "/" ++ line (map (second negate) de)
    line :: [(L.Symbol,Integer)] -> String
    line []  = ""
    line [x] = pow x
    line l   = "(" ++ intercalate "&sdot;" (map pow l) ++ ")"
    pow :: (L.Symbol,Integer) -> String
    pow (x,1) = symbol x
    pow (x,p) = symbol x ++ (render . sup . text) (show p)

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------


-- | Renders expressions in the HTML (called by multiple functions)
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
pExpr e              = printMath $ toMath $ TeX.pExpr e
-- Non-mathjax
{-
pExpr (Sqrt e)       = text "&radic;(" <> pExpr e <> text ")"
pExpr (Div a b)      = fraction (pExpr a) (pExpr b)
pExpr (Case ps)      = cases ps pExpr
pExpr (Mtx a)        = text "<table class=\"matrix\">\n" <> pMatrix a <> text "</table>"
-}

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
pOps Dot      = "&sdot;"
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

-- | Renders HTML table, called by 'printLO'
makeTable :: Tags -> [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable _ [] _ _ _       = error "No table to print (see PrintHTML)"
makeTable ts (l:lls) r b t = refwrap r (table ts (
    tr (makeHeaderCols l) $$ makeRows lls) $$ if b then caption t else empty)

-- | Helper for creating table rows
makeRows :: [[Spec]] -> Doc
makeRows = foldr (($$) . tr . makeColumns) empty

makeColumns, makeHeaderCols :: [Spec] -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols = vcat . map (th . pSpec)

-- | Helper for creating table columns
makeColumns = vcat . map (td . pSpec)

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: L.DType -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _ [] _  = error "L.Empty definition"
makeDefn dt ps l = refwrap l $ table [dtag dt]
  (tr (th (text "Refname") $$ td (bold l)) $$ makeDRows ps)
  where dtag L.General  = "gdefn"
        dtag L.Instance = "idefn"
        dtag L.Theory   = "tdefn"
        dtag L.Data     = "ddefn"

-- | Helper for making the definition table rows
makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows [(f,d)] = tr (th (text f) $$ td (vcat $ map printLO d))
makeDRows ((f,d):ps) = tr (th (text f) $$ td (vcat $ map printLO d)) $$ makeDRows ps

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists
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

-- | Helper for setting up references
mlref :: Maybe Label -> Doc -> Doc
mlref = maybe id $ refwrap . pSpec

-- | Helper for rendering list items
pItem :: ItemType -> Doc
pItem (Flat s)     = pSpec s
pItem (Nested s l) = vcat [pSpec s, makeList l]

-----------------------------------------------------------------
------------------BEGIN FIGURE PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders figures in HTML
makeFigure :: Doc -> Doc -> Doc -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refwrap r (image f c wp)

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = li (refwrap l (i <> text ": " <> a))

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**

makeBib :: BibRef -> Doc
makeBib = ul ["hide-list-style"] . vcat .
  map (\(x,(y,z)) -> makeRefList z y x) .
  zip [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map renderCite

--for when we add other things to reference like website, newspaper
renderCite :: Citation -> (Doc, Doc)
renderCite (Cite e L.Book cfs)      = (text e, renderF cfs useStyleBk    <> text " Print.")
renderCite (Cite e L.Article cfs)   = (text e, renderF cfs useStyleArtcl <> text " Print.")
renderCite (Cite e L.MThesis cfs)   = (text e, renderF cfs useStyleBk    <> text " Print.")
renderCite (Cite e L.PhDThesis cfs) = (text e, renderF cfs useStyleBk    <> text " Print.")
renderCite (Cite e L.Misc cfs)      = (text e, renderF cfs useStyleBk)
renderCite (Cite e _ cfs)           = (text e, renderF cfs useStyleArtcl) --FIXME: Properly render these later.

renderF :: [CiteField] -> (StyleGuide -> (CiteField -> Doc)) -> Doc
renderF fields styl = hsep $ map (styl bibStyleH) (sortBy compCiteField fields)

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
useStyleBk :: StyleGuide -> (CiteField -> Doc)
useStyleBk MLA     = bookMLA
useStyleBk APA     = bookAPA
useStyleBk Chicago = bookChicago

useStyleArtcl :: StyleGuide -> (CiteField -> Doc)
useStyleArtcl MLA     = artclMLA
useStyleArtcl APA     = artclAPA
useStyleArtcl Chicago = artclChicago

-- FIXME: move these show functions and use tags, combinators
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

bookAPA :: CiteField -> Doc --FIXME: year needs to come after author in L.APA
bookAPA (Author   p) = pSpec (rendPeople L.rendPersLFM' p) --L.APA uses initals rather than full name
bookAPA (Year     y) = dot $ text $ paren $ show y --L.APA puts "()" around the year
--bookAPA (Date _ _ y) = bookAPA (Year y) --L.APA doesn't care about the day or month
--bookAPA (URLdate d m y) = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA (Pages    p) = dot $ foldPages p
bookAPA (Editor   p) = dot $ foldPeople p <> text " (Ed.)"
bookAPA i = bookMLA i --Most items are rendered the same as L.MLA

bookChicago :: CiteField -> Doc
bookChicago (Author   p) = pSpec (rendPeople L.rendPersLFM'' p) --L.APA uses middle initals rather than full name
bookChicago (Pages    p) = dot $ foldPages p
bookChicago (Editor   p) = dot $ foldPeople p <> text (toPlural p " ed")
bookChicago i = bookMLA i --Most items are rendered the same as L.MLA

-- for article renderings
artclMLA :: CiteField -> Doc
artclMLA (Title s) = doubleQuotes $ dot $ pSpec s
artclMLA i         = bookMLA i

artclAPA :: CiteField -> Doc
artclAPA (Title  s)  = dot $ pSpec s
artclAPA (Volume n)  = em $ text $ show n
artclAPA (Number  n) = comm $ text $ paren $ show n
artclAPA i           = bookAPA i

artclChicago :: CiteField -> Doc
artclChicago i@(Title    _) = artclMLA i
artclChicago (Volume     n) = comm $ text $ show n
artclChicago (Number      n) = text $ "no. " ++ show n
artclChicago i@(Year     _) = bookAPA i
--artclChicago i@(Date _ _ _) = bookAPA i
artclChicago i = bookChicago i

-- PEOPLE RENDERING --
rendPeople :: (L.Person -> String) -> L.People -> Spec
rendPeople _ []  = S "N.a." -- "No authors given"
rendPeople f people = S . foldlList $ map f people --foldlList is in drasil-utils

rendPeople' :: L.People -> Spec
rendPeople' []  = S "N.a." -- "No authors given"
rendPeople' people = S . foldlList $ map rendPers (init people) ++  [rendPersL (last people)]

foldPages :: [Int] -> Doc
foldPages = text . foldlList . numList "&ndash;"

foldPeople :: L.People -> Doc
foldPeople p = text . foldlList $ map L.nameStr p

foldlList :: [String] -> String
foldlList []    = ""
foldlList [a,b] = a ++ " and " ++ b
foldlList lst   = foldle1 (\a b -> a ++ ", " ++ b) (\a b -> a ++ ", and " ++ b) lst

foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g (f x y : xs)

-- LFM is Last, First Middle
rendPers :: L.Person -> String
rendPers = L.rendPersLFM

-- To render the last person's name
rendPersL :: L.Person -> String
rendPersL =
  (\n -> (if not (null n) && last n == '.' then init else id) n) . rendPers

--adds an 's' if there is more than one person in a list
toPlural :: L.People -> String -> String
toPlural (_:_) str = str ++ "s"
toPlural _     str = str
