module Language.Drasil.HTML.Print(genHTML) where

import Prelude hiding (print)
import Data.List (sortBy,partition,intersperse)
import Text.PrettyPrint hiding (render, Str)
import Numeric (showFFloat)
import Control.Arrow (second)

import qualified Language.Drasil as L (People, Person, StyleGuide(APA, MLA, Chicago), 
  CitationKind(Misc, Book, MThesis, PhDThesis, Article), 
  Symbol(Corners, Concat, Special, Atomic, Empty, Atop), USymb(US),
  DType(DD, TM, Instance, General, Theory, Data), MaxWidthPercent, 
  Decoration(Prime, Hat, Vector), Document, HasSymbolTable, nameStr, 
  rendPersLFM, rendPersLFM', rendPersLFM'', special, bibStyleH)

--import Language.Drasil hiding ((:+:), S, EmptyS, Year, Number, And, Add, Subt, Or, Expr,
  --CiteField, Title, Volume, School, HowPublished, Editor, Pages, Author, Iff, Impl, GEq,
  --Eq, NEq, LEq, Gt, title, Organization, Type, URL, Verb, Institution, Chapter, Month, Note,
  --Journal, Series, Edition, Publisher, Address, Type, BookTitle, Cite, ListType, Simple,
  --Citation, BibRef, Nested, Flat, ItemType, Definition, Definitions, Table, EqnBlock, Figure,
  --Desc, Norm, Abs, Mul, Lt, Sqrt, Sub, Case, Str, Int, Dbl, Quote, Ref, Sp, Sy, E, Neg, Dot, 
  --Cross, Exp, symbol)

import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Helpers (em, wrap, refwrap, caption, image, div_tag,
  td, th, tr, bold, sub, sup, cases, fraction, reflink, paragraph, h, html, body,
  author, article_title, title, linkCSS, head_tag)
import qualified Language.Drasil.Output.Formats as F

import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, Fence(Curly, Paren, Abs, Norm),
  Ops(Prod, Inte, Mul, Summ, Or, Add, And, Subt, Iff, Impl, GEq, LEq, Lt, Gt, NEq, Eq,
  Dot, Cross, Neg, Exp, Not, Dim, Cot, Csc, Sec, Tan, Cos, Sin, Log, Ln, Prime, Comma, Boolean, 
  Real, Rational, Natural, Integer, IsIn), 
  Expr(Sub, Sup, Over, Sqrt, Spc, Font, MO, Fenced, Spec, Ident, Row, Mtx, Case, Div, Str, 
  Int, Dbl), Spec(Quote, EmptyS, Ref, HARDNL, Sp, Sy, S, E, (:+:)),
  Spacing(Thin), Fonts(Bold, Emph), OverSymb(Hat), Label)
import Language.Drasil.Printing.Citation (CiteField(Year, Number, Volume, Title, Author, 
  Editor, Pages, Type, Month, Organization, Institution, Chapter, HowPublished, School, Note,
  Journal, BookTitle, Publisher, Series, Address, Edition), HP(URL, Verb), 
  Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (Tags, ALUR, Document(Document),
  LayoutObj(Graph, Bib, List, Header, Figure, Definition, Table, EqnBlock, Paragraph, 
  HDiv, ALUR))
import Language.Drasil.Printing.Helpers (comm, dot, paren, sufxer, sqbrac)

{-
import Language.Drasil.Development.UnitLang (L.USymb(L.US))
import Language.Drasil.Unicode (special)
import           Language.Drasil.L.Symbol (L.Symbol(..))
import qualified Language.Drasil.L.Symbol as S
import qualified Language.Drasil.Document as L
import Language.Drasil.L.People (L.People, L.Person(..), rendPersLFM', rendPersLFM'',
  nameStr, rendPersLFM)
import Language.Drasil.Config (L.StyleGuide(..), bibStyleH)
import Language.Drasil.ChunkDB(HasSymbolTable)

import Language.Drasil.Chunk.Citation (CitationKind(..))
-}

data OpenClose = Open | Close

-- | Generate an HTML document from a Drasil 'Document'
genHTML :: L.HasSymbolTable ctx => ctx -> F.Filename -> L.Document -> Doc
genHTML sm fn doc = build fn (makeDocument sm doc)

-- | Build the HTML Document, called by genHTML
build :: String -> Document -> Doc
build fn (Document t a c) =
  text ( "<!DOCTYPE html>") $$
  html ( head_tag ((linkCSS fn) $$ title (title_spec t) $$
  text ("<meta charset=\"utf-8\">") $$
  text ("<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/"++
          "2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>")) $$
  body (article_title (p_spec t) $$ author (p_spec a)
  $$ print c
  ))

-- | Helper for rendering LayoutObjects into HTML
printLO :: LayoutObj -> Doc
printLO (HDiv ts layoutObs l)  = refwrap (p_spec l) $
                                 div_tag ts (vcat (map printLO layoutObs))
printLO (Paragraph contents)   = paragraph $ p_spec contents
printLO (EqnBlock contents)    = p_spec contents
printLO (Table ts rows r b t)  = makeTable ts rows (p_spec r) b (p_spec t)
printLO (Definition dt ssPs l) = makeDefn dt ssPs (p_spec l)
printLO (Header n contents _)  = h (n + 1) $ p_spec contents -- FIXME
printLO (List t)               = makeList t
printLO (Figure r c f wp)      = makeFigure (p_spec r) (p_spec c) (text f) wp
printLO (ALUR _ x l i)         = wrap "ul" ["hide-list-style"] $
  makeRefList (p_spec x) (p_spec l) (p_spec i)
printLO (Bib bib)              = makeBib bib
printLO (Graph _ _ _ _ _)      = empty -- FIXME


-- | Called by build, uses 'printLO' to render the layout
-- objects in Doc format.
print :: [LayoutObj] -> Doc
print l = foldr ($$) empty $ map printLO l

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders the title of the document. Different than body rendering
-- because newline can't be rendered in an HTML title.
title_spec :: Spec -> Doc
title_spec (a :+: b) = title_spec a <> title_spec b
title_spec HARDNL    = empty
title_spec s         = p_spec s

-- | Renders the Sentences in the HTML body (called by 'printLO')
p_spec :: Spec -> Doc
p_spec (E e)             = em $ text $ p_expr e
p_spec (a :+: b)         = p_spec a <> p_spec b
p_spec (S s)             = text s
p_spec (Sy s)            = text $ uSymb s
p_spec (Sp s)            = text $ unPH $ L.special s
p_spec HARDNL            = text "<br />"
p_spec (Ref _ r a _)    = reflink r $ p_spec a
p_spec EmptyS            = text "" -- Expected in the output
p_spec (Quote q)         = text "&quot;" <> p_spec q <> text "&quot;"
-- p_spec (Acc Grave c)     = text $ '&' : c : "grave;" --Only works on vowels.
-- p_spec (Acc Acute c)     = text $ '&' : c : "acute;" --Only works on vowels.


-- | Renders symbols for HTML document
symbol :: L.Symbol -> String
symbol (L.Atomic s)  = s
symbol (L.Special s) = unPH $ L.special s
symbol (L.Concat sl) = foldr (++) "" $ map symbol sl
--symbol (Greek g)   = unPH $ greek g
-- handle the special cases first, then general case
symbol (L.Corners [] [] [x] [] s) = (symbol s) ++ sup (symbol x)
symbol (L.Corners [] [] [] [x] s) = (symbol s) ++ sub (symbol x)
symbol (L.Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (L.Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (L.Corners _ _ _ _ _)      = error "rendering of L.Corners (general)"
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
    formatu l [] = concat $ intersperse "&sdot;" $ map pow l
    formatu nu de = line nu ++ "/" ++ (line $ map (second negate) de)
    line :: [(L.Symbol,Integer)] -> String
    line []  = ""
    line [x] = pow x
    line l   = '(' : (concat $ intersperse "&sdot;" $ map pow l) ++ ")"
    pow :: (L.Symbol,Integer) -> String
    pow (x,1) = symbol x
    pow (x,p) = symbol x ++ sup (show p)

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- | Renders expressions in the HTML (called by multiple functions)
p_expr :: Expr -> String
p_expr (Dbl d)        = showFFloat Nothing d ""
p_expr (Int i)        = show i
p_expr (Str s)        = s
p_expr (Div a b)      = fraction (p_expr a) (p_expr b)
p_expr (Case ps)      = cases ps p_expr
p_expr (Mtx a)        = "<table class=\"matrix\">\n" ++ p_matrix a ++ "</table>"
p_expr (Row l)        = concatMap p_expr l
p_expr (Ident s)      = s
p_expr (Spec s)       = unPH $ L.special s
--p_expr (Gr g)         = unPH $ greek g
p_expr (Sub e)        = sub $ p_expr e
p_expr (Sup e)        = sup $ p_expr e
p_expr (Over Hat s)   = p_expr s ++ "&#770;"
p_expr (MO o)         = p_ops o
p_expr (Fenced l r e) = fence Open l ++ p_expr e ++ fence Close r
p_expr (Font Bold e)  = bold $ p_expr e
p_expr (Font Emph e)  = "<em>" ++ p_expr e ++ "</em>" -- FIXME
p_expr (Spc Thin)     = "&#8239;"
p_expr (Sqrt e)       = "&radic;(" ++ p_expr e ++")"

p_ops :: Ops -> String
p_ops IsIn     = "&thinsp;&isin;&thinsp;"
p_ops Integer  = "&#8484;"
p_ops Rational = "&#8474;"
p_ops Real     = "&#8477;"
p_ops Natural  = "&#8469;"
p_ops Boolean  = "&#120121;"
p_ops Comma    = ","
p_ops Prime    = "&prime;"
p_ops Log      = "log"
p_ops Ln       = "ln"
p_ops Sin      = "sin"
p_ops Cos      = "cos"
p_ops Tan      = "tan"
p_ops Sec      = "sec"
p_ops Csc      = "csc"
p_ops Cot      = "cot"
p_ops Not      = "&not;"
p_ops Dim      = "dim"
p_ops Exp      = "e"
p_ops Neg      = "&minus;"
p_ops Cross    = "&#10799;"
p_ops Dot      = "&sdot;"
p_ops Eq       = " = " -- with spaces?
p_ops NEq      = "&ne;"
p_ops Lt       = "&thinsp;&lt;&thinsp;" --thin spaces make these more readable
p_ops Gt       = "&thinsp;&gt;&thinsp;"
p_ops LEq      = "&thinsp;&le;&thinsp;"
p_ops GEq      = "&thinsp;&ge;&thinsp;"
p_ops Impl     = " &rArr; "
p_ops Iff      = " &hArr; "
p_ops Subt     = "&minus;"
p_ops And      = " &and; "
p_ops Or       = " &or; "
p_ops Add      = "&plus;"
p_ops Mul      = "&#8239;"
p_ops Summ     = "&sum;"
p_ops Inte     = "&int;"
p_ops Prod     = "&prod;"

fence :: OpenClose -> Fence -> String
fence Open  Paren = "("
fence Close Paren = ")"
fence Open  Curly = "{"
fence Close Curly = "}"
fence _     Abs   = "|"
fence _     Norm  = "||"

-- | For printing Matrix
p_matrix :: [[Expr]] -> String
p_matrix [] = ""
p_matrix [x] = "<tr>" ++ p_in x ++ "</tr>\n"
p_matrix (x:xs) = p_matrix [x] ++ p_matrix xs

p_in :: [Expr] -> String
p_in [] = ""
p_in [x] = "<td>" ++ p_expr x ++ "</td>"
p_in (x:xs) = p_in [x] ++ p_in xs

-----------------------------------------------------------------
------------------BEGIN TABLE PRINTING---------------------------
-----------------------------------------------------------------

-- | Renders HTML table, called by 'printLO'
makeTable :: Tags -> [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable _ [] _ _ _       = error "No table to print (see PrintHTML)"
makeTable ts (l:lls) r b t = refwrap r (wrap "table" ts (
    tr (makeHeaderCols l) $$ makeRows lls) $$ if b then caption t else empty)

-- | Helper for creating table rows
makeRows :: [[Spec]] -> Doc
makeRows = foldr ($$) empty . map (tr . makeColumns)

makeColumns, makeHeaderCols :: [Spec] -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols = vcat . map (th . p_spec)

-- | Helper for creating table columns
makeColumns = vcat . map (td . p_spec)

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: L.DType -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _ [] _  = error "L.Empty definition"
makeDefn dt ps l = refwrap l $ wrap "table" [dtag dt] (makeDRows ps)
  where dtag (L.Data _)   = "ddefn"
        dtag (L.Theory _) = "tdefn"
        dtag (L.General)  = "gdefn"
        dtag (L.Instance) = "idefn"
        dtag (L.TM)       = "tdefn"
        dtag (L.DD)       = "ddefn"

-- | Helper for making the definition table rows
makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows ((f,d):[]) = tr (th (text f) $$ td (vcat $ map printLO d))
makeDRows ((f,d):ps) = tr (th (text f) $$ td (vcat $ map printLO d)) $$ makeDRows ps

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists
makeList :: ListType -> Doc -- FIXME: ref id's should be folded into the li
makeList (Simple items) = div_tag ["list"] $
  vcat $ map (\(b,e,l) -> wrap "p" [] $ mlref l $ p_spec b <> text ": "
   <> p_item e) items
makeList (Desc items)   = div_tag ["list"] $
  vcat $ map (\(b,e,l) -> wrap "p" [] $ mlref l $ wrap "b" [] $ p_spec b
   <> text ": " <> p_item e) items
makeList (Ordered items) = wrap "ol" ["list"] (vcat $ map
  (wrap "li" [] . \(i,l) -> mlref l $ p_item i) items)
makeList (Unordered items) = wrap "ul" ["list"] (vcat $ map
  (wrap "li" [] . \(i,l) -> mlref l $ p_item i) items)
makeList (Definitions items) = wrap "ul" ["hide-list-style-no-indent"] $
  vcat $ map (\(b,e,l) -> wrap "li" [] $ mlref l $ p_spec b <> text " is the"
   <+> p_item e) items

-- | Helper for setting up references
mlref :: Maybe Label -> Doc -> Doc
mlref = maybe id $ refwrap . p_spec

-- | Helper for rendering list items
p_item :: ItemType -> Doc
p_item (Flat s)     = p_spec s
p_item (Nested s l) = vcat [p_spec s, makeList l]

-----------------------------------------------------------------
------------------BEGIN FIGURE PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders figures in HTML
makeFigure :: Doc -> Doc -> Doc -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refwrap r (image f c wp $$ caption c)

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = wrap "li" [] (refwrap l (i <> text ": " <> a))

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**

makeBib :: BibRef -> Doc
makeBib = wrap "ul" ["hide-list-style"] . vcat .
  map (\(x,(y,z)) -> makeRefList z y x) .
  zip [text $ sqbrac $ show x | x <- ([1..] :: [Int])] . map renderCite

--for when we add other things to reference like website, newspaper
renderCite :: Citation -> (Doc, Doc)
renderCite (Cite e L.Book cfs) = (text e, renderF cfs useStyleBk <> text " Print.")
renderCite (Cite e L.Article cfs) = (text e, renderF cfs useStyleArtcl <> text " Print.")
renderCite (Cite e L.MThesis cfs) = (text e, renderF cfs useStyleBk <> text " Print.")
renderCite (Cite e L.PhDThesis cfs) = (text e, renderF cfs useStyleBk <> text " Print.")
renderCite (Cite e L.Misc cfs) = (text e, renderF cfs useStyleBk)
renderCite (Cite e _ cfs) = (text e, renderF cfs useStyleArtcl) --FIXME: Properly render these later.

renderF :: [CiteField] -> (L.StyleGuide -> (CiteField -> Doc)) -> Doc
renderF fields styl = hsep $ map (styl L.bibStyleH) (sortBy compCiteField fields)

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
useStyleBk :: L.StyleGuide -> (CiteField -> Doc)
useStyleBk L.MLA     = bookMLA
useStyleBk L.APA     = bookAPA
useStyleBk L.Chicago = bookChicago

useStyleArtcl :: L.StyleGuide -> (CiteField -> Doc)
useStyleArtcl L.MLA     = artclMLA
useStyleArtcl L.APA     = artclAPA
useStyleArtcl L.Chicago = artclChicago

-- FIXME: move these show functions and use tags, combinators
bookMLA :: CiteField -> Doc
bookMLA (Address s)     = p_spec s <> text ":"
bookMLA (Edition    s)  = comm $ text $ show s ++ sufxer s ++ " ed."
bookMLA (Series     s)  = dot $ em $ p_spec s
bookMLA (Title      s)  = dot $ em $ p_spec s --If there is a series or collection, this should be in quotes, not italics
bookMLA (Volume     s)  = comm $ text $ "vol. " ++ show s
bookMLA (Publisher  s)  = comm $ p_spec s
bookMLA (Author     p)  = dot $ p_spec (rendPeople' p)
bookMLA (Year       y)  = dot $ text $ show y
--bookMLA (Date    d m y) = dot $ unwords [show d, show m, show y]
--bookMLA (URLdate d m y) = "Web. " ++ bookMLA (Date d m y) sm
bookMLA (BookTitle s)   = dot $ em $ p_spec s
bookMLA (Journal    s)  = comm $ em $ p_spec s
bookMLA (Pages      [n]) = dot $ text $ "p. " ++ show n
bookMLA (Pages  (a:b:[])) = dot $ text $ "pp. " ++ show a ++ "&ndash;" ++ show b
bookMLA (Pages _) = error "Page range specified is empty or has more than two items"
bookMLA (Note       s)    = p_spec s
bookMLA (Number      n)   = comm $ text $ ("no. " ++ show n)
bookMLA (School     s)    = comm $ p_spec s
--bookMLA (Thesis     t)  = comm $ show t
--bookMLA (URL        s)  = dot $ p_spec s
bookMLA (HowPublished (Verb s)) = comm $ p_spec s
bookMLA (HowPublished (URL s))  = dot $ p_spec s
bookMLA  (Editor     p)    = comm $ text "Edited by " <> p_spec (foldlList (map (S . L.nameStr) p))
bookMLA (Chapter _)       = text ""
bookMLA (Institution i)   = comm $ p_spec i
bookMLA (Organization i)  = comm $ p_spec i
bookMLA (Month m)         = comm $ text $ show m
bookMLA (Type t)          = comm $ p_spec t

bookAPA :: CiteField -> Doc --FIXME: year needs to come after author in L.APA
bookAPA (Author   p) = p_spec (rendPeople L.rendPersLFM' p) --L.APA uses initals rather than full name
bookAPA (Year     y) = dot $ text $ paren $ show y --L.APA puts "()" around the year
--bookAPA (Date _ _ y) = bookAPA (Year y) --L.APA doesn't care about the day or month
--bookAPA (URLdate d m y) = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA (Pages     [n])  = dot $ text $ show n
bookAPA (Pages (a:b:[])) = dot $ text $ show a ++ "&ndash;" ++ show b
bookAPA (Pages _) = error "Page range specified is empty or has more than two items"
bookAPA (Editor   p)  = dot $ p_spec (foldlList $ map (S . L.nameStr) p) <> text " (Ed.)"
bookAPA i = bookMLA i --Most items are rendered the same as L.MLA

bookChicago :: CiteField -> Doc
bookChicago (Author   p) = p_spec (rendPeople L.rendPersLFM'' p) --L.APA uses middle initals rather than full name
bookChicago p@(Pages  _) = bookAPA p
bookChicago (Editor   p) = dot $ p_spec (foldlList $ map (S . L.nameStr) p) <> (text $ toPlural p " ed")
bookChicago i = bookMLA i --Most items are rendered the same as L.MLA

-- for article renderings
artclMLA :: CiteField -> Doc
artclMLA (Title s) = doubleQuotes $ dot $ p_spec s
artclMLA i         = bookMLA i

artclAPA :: CiteField -> Doc
artclAPA (Title  s)  = dot $ p_spec s
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
rendPeople f people = foldlList $ map (S . f) people --foldlList is in SentenceStructures.hs

rendPeople' :: L.People -> Spec
rendPeople' []  = S "N.a." -- "No authors given"
rendPeople' people = foldlList $ map (S . rendPers) (init people) ++  [S (rendPersL $ last people)]

foldlList :: [Spec] -> Spec
foldlList []    = EmptyS
foldlList [a,b] = a :+: S " and " :+: b
foldlList lst   = foldle1 (\a b -> a :+: S ", " :+: b) (\a b -> a :+: S ", and " :+: b) lst

foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g ((f x y):xs)

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
