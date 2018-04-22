module Language.Drasil.HTML.Print(genHTML) where

import Prelude hiding (print)
import Data.List (sortBy,partition,intersperse)
import Text.PrettyPrint hiding (render, Str)
import Numeric (showFFloat)
import Control.Arrow (second)

import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST
import Language.Drasil.Printing.Citation
import Language.Drasil.Printing.LayoutObj
import Language.Drasil.Printing.Import (spec)
import qualified Language.Drasil.Output.Formats as F
import Language.Drasil.Spec (Sentence, sC, (+:+), Accent(..))
import Language.Drasil.UnitLang

import Language.Drasil.HTML.Helpers
import Language.Drasil.Printing.Helpers
import Language.Drasil.Unicode
import           Language.Drasil.Symbol (Symbol(..))
import qualified Language.Drasil.Symbol as S
import qualified Language.Drasil.Document as L
import Language.Drasil.HTML.Monad
import Language.Drasil.People (People,Person(..),rendPersLFM',rendPersLFM'',Conv(..),nameStr,rendPersLFM, dotInitial)
import Language.Drasil.Config (StyleGuide(..), bibStyleH)
import Language.Drasil.ChunkDB (HasSymbolTable(..))

import Language.Drasil.Chunk.Citation (CitationKind(..))

data OpenClose = Open | Close

-- | Generate an HTML document from a Drasil 'Document'
genHTML :: HasSymbolTable ctx => ctx -> F.Filename -> L.Document -> Doc
genHTML sm fn doc = build sm fn (makeDocument sm doc)

-- | Build the HTML Document, called by genHTML
build :: HasSymbolTable s => s -> String -> Document -> Doc
build s fn (Document t a c) =
  text ( "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""++
          " \"http://www.w3.org/TR/html4/loose.dtd\">" ++ "\n" ++
          "<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/"++
          "2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>") $$
  html (head_tag ((linkCSS fn) $$ title (title_spec t)) $$
  body (article_title (p_spec t) $$ author (p_spec a)
  $$ print s c
  ))

-- | Helper for rendering LayoutObjects into HTML
printLO :: HasSymbolTable s => s -> LayoutObj -> Doc
printLO sm (HDiv ts layoutObs l)  = refwrap (p_spec l) $
                                   div_tag ts (vcat (map (printLO sm) layoutObs))
printLO _  (Paragraph contents)   = paragraph $ p_spec contents
printLO _  (EqnBlock contents)    = p_spec contents
printLO _  (Table ts rows r b t)  = makeTable ts rows (p_spec r) b (p_spec t)
printLO sm (Definition dt ssPs l) = makeDefn dt ssPs (p_spec l) sm
printLO _  (Header n contents _)  = h n $ p_spec contents -- FIXME
printLO _  (List t)               = makeList t
printLO _  (Figure r c f wp)      = makeFigure (p_spec r) (p_spec c) (text f) wp
printLO _  (ALUR _ x l i)         = makeRefList (p_spec x) (p_spec l) (p_spec i)
printLO sm (Bib bib)              = makeBib sm bib
printLO _  (Graph _ _ _ _ _)      = empty -- FIXME

-- | Called by build, uses 'printLO' to render the layout
-- objects in Doc format.
print :: HasSymbolTable s => s -> [LayoutObj] -> Doc
print sm l = foldr ($$) empty $ map (printLO sm) l

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
p_spec (Sp s)            = text $ unPH $ special s
p_spec HARDNL            = text "<br />"
p_spec (Ref _ r a)       = reflink r $ p_spec a
p_spec EmptyS            = text "" -- Expected in the output
p_spec (Quote q)         = text "&quot;" <> p_spec q <> text "&quot;"
p_spec (Acc Grave c)     = text $ '&' : c : "grave;" --Only works on vowels.
p_spec (Acc Acute c)     = text $ '&' : c : "acute;" --Only works on vowels.


-- | Renders symbols for HTML document
symbol :: Symbol -> String
symbol (Atomic s)  = s
symbol (Special s) = unPH $ special s
symbol (Concat sl) = foldr (++) "" $ map symbol sl
symbol (Greek g)   = unPH $ greek g
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = (symbol s) ++ sup (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) ++ sub (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (Atop S.Vector s)        = "<b>" ++ symbol s ++ "</b>"
symbol (Atop S.Hat s)           = symbol s ++ "&#770;"
symbol (Atop S.Prime s)         = symbol s ++ "&prime;"
symbol Empty                    = ""

uSymb :: USymb -> String
uSymb (US ls) = formatu t b
  where
    (t,b) = partition ((> 0) . snd) ls
    formatu :: [(Symbol,Integer)] -> [(Symbol,Integer)] -> String
    formatu [] l = line l
    formatu l [] = concat $ intersperse "&sdot;" $ map pow l
    formatu nu de = line nu ++ "/" ++ (line $ map (second negate) de)
    line :: [(Symbol,Integer)] -> String
    line []  = ""
    line [x] = pow x
    line l   = '(' : (concat $ intersperse "&sdot;" $ map pow l) ++ ")"
    pow :: (Symbol,Integer) -> String
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
p_expr (Spec s)       = unPH $ special s
p_expr (Gr g)         = unPH $ greek g
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
makeDefn :: HasSymbolTable s => L.DType -> [(String,[LayoutObj])] -> Doc -> s -> Doc
makeDefn _ [] _  _  = error "Empty definition"
makeDefn dt ps l sm = refwrap l $ wrap "table" [dtag dt] (makeDRows ps sm)
  where dtag (L.Data _)   = "ddefn"
        dtag (L.Theory _) = "tdefn"
        dtag (L.General)  = "gdefn"
        dtag (L.Instance) = "idefn"
        dtag (L.TM)       = "tdefn"
        dtag (L.DD)       = "ddefn"

-- | Helper for making the definition table rows
makeDRows :: HasSymbolTable s => [(String,[LayoutObj])] -> s -> Doc
makeDRows []          _ = error "No fields to create defn table"
makeDRows ((f,d):[]) sm = tr (th (text f) $$ td (vcat $ map (printLO sm) d))
makeDRows ((f,d):ps) sm = tr (th (text f) $$ td (vcat $ map (printLO sm) d)) $$ makeDRows ps sm

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists
makeList :: ListType -> Doc
makeList (Simple items) = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((p_spec b <> text ": ") <> (p_item e))) items)
makeList (Desc items)   = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((wrap "b" [] (p_spec b <> text ": ")
   <> (p_item e)))) items)
makeList (Ordered items) = wrap "ol" ["list"] (vcat $ map
  (wrap "li" [] . p_item) items)
makeList (Unordered items) = wrap "ul" ["list"] (vcat $ map
  (wrap "li" [] . p_item) items)
makeList (Definitions items) = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((p_spec b <> text " is the") <+>
  (p_item e))) items)

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
makeRefList a l i = refwrap l (wrap "ul" [] (i <> text ": " <> a))

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**

makeBib :: HasSymbolTable s => s -> BibRef -> Doc
makeBib sm = vcat . map (\(x,(y,z)) -> makeRefList z y x) .
  zip [text $ sqbrac $ show x | x <- ([1..] :: [Int])] . map (renderCite sm)

--for when we add other things to reference like website, newspaper
renderCite :: HasSymbolTable s => s -> Citation -> (Doc, Doc)
renderCite sm (Cite e Book cfs) = (text e, renderF sm cfs useStyleBk <> text " Print.")
renderCite sm (Cite e Article cfs) = (text e, renderF sm cfs useStyleArtcl <> text " Print.")
renderCite sm (Cite e MThesis cfs) = (text e, renderF sm cfs useStyleBk <> text " Print.")
renderCite sm (Cite e PhDThesis cfs) = (text e, renderF sm cfs useStyleBk <> text " Print.")
renderCite sm (Cite e Misc cfs) = (text e, renderF sm cfs useStyleBk)
renderCite sm (Cite e _ cfs) = (text e, renderF sm cfs useStyleArtcl) --FIXME: Properly render these later.

renderF :: HasSymbolTable s => s -> [CiteField] -> (StyleGuide -> (s -> CiteField -> Doc)) -> Doc
renderF sm fields styl = hcat $ map (styl bibStyleH sm) (sortBy compCiteField fields)

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
useStyleBk :: HasSymbolTable s => StyleGuide -> (s -> CiteField -> Doc)
useStyleBk MLA     = bookMLA
useStyleBk APA     = bookAPA
useStyleBk Chicago = bookChicago

useStyleArtcl :: HasSymbolTable s => StyleGuide -> (s -> CiteField -> Doc)
useStyleArtcl MLA     = artclMLA
useStyleArtcl APA     = artclAPA
useStyleArtcl Chicago = artclChicago

-- FIXME: move these show functions and use tags, combinators
bookMLA :: HasSymbolTable s => s -> CiteField -> Doc
bookMLA _ (Address s)     = p_spec s <> text ":"
bookMLA _ (Edition    s)  = comm $ text $ show s ++ sufxer s ++ " ed."
bookMLA _ (Series     s)  = dot $ em $ p_spec s
bookMLA _ (Title      s)  = dot $ em $ p_spec s --If there is a series or collection, this should be in quotes, not italics
bookMLA _ (Volume     s)  = comm $ text $ "vol. " ++ show s
bookMLA _ (Publisher  s)  = comm $ p_spec s
bookMLA sm (Author     p)  = dot $ p_spec (rendPeople' sm p)
bookMLA _ (Year       y)  = dot $ text $ show y
--bookMLA (Date    d m y) = dot $ unwords [show d, show m, show y]
--bookMLA (URLdate d m y) = "Web. " ++ bookMLA (Date d m y) sm
bookMLA _ (BookTitle s)   = dot $ em $ p_spec s
bookMLA _ (Journal    s)  = comm $ em $ p_spec s
bookMLA _ (Pages      [n]) = dot $ text $ "p. " ++ show n
bookMLA _ (Pages  (a:b:[])) = dot $ text $ "pp. " ++ show a ++ "&ndash;" ++ show b
bookMLA _ (Pages _) = error "Page range specified is empty or has more than two items"
bookMLA _ (Note       s)    = p_spec s
bookMLA _ (Number      n)   = comm $ text $ ("no. " ++ show n)
bookMLA _ (School     s)    = comm $ p_spec s
--bookMLA (Thesis     t)  = comm $ show t
--bookMLA (URL        s)  = dot $ p_spec s
bookMLA _ (HowPublished (Verb s)) = comm $ p_spec s
bookMLA _ (HowPublished (URL s))  = dot $ p_spec s
bookMLA sm (Editor     p)    = comm $ text "Edited by " <> p_spec (foldlList (map (spec sm . nameStr) p))
bookMLA _ (Chapter _)       = text ""
bookMLA _ (Institution i)   = comm $ p_spec i
bookMLA _ (Organization i)  = comm $ p_spec i
bookMLA _ (Month m)         = comm $ text $ show m
bookMLA _ (Type t)          = comm $ p_spec t

bookAPA :: HasSymbolTable s => s -> CiteField -> Doc --FIXME: year needs to come after author in APA
bookAPA sm (Author   p) = p_spec (rendPeople sm rendPersLFM' p) --APA uses initals rather than full name
bookAPA _  (Year     y) = dot $ text $ paren $ show y --APA puts "()" around the year
--bookAPA (Date _ _ y) = bookAPA (Year y) sm --APA doesn't care about the day or month
--bookAPA (URLdate d m y) = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA _  (Pages     [n])  = dot $ text $ show n
bookAPA _  (Pages (a:b:[])) = dot $ text $ show a ++ "&ndash;" ++ show b
bookAPA _  (Pages _) = error "Page range specified is empty or has more than two items"
bookAPA sm (Editor   p)  = dot $ p_spec (foldlList $ map (spec sm . nameStr) p) <> text " (Ed.)"
bookAPA sm i = bookMLA sm i --Most items are rendered the same as MLA

bookChicago :: HasSymbolTable s => s -> CiteField -> Doc
bookChicago sm (Author   p) = p_spec (rendPeople sm rendPersLFM'' p) --APA uses middle initals rather than full name
bookChicago sm p@(Pages  _) = bookAPA sm p
bookChicago sm (Editor   p) = dot $ p_spec (foldlList $ map (spec sm . nameStr) p) <> (text $ toPlural p " ed")
bookChicago sm i = bookMLA sm i --Most items are rendered the same as MLA

-- for article renderings
artclMLA :: HasSymbolTable s => s -> CiteField -> Doc
artclMLA _  (Title s) = doubleQuotes $ dot $ p_spec s
artclMLA sm i         = bookMLA sm i

artclAPA :: HasSymbolTable s => s -> CiteField -> Doc
artclAPA _  (Title  s)  = dot $ p_spec s
artclAPA _  (Volume n)  = em $ text $ show n
artclAPA _  (Number  n) = comm $ text $ paren $ show n
artclAPA sm i           = bookAPA sm i

artclChicago :: HasSymbolTable ctx => ctx -> CiteField -> Doc
artclChicago sm i@(Title    _) = artclMLA sm i
artclChicago _  (Volume     n) = comm $ text $ show n
artclChicago _  (Number      n) = text $ "no. " ++ show n
artclChicago sm  i@(Year     _) = bookAPA sm i
--artclChicago i@(Date _ _ _) = bookAPA i
artclChicago sm i = bookChicago sm i

-- PEOPLE RENDERING --
rendPeople :: HasSymbolTable s => s -> (Person -> Sentence) -> People -> Spec
rendPeople _ _ []  = S "N.a." -- "No authors given"
rendPeople sm f people = foldlList $ map (spec sm . f) people --foldlList is in SentenceStructures.hs

rendPeople' :: HasSymbolTable s => s -> People -> Spec
rendPeople' _ []  = S "N.a." -- "No authors given"
rendPeople' sm people = foldlList $ map (spec sm . rendPers) (init people) ++  [spec sm (rendPersL $ last people)]

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
rendPers :: Person -> Sentence
rendPers = rendPersLFM

-- To render the last person's name
rendPersL :: Person -> Sentence
rendPersL (Person {_surname = n, _convention = Mono}) = n
rendPersL (Person {_given = f, _surname = l, _middle = []}) =
  dotInitial l `sC` dotInitial f
rendPersL (Person {_given = f, _surname = l, _middle = ms}) =
  dotInitial l `sC` foldr1 (+:+) (dotInitial f : map dotInitial (init ms) ++ [last ms])

--adds an 's' if there is more than one person in a list
toPlural :: People -> String -> String
toPlural (_:_) str = str ++ "s"
toPlural _     str = str
