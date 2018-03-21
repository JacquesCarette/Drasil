module Language.Drasil.HTML.Print(genHTML) where

import Prelude hiding (print)
import Data.List (sortBy)
import Text.PrettyPrint hiding (render, quotes, Str)
import Numeric (showFFloat)

import Language.Drasil.HTML.Import (makeDocument, spec)
import Language.Drasil.Printing.AST
import Language.Drasil.HTML.AST
import qualified Language.Drasil.Output.Formats as F
import Language.Drasil.Spec (USymb(..), Sentence, sC, (+:+))

import Language.Drasil.HTML.Helpers
import Language.Drasil.Printing.Helpers
import Language.Drasil.Unicode
import           Language.Drasil.Symbol (Symbol(..))
import qualified Language.Drasil.Symbol as S
import qualified Language.Drasil.Document as L
import Language.Drasil.HTML.Monad
import Language.Drasil.People (People,Person(..),rendPersLFM',rendPersLFM'',Conv(..),nameStr,rendPersLFM, isInitial)
import Language.Drasil.Config (StyleGuide(..), bibStyleH)
import Language.Drasil.ChunkDB (HasSymbolTable(..))

import Language.Drasil.Chunk.Citation (CitationKind(..))

data OpenClose = Open | Close

--FIXME? Use Doc in place of Strings for p_spec/title_spec

-- | Generate an HTML document from a Drasil 'Document'
genHTML :: HasSymbolTable s => F.Filename -> L.Document -> s -> Doc
genHTML fn doc sm = build fn (makeDocument doc sm) sm

-- | Build the HTML Document, called by genHTML
build :: HasSymbolTable s => String -> Document -> s -> Doc
build fn (Document t a c) sm =
  text ( "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""++
          " \"http://www.w3.org/TR/html4/loose.dtd\">" ++ "\n" ++
          "<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/"++
          "2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>") $$
  html (head_tag ((linkCSS fn) $$ (title (text (title_spec t sm)))) $$
  body (article_title (text (p_spec t sm)) $$ author (text (p_spec a sm))
  $$ print c sm
  ))

-- | Helper for rendering LayoutObjects into HTML
printLO :: HasSymbolTable s => s -> LayoutObj -> Doc
printLO sm (HDiv ts layoutObs l)  = refwrap (p_spec l sm) $
                                   div_tag ts (vcat (map (printLO sm) layoutObs))
printLO sm (Paragraph contents)   = paragraph $ text (p_spec contents sm)
printLO sm (Tagless contents)     = text $ p_spec contents sm
printLO sm (Table ts rows r b t)  = makeTable ts rows (p_spec r sm) b (p_spec t sm) sm
printLO sm (Definition dt ssPs l) = makeDefn dt ssPs (p_spec l sm) sm
printLO sm (Header n contents)    = h n $ text (p_spec contents sm)
printLO sm (List t)               = makeList t sm
printLO sm (Figure r c f wp)      = makeFigure (p_spec r sm) (p_spec c sm) f wp
printLO sm (ALUR _ x l i)         = makeRefList (p_spec x sm) (p_spec l sm) (p_spec i sm)
printLO sm (Bib bib)              = makeBib sm bib


-- | Called by build, uses 'printLO' to render the layout
-- objects in Doc format.
print :: HasSymbolTable s => [LayoutObj] -> s -> Doc
print l sm = foldr ($$) empty $ map (printLO sm) l

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders the title of the document. Different than body rendering
-- because certain things can't be rendered in an HTML title.
title_spec :: HasSymbolTable s => Spec -> s -> String
title_spec (N s)       _ = t_symbol s
title_spec (a :+: b)  sm = title_spec a sm ++ title_spec b sm
title_spec HARDNL      _ = ""
title_spec s          sm = p_spec s sm

-- | Renders the Sentences in the HTML body (called by 'printLO')
p_spec :: HasSymbolTable s => Spec -> s -> String
p_spec (E e)        _ = p_expr e
p_spec (a :+: b)   sm = p_spec a sm ++ p_spec b sm
p_spec (S s)        _ = s
p_spec (N s)        _ = symbol s
p_spec (Sy s)       _ = uSymb s
p_spec (G g)        _ = unPH $ greek g
p_spec (Sp s)       _ = unPH $ special s
p_spec HARDNL       _ = "<br />"
p_spec (Ref _ r a) sm = reflink r (p_spec a sm)
p_spec EmptyS       _ = ""

-- | Renders symbols for HTML title
t_symbol :: Symbol -> String
t_symbol (Corners [] [] [] [x] s) = t_symbol s ++ "_" ++ t_symbol x
t_symbol (Corners [] [] [x] [] s) = t_symbol s ++ "^" ++ t_symbol x
t_symbol s                        = symbol s

-- | Adds emphasis to symbols by default. Use symbolNoEm for no <em>
--   Units do not need this, for example.
symbol :: Symbol -> String
symbol s = em $ symbolNoEm s

-- | Renders symbols for HTML document
symbolNoEm :: Symbol -> String
symbolNoEm (Atomic s)  = s
symbolNoEm (Special s) = unPH $ special s
symbolNoEm (Concat sl) = foldr (++) "" $ map symbolNoEm sl
symbolNoEm (Greek g)   = unPH $ greek g
-- handle the special cases first, then general case
symbolNoEm (Corners [] [] [x] [] s) = (symbolNoEm s) ++ sup (symbolNoEm x)
symbolNoEm (Corners [] [] [] [x] s) = (symbolNoEm s) ++ sub (symbolNoEm x)
symbolNoEm (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbolNoEm (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbolNoEm (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbolNoEm (Atop S.Vector s)       = "<b>" ++ symbolNoEm s ++ "</b>"
symbolNoEm (Atop S.Hat s)          = symbolNoEm s ++ "&#770;"
symbolNoEm (Atop S.Prime s)        = symbolNoEm s ++ "&prime;"
symbolNoEm Empty                 = ""

uSymb :: USymb -> String
uSymb (UName s)           = symbolNoEm s
uSymb (UProd l)           = foldr1 (\x -> ((x++"&sdot;")++) ) (map uSymb l)
uSymb (UPow s i)          = uSymb s ++ sup (show i)
uSymb (UDiv n (UName d))  = uSymb n ++ "/" ++ uSymb (UName d)
uSymb (UDiv n d)          = uSymb n ++ "/(" ++ (uSymb d) ++ ")"

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- | Renders expressions in the HTML (called by multiple functions)
p_expr :: Expr -> String
p_expr (Dbl d)    = showFFloat Nothing d ""
p_expr (Int i)    = show i
p_expr (Str s)    = s
p_expr (Div a b) = fraction (p_expr a) (p_expr b) --Found in HTMLHelpers
p_expr (Funct f e)    = p_op f e
p_expr (Case ps)  = cases ps (p_expr)
p_expr (Mtx a)    = "<table class=\"matrix\">\n" ++ p_matrix a ++ "</table>"
p_expr (Row l) = concatMap p_expr l
p_expr (Ident s) = s
p_expr (Spec s) = unPH $ special s
p_expr (Gr g) = unPH $ greek g
p_expr (Sub e) = sub $ p_expr e
p_expr (Sup e) = sup $ p_expr e
p_expr (Over Hat s)     = p_expr s ++ "&#770;"
p_expr (MO o) = p_ops o
p_expr (Fenced l r e) = fence Open l ++ p_expr e ++ fence Close r
p_expr (Font Bold e) = bold $ p_expr e
p_expr (Font Emph e) = em $ p_expr e

p_ops :: Ops -> String
p_ops IsIn = "&thinsp;&isin;&thinsp;"
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
p_ops Sqrt     = "&radic;"
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

fence :: OpenClose -> Fence -> String
fence Open Paren = "("
fence Close Paren = ")"
fence Open Curly = "{"
fence Close Curly = "}"
fence _ Abs = "|"
fence _ Norm = "||"

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
makeTable ::HasSymbolTable s => Tags -> [[Spec]] -> String -> Bool -> String -> s -> Doc
makeTable _ [] _ _ _        _ = error "No table to print (see PrintHTML)"
makeTable ts (l:lls) r b t sm = refwrap r (wrap "table" ts (
    tr (makeHeaderCols l sm) $$ makeRows lls sm) $$ if b then caption t else empty)

-- | Helper for creating table rows
makeRows :: HasSymbolTable s => [[Spec]] -> s -> Doc
makeRows []      _ = empty
makeRows (c:cs) sm = tr (makeColumns c sm) $$ makeRows cs sm

makeColumns, makeHeaderCols :: HasSymbolTable s => [Spec] -> s -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols ls sm = vcat $ map (th . text . flip p_spec sm) ls

-- | Helper for creating table columns
makeColumns ls sm = vcat $ map (td . text . flip p_spec sm) ls

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: HasSymbolTable s => L.DType -> [(String,[LayoutObj])] -> String -> s -> Doc
makeDefn _ [] _    _ = error "Empty definition"
makeDefn dt ps l sm = refwrap l $ wrap "table" [dtag dt] (makeDRows ps sm)
  where dtag (L.Data _)   = "ddefn"
        dtag (L.Theory _) = "tdefn"
        dtag (L.General)  = "gdefn"
        dtag (L.Instance) = "idefn"
        dtag (L.TM) = "tdefn"
        dtag (L.DD) = "ddefn"

-- | Helper for making the definition table rows
makeDRows :: HasSymbolTable s => [(String,[LayoutObj])] -> s -> Doc
makeDRows []          _ = error "No fields to create defn table"
makeDRows ((f,d):[]) sm = tr (th (text f) $$ td (vcat $ map (printLO sm) d))
makeDRows ((f,d):ps) sm = tr (th (text f) $$ td (vcat $ map (printLO sm) d)) $$ makeDRows ps sm

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists
makeList :: HasSymbolTable s => ListType -> s -> Doc
makeList (Simple items) sm = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((text (p_spec b sm ++ ": ") <> (p_item e sm)))) items)
makeList (Desc items)   sm = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((wrap "b" [] (text (p_spec b sm ++ ": "))
   <> (p_item e sm)))) items)
makeList (Ordered items) sm = wrap "ol" ["list"] (vcat $ map
  (wrap "li" [] . flip p_item sm) items)
makeList (Unordered items) sm = wrap "ul" ["list"] (vcat $ map
  (wrap "li" [] . flip p_item sm) items)
makeList (Definitions items) sm = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((text (p_spec b sm ++ " is the") <+>
  (p_item e sm)))) items)

-- | Helper for rendering list items
p_item :: HasSymbolTable s => ItemType -> s -> Doc
p_item (Flat s)     sm = text $ p_spec s sm
p_item (Nested s l) sm = vcat [text (p_spec s sm),makeList l sm]

-----------------------------------------------------------------
------------------BEGIN FIGURE PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders figures in HTML
makeFigure :: String -> String -> String -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refwrap r (image f c wp $$ caption c)

-----------------------------------------------------------------
------------------BEGIN EXPR OP PRINTING-------------------------
-----------------------------------------------------------------
-- | Renders expression operations/functions.
p_op :: Functional -> Expr -> String
p_op (Summation bs) x = lrgOp "&sum;" bs ++ paren (p_expr x)
p_op (Product bs) x = lrgOp "&prod;" bs ++ paren (p_expr x)
p_op (Integral bs wrtc) x = intg bs
  ++ paren (p_expr x ++ (symbol (Atomic "d") ++ "&#8239;" ++ symbol wrtc))

-- | Helpers for summation/product, used by 'p_op'
makeBound :: String -> String
makeBound s = "<tr><td><span class=\"bound\">" ++ s ++ "</span></td></tr>\n"

lrgOp :: String -> Maybe ((Symbol, Expr),Expr) -> String
lrgOp f Nothing = "<span class=\"symb\">" ++ f ++ "</span>"
lrgOp f (Just ((s,v),hi)) = "<table class=\"operator\">\n" ++ makeBound (p_expr hi) ++
  "<tr><td><span class=\"symb\">" ++ f ++ "</span></td></tr>\n" ++
  makeBound (symbol s ++"="++ p_expr v) ++ "</table>"

intg :: (Maybe Expr, Maybe Expr) -> String
intg (Nothing, Nothing) = "<span class=\"symb\">&int;</span>"
intg (Just l, Nothing) = "<span class=\"symb\">&int;</span>" ++ sub (p_expr l ++ " ")
intg (low,high) = "<table class=\"operator\">\n" ++ pHigh high ++
  "<tr><td><span class=\"symb\">&int;</span></td></tr>\n" ++
  pLow low ++ "</table>"
  where pLow Nothing   = ""
        pLow (Just l)  = makeBound (p_expr l)
        pHigh Nothing  = ""
        pHigh (Just hi) = makeBound (p_expr hi)

-- | Renders modules
-- makeModule :: String -> String -> Doc
-- makeModule m l = refwrap l (paragraph $ wrap "b" [] (text m))

-- | Renders assumptions, requirements, likely changes
makeRefList :: String -> String -> String -> Doc
makeRefList a l i = refwrap l (wrap "ul" [] (text $ i ++ ": " ++ a))

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**

makeBib :: HasSymbolTable s => s -> BibRef -> Doc
makeBib sm = vcat . map (\(x,(y,z)) -> makeRefList z y x) .
  zip [sqbrac $ show x | x <- ([1..] :: [Int])] . map (renderCite sm)
  --some function to get a numbered list, idealy it wouldn't go from string to Spec

--for when we add other things to reference like website, newspaper
renderCite :: HasSymbolTable s => s -> Citation -> (String, String)
renderCite sm (Cite e Book cfs) = (e, renderF sm cfs useStyleBk ++ " Print.")
renderCite sm (Cite e Article cfs) = (e, renderF sm cfs useStyleArtcl ++ " Print.")
renderCite sm (Cite e MThesis cfs) = (e, renderF sm cfs useStyleBk ++ " Print.")
renderCite sm (Cite e PhDThesis cfs) = (e, renderF sm cfs useStyleBk ++ " Print.")
renderCite sm (Cite e Misc cfs) = (e, renderF sm cfs useStyleBk ++ "")
renderCite sm (Cite e _ cfs) = (e, renderF sm cfs useStyleArtcl ++ "") --FIXME: Properly render these later.

renderF :: HasSymbolTable s => s -> [CiteField] -> (StyleGuide -> (CiteField -> s -> String)) -> String
renderF sm fields styl = unwords $
  map (flip (styl bibStyleH) sm) (sortBy compCiteField fields)

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
useStyleBk :: HasSymbolTable s => StyleGuide -> (CiteField -> s -> String)
useStyleBk MLA     = bookMLA
useStyleBk APA     = bookAPA
useStyleBk Chicago = bookChicago

useStyleArtcl :: HasSymbolTable s => StyleGuide -> (CiteField -> s -> String)
useStyleArtcl MLA     = artclMLA
useStyleArtcl APA     = artclAPA
useStyleArtcl Chicago = artclChicago

-- FIXME: move these show functions and use tags, combinators
bookMLA :: HasSymbolTable s => CiteField -> s -> String
bookMLA (Address s) sm = p_spec s sm ++ ":"
bookMLA (Edition    s)  _  = comm $ show s ++ sufxer s ++ " ed."
bookMLA (Series     s) sm  = dot $ em $ p_spec s sm
bookMLA (Title      s) sm  = dot $ em $ p_spec s sm --If there is a series or collection, this should be in quotes, not italics
bookMLA (Volume     s)  _  = comm $ "vol. " ++ show s
bookMLA (Publisher  s) sm  = comm $ p_spec s sm
bookMLA (Author     p) sm  = dot $ p_spec (rendPeople' sm p) sm
bookMLA (Year       y)  _  = dot $ show y
--bookMLA (Date    d m y)  _ = dot $ unwords [show d, show m, show y]
--bookMLA (URLdate d m y) sm = "Web. " ++ bookMLA (Date d m y) sm
bookMLA (BookTitle s) sm  = dot $ em $ p_spec s sm
bookMLA (Journal    s) sm  = comm $ em $ p_spec s sm
bookMLA (Pages      [n])  _  = dot $ "p. " ++ show n
bookMLA (Pages  (a:b:[]))  _  = dot $ "pp. " ++ show a ++ "&ndash;" ++ show b
bookMLA (Pages _) _ = error "Page range specified is empty or has more than two items"
bookMLA (Note       s) sm  = p_spec s sm
bookMLA (Number      n)  _  = comm $ "no. " ++ show n
bookMLA (School     s) sm  = comm $ p_spec s sm
--bookMLA (Thesis     t)  _  = comm $ show t
--bookMLA (URL        s) sm  = dot $ p_spec s sm
bookMLA (HowPublished (Verb s)) sm  = comm $ p_spec s sm
bookMLA (HowPublished (URL s)) sm = dot $ p_spec s sm
bookMLA (Editor     p) sm  = comm $ "Edited by " ++ p_spec (foldlList (map (flip spec sm . nameStr) p)) sm
bookMLA (Chapter _) _ = ""
bookMLA (Institution i) sm = comm $ p_spec i sm
bookMLA (Organization i) sm = comm $ p_spec i sm
bookMLA (Month m) _ = comm $ show m
bookMLA (Type t) sm = comm $ p_spec t sm


bookAPA :: HasSymbolTable s => CiteField -> s -> String --FIXME: year needs to come after author in APA
bookAPA (Author   p) sm = needDot $ p_spec (rendPeople sm rendPersLFM' p) sm --APA uses initals rather than full name
bookAPA (Year     y)  _ = dot $ paren $ show y --APA puts "()" around the year
--bookAPA (Date _ _ y) sm = bookAPA (Year y) sm --APA doesn't care about the day or month
--bookAPA (URLdate d m y)  _ = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA (Pages     [n])   _ = dot $ show n
bookAPA (Pages (a:b:[]))  _ = dot $ show a ++ "&ndash;" ++ show b
bookAPA (Pages _) _ = error "Page range specified is empty or has more than two items"
bookAPA (Editor   p)  sm = dot $ p_spec (foldlList $ map ( flip spec sm . nameStr) p) sm ++ " (Ed.)"
bookAPA i sm = bookMLA i sm --Most items are rendered the same as MLA

bookChicago :: HasSymbolTable s => CiteField -> s -> String
bookChicago (Author   p) sm = needDot $ p_spec (rendPeople sm rendPersLFM'' p) sm --APA uses middle initals rather than full name
--bookChicago (Date _ _ y) sm = bookChicago (Year y) sm --APA doesn't care about the day or month
--bookChicago (URLdate d m y)  _ = "accessed " ++ (comm $ unwords [show d, show m, show y])
bookChicago p@(Pages  _) sm = bookAPA p sm
bookChicago (Editor   p) sm = dot $ p_spec (foldlList $ map (flip spec sm . nameStr) p) sm ++ toPlural p " ed"
bookChicago i sm = bookMLA i sm--Most items are rendered the same as MLA

-- for article renderings
artclMLA :: HasSymbolTable s => CiteField -> s -> String
artclMLA (Title s) = quotes . dot . p_spec s
artclMLA i = bookMLA i

artclAPA :: HasSymbolTable s => CiteField -> s -> String
artclAPA (Title  s) sm = dot $ p_spec s sm
artclAPA (Volume n)  _ = em $ show n
artclAPA (Number  n)  _ = comm $ paren $ show n
artclAPA i sm = bookAPA i sm

artclChicago :: HasSymbolTable s => CiteField -> s -> String
artclChicago i@(Title    _) = artclMLA i
artclChicago (Volume     n) = \_ -> comm $ show n
artclChicago (Number      n) = \_ -> "no. " ++ show n
artclChicago i@(Year     _) = bookAPA i
--artclChicago i@(Date _ _ _) = bookAPA i
artclChicago i = bookChicago i

-- PEOPLE RENDERING --

rendPeople :: HasSymbolTable s => s -> (Person -> Sentence) -> People -> Spec
rendPeople _ _ []  = S "N.a." -- "No authors given"
rendPeople sm f people = foldlList $ map (flip spec sm . f) people --foldlList is in SentenceStructures.hs

rendPeople' :: HasSymbolTable s => s -> People -> Spec
rendPeople' _ []  = S "N.a." -- "No authors given"
rendPeople' sm people = foldlList $ map (flip spec sm . rendPers) (init people) ++  [spec (rendPersL $ last people) sm]

foldlList :: [Spec] -> Spec
foldlList []    = EmptyS
foldlList [a,b] = a :+: S " and " :+: b
foldlList lst   = foldle1 (\a b -> a :+: S ", " :+: b) (\a b -> a :+: S ", and " :+: b) lst

foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g ((f x y):xs)

needDot :: String -> String
needDot str = dotIt $ last str
  where dotIt '.' = str
        dotIt _   = dot str
-- LFM is Last, First Middle
rendPers :: Person -> Sentence
rendPers = rendPersLFM

-- To render the last person's name
rendPersL :: Person -> Sentence
rendPersL (Person {_surname = n, _convention = Mono}) = n
rendPersL (Person {_given = f, _surname = l, _middle = []}) =
  isInitial l `sC` isInitial f
rendPersL (Person {_given = f, _surname = l, _middle = ms}) =
  isInitial l `sC` foldr1 (+:+) ([isInitial f] ++ map (isInitial) (init ms) ++ [last ms])

--adds an 's' if there is more than one person in a list
toPlural :: People -> String -> String
toPlural (_:_) str = str ++ "s"
toPlural _     str = str
