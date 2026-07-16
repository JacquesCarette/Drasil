{-# LANGUAGE OverloadedStrings #-}
-- | Defines all functions needed to print HTML files. For more information on each of the helper functions, please view the [source files](https://jacquescarette.github.io/Drasil/docs/full/drasil-printers-0.1.10.0/src/Language.Drasil.HTML.Print.html).
module Language.Drasil.HTML2.Print(
  -- * Main Function
  genHTML2,
  -- * Citation Renderer
  renderCite,
  -- * HTML Bib Formatter
  htmlBibFormatter,
  -- * HTML Spec Printing
  specToHTML,
  -- * Term Fencing Helpers
  OpenClose(Open, Close),
  fence) where

import Prelude hiding (print)
import Data.List (sortBy)
import Text.PrettyPrint as PLegacy hiding (Str, (<>))
import Numeric (showEFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PNew (Doc)
import Data.String (IsString)

import Language.Drasil (People, Person, fullName, rendPersLFM, rendPersLFM',
  rendPersLFM'', special, CitationKind(..),
  numList)

import Drasil.Data.Formats.HTML hiding (Title, Row, Bold, ListType, Ordered,
  Unordered, span, Paragraph, Table, List, Figure)
import qualified Drasil.Data.Formats.HTML as HTML
import Language.Drasil.HTML2.Monad (unPH)
import Language.Drasil.HTML2.Helpers (BibFormatter(..), articleTitle, author, foldRaw)
import Language.Drasil.HTML2.CSS (stylesheet)

import Language.Drasil.Config (StyleGuide(APA, MLA, Chicago), bibStyleH)
import Language.Drasil.Printing.AST (ItemType(Flat, Nested),
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, Fence(Curly, Paren, Abs, Norm),
  Ops(..), Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, Sp, S, E, (:+:), Tooltip),
  Spacing(Thin), Fonts(Bold, Emph), OverSymb(Hat),
  LinkType(Internal, Cite2, External))
import Language.Drasil.Printing.Citation (CiteField(Year, Number, Volume, Title, Author,
  Editor, Pages, Type, Month, Organization, Institution, Chapter, HowPublished, School, Note,
  Journal, BookTitle, Publisher, Series, Address, Edition), HP(URL, Verb),
  Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import Language.Drasil.Printing.Helpers (paren, sufxer, sufxPrint)

import qualified Language.Drasil.TeX.Print as TeX (pExpr, spec)
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, PrintLaTeX(PL))

-- | Referring to 'fence' (for parenthesis and brackeds). Either opened or closed.
data OpenClose = Open | Close

-- | Generate an HTML document from a Drasil 'Document'.
genHTML2 :: HTMLRenderOptions -> String -> Document -> PNew.Doc ann
genHTML2 opts fn doc = renderHTML opts (build fn doc)
--      second arg should really be of type Filename, but that's not in scope

-- | Variable to include MathJax in our HTML files so we can render equations in LaTeX.
mathJaxScript :: Text
mathJaxScript = T.unlines [
    "MathJax = {",
    "  loader: {load: ['[tex]/textmacros', 'output/chtml']},",
    "  tex: {",
    "    packages: {'[+]': ['textmacros']}",
    "  },",
    "  svg: {",
    "    fontCache: 'global'",
    "  }",
    "};"]

-- HTML printer doesn't need to know if there is a table of contents or not.
-- | Build the HTML Document, called by 'genHTML'.
build :: String -> Document -> HTML
build fn (Document t a c) = HTML heads bodies
  where
    heads = [
      stylesheet (T.pack fn),
      HTML.Title (extractText t),
      Meta [Attr "charset" "utf-8"],
      Script [] mathJaxScript,
      Script [Attr "type" "text/javascript", Attr "id" "MathJax-script", Attr "async" "",
      Attr "src" "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"] ""
      ]
    bodies = [
      articleTitle (specToHTML t),
      author (specToHTML a)
      ] ++ concatMap loToHTML c

extractText :: Spec -> Text
extractText (S s)               = T.pack s
extractText (E e)               = T.pack $ show $ printMath $ toMath $ TeX.pExpr e
extractText (a :+: b)           = extractText a <> extractText b
extractText HARDNL              = " "
extractText (Sp s)              = T.pack $ unPH $ special s
extractText (Ref (Cite2 n) _ a) = extractText a <> " " <> extractText n
extractText (Ref _ _ a)         = extractText a
extractText EmptyS              = ""
extractText (Quote q)           = "\"" <> extractText q <> "\""
extractText (Tooltip _ s)       = extractText s

-- | Helper for rendering a 'D' from Latex print.
printMath :: D -> PLegacy.Doc
printMath = (`runPrint` Math)

-- | Helper for transforming layout objects ('LayoutObj's) into HTML.
loToHTML :: LayoutObj -> [HTMLBody]
-- Creates delimeters to be used for mathjax displayed equations
-- Latex print sets up a \begin{displaymath} environment instead of this
loToHTML (EqnBlock contents) =
  [RawText $ T.pack $ show $ mjDelimDisp $ printMath $ toMathHelper $ TeX.spec contents]
  where
    toMathHelper (PL g) = PL (\_ -> g Math)
    mjDelimDisp d = text "\\[" <> d <> text "\\]"
-- Non-mathjax
loToHTML (HDiv ts layoutObs EmptyS) =
  [HTML.Div [Attr "class" (T.unwords $ map T.pack ts)] (concatMap loToHTML layoutObs)]
loToHTML (HDiv ts layoutObs l)      =
  [HTML.Div [Attr "id" (extractText l), Attr "class" (T.unwords $ map T.pack ts)] (concatMap loToHTML layoutObs)]
loToHTML (Paragraph contents)       = [HTML.Paragraph [Attr "class" "paragraph"] (specToHTML contents)]
loToHTML (Table ts rows r b t)      = makeTableHTML ts rows r b t
loToHTML (Definition ssPs l)        = makeDefnHTML ssPs l
loToHTML (Header n contents _)      = [Heading (toHLevel n) [] (specToHTML contents)]
  where
    toHLevel 0 = H1
    toHLevel 1 = H2
    toHLevel 2 = H3
    toHLevel 3 = H4
    toHLevel 4 = H5
    toHLevel _ = H6
loToHTML (List t)                   = [makeListHTML t]
loToHTML (Figure r c f wp)          =
  [HTML.Div [Attr "id" (extractText r)] [figureImage [] attrs (T.pack f) captionText ("Figure: " <> captionText)]]
  where
    attrs = [Attr "width" (T.pack $ show wp ++ "%") | wp /= 100]
    captionText = maybe "" extractText c
loToHTML (Bib bib)                  = makeBib bib
loToHTML Graph{}                    = []
loToHTML Cell{}                     = []
loToHTML CodeBlock{}                = []

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------

-- | Transforms the Sentences ('Spec's) into HTML (called by 'loToHTML').
specToHTML :: Spec -> [HTMLBody]
-- Non-mathjax
specToHTML (E e)               = [TextFormat Emphasis [] (foldRaw (exprToHTML e))]
-- Latex based math for expressions and units
-- pSpec (E e)     = printMath $ toMath $ TeX.pExpr e
-- pSpec (Sy s)    = printMath $ TeX.pUnit s
specToHTML (a :+: b)           = foldRaw (specToHTML a ++ specToHTML b)
specToHTML (S s)               = [RawText (T.pack s)]
specToHTML (Tooltip t s)       = [TextFormat Span [Attr "title" (extractText t)] (specToHTML s)]
specToHTML (Sp s)              = [RawText (T.pack $ unPH $ special s)]
specToHTML HARDNL              = [Custom (customTag "br") [] []]
specToHTML (Ref Internal r a)  = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (Ref (Cite2 EmptyS) r a) = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (Ref (Cite2 n) r a) = Anchor (T.pack $ "#" ++ r) [] (specToHTML a) : specToHTML n
specToHTML (Ref External r a)  = [Anchor (T.pack r) [] (specToHTML a)]
specToHTML EmptyS              = []
specToHTML (Quote q)           = foldRaw $ [RawText "\""] ++ specToHTML q ++ [RawText "\""]
--pSpec (Acc Grave c) = text $ '&' : c : "grave;" --Only works on vowels.
--pSpec (Acc Acute c) = text $ '&' : c : "acute;" --Only works on vowels.

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders expressions in the HTML document (called by multiple functions).
exprToHTML :: Expr -> [HTMLBody]
exprToHTML (Dbl d)        = [RawText (T.pack $ showEFloat Nothing d "")]
exprToHTML (Int i)        = [RawText (T.pack $ show i)]
exprToHTML (Str s)        = [RawText $ "\"" <> T.pack s <> "\""]
exprToHTML (Row l)        = concatMap exprToHTML l
exprToHTML (Ident s)      = [RawText (T.pack s)]
exprToHTML (Label s)      = [RawText (T.pack s)]
exprToHTML (Spec s)       = [RawText (T.pack $ unPH $ special s)]
--pExpr (Gr g)         = unPH $ greek g
exprToHTML (Sub e)        = [TextFormat Subscript [] (exprToHTML e)]
exprToHTML (Sup e)        = [TextFormat Superscript [] (exprToHTML e)]
exprToHTML (Over Hat s)   = foldRaw (exprToHTML s ++ [RawText "̂"])
exprToHTML (MO o)         = [RawText (pOps o)]
exprToHTML (Fenced l r e) =
  foldRaw $ [RawText (fence Open l)] ++ exprToHTML e ++ [RawText (fence Close r)]
exprToHTML (Font Bold e)  = [TextFormat HTML.Bold [] (exprToHTML e)]
exprToHTML (Font Emph e)  = [TextFormat Emphasis [] (exprToHTML e)]
exprToHTML (Spc Thin)     = [RawText " "]
-- Uses TeX for Mathjax for all other exprs
exprToHTML e              =
  [RawText $ T.pack $ show $ mjDelimDisp $ printMath $ toMath $ TeX.pExpr e]
  where mjDelimDisp d = text "\\(" <> d <> text "\\)"  -- PRINTMATH RETURNS OLD DOC
-- Non-mathjax
{-
pExpr (Sqrt e)       = text "&radic;(" <> pExpr e <> text ")"
pExpr (Div a b)      = fraction (pExpr a) (pExpr b)
pExpr (Case ps)      = cases ps pExpr
pExpr (Mtx a)        = text "<table class=\"matrix\">\n" <> pMatrix a <> text "</table>"
-}

-- | Converts expression operators into HTML characters (Text format).
pOps :: Ops -> Text
pOps IsIn       = " ⋲ "
pOps Integer    = "ℤ"
pOps Rational   = "ℚ"
pOps Real       = "ℝ"
pOps Natural    = "ℕ"
pOps Boolean    = "𝔹"
pOps Comma      = ","
pOps Prime      = "′"
pOps Log        = "log"
pOps Ln         = "ln"
pOps Sin        = "sin"
pOps Cos        = "cos"
pOps Tan        = "tan"
pOps Sec        = "sec"
pOps Csc        = "csc"
pOps Cot        = "cot"
pOps Arcsin     = "arcsin"
pOps Arccos     = "arccos"
pOps Arctan     = "arctan"
pOps Not        = "¬"
pOps Dim        = "dim"
pOps Exp        = "e"
pOps Neg        = "−"
pOps Cross      = "⨯"
pOps VAdd       = "+"
pOps VSub       = "−"
pOps Dot        = "⋅"
pOps Scale      = " " -- same as Mul
pOps Eq         = " = " -- with spaces?
pOps NEq        = "≠"
pOps Lt         = " < " --thin spaces make these more readable
pOps Gt         = " > "
pOps LEq        = " ≤ "
pOps GEq        = " ≥ "
pOps Impl       = " ⇒ "
pOps Iff        = " ⇔ "
pOps Subt       = "−"
pOps And        = " ∧ "
pOps Or         = " ∨ "
pOps Add        = "+"
pOps Mul        = " "
pOps Summ       = "∑"
pOps Inte       = "∫"
pOps Prod       = "∏"
pOps Point      = "."
pOps Perc       = "%"
pOps LArrow     = " ← "
pOps RArrow     = " → "
pOps ForAll     = " ∀ "
pOps Partial    = "∂"
pOps SAdd       = " + "
pOps SRemove    = " - "
pOps SContains  = " in "
pOps SUnion     = " and "

-- | Allows for open/closed variants of parenthesis, curly brackets, absolute value symbols, and normal symbols.
fence :: OpenClose -> Fence -> Text
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

makeTableHTML :: [String] -> [[Spec]] -> Spec -> Bool -> Spec -> [HTMLBody]
makeTableHTML _ [] _ _ _       = error "No table to print (see PrintHTML)"
makeTableHTML ts (l:lls) r b t =
  if b
    then [HTML.Div wrapperAttrs [tableNode, captionNode]]
    else [HTML.Div wrapperAttrs [tableNode]]
  where
    attrs = [Attr "class" (T.unwords $ map T.pack ts)]
    headerRow = HTML.Row [] (map (THeader [] . specToHTML) l)
    dataRows = map (HTML.Row [] . map (TData [] . specToHTML)) lls
    tableNode = HTML.Table attrs (headerRow : dataRows)
    captionNode = HTML.Paragraph [Attr "class" "caption"] (specToHTML t)
    wrapperAttrs = [Attr "id" (extractText r)]

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.).
makeDefnHTML :: [(String, [LayoutObj])] -> Spec -> [HTMLBody]
makeDefnHTML [] _ = error "Empty definition"
makeDefnHTML ps l =
  let
    attrs  = [Attr "id" (extractText l), Attr "class" "defn-table"]
    refRow = HTML.Row []
      [THeader [] [RawText "Refname"], TData [] [TextFormat HTML.Bold [] (specToHTML l)]]
    dataRows = map (\(f, d) -> HTML.Row []
      [THeader [] [RawText (T.pack f)], TData [] (concatMap loToHTML d)]) ps
  in
    [HTML.Table attrs (refRow : dataRows)]

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists in HTML.
makeListHTML :: ListType -> HTMLBody -- FIXME: ref id's should be folded into the li
makeListHTML (Simple items) = HTML.Div [Attr "class" "list"] $
  map (\(b, e, l) -> HTML.Paragraph (mlrefAttr l)
  (foldRaw $ specToHTML b ++ [RawText ": "] ++ itemToHTML e)) items
makeListHTML (Desc items) = HTML.Div [Attr "class" "list"] $
  map (\(b, e, l) -> HTML.Paragraph (mlrefAttr l)
  (foldRaw $ [TextFormat HTML.Bold [] (specToHTML b), RawText ": "] ++ itemToHTML e)) items
makeListHTML (Ordered items) = HTML.List HTML.Ordered [Attr "class" "list"] $
  map (\(i, l) -> LItem (mlrefAttr l) (itemToHTML i)) items
makeListHTML (Unordered items) = HTML.List HTML.Unordered [Attr "class" "list"] $
  map (\(i, l) -> LItem (mlrefAttr l) (itemToHTML i)) items
makeListHTML (Definitions items) = HTML.List HTML.Unordered [Attr "class" "hide-list-style-no-indent"] $
  map (\(b, e, l) -> LItem (mlrefAttr l) (specToHTML b ++ [RawText " is the "] ++ itemToHTML e)) items

-- | Helper for setting up references as HTML Attributes.
mlrefAttr :: Maybe Spec -> [Attr]
mlrefAttr Nothing  = []
mlrefAttr (Just l) = [Attr "id" (extractText l)]

-- | Helper for rendering list items.
itemToHTML :: ItemType -> [HTMLBody]
itemToHTML (Flat s)     = foldRaw $ specToHTML s
itemToHTML (Nested s l) = foldRaw $ specToHTML s ++ [makeListHTML l]

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**

-- | Makes a bilbliography for the document.
makeBib :: BibRef -> [HTMLBody]
makeBib bib =
  [DescriptionList [Attr "class" "reference-list"] (concatMap renderCitation bib)]
  where
    renderCitation :: Citation -> [DItem]
    renderCitation cite@(Cite e _ _) =
      let (termDoc, detailsDoc) = renderCite htmlBibFormatter cite
          termHTML = [RawText "[", TextFormat HTML.Bold [] termDoc, RawText "]"]
      in [DTerm [Attr "id" (T.pack e)] termHTML, DDetails [] detailsDoc]

-- | HTML specific bib rendering functions
htmlBibFormatter :: BibFormatter
htmlBibFormatter = BibFormatter {
  emph = \x -> [TextFormat Emphasis [] x],
  spec = specToHTML
}

-- | For when we add other things to reference like website, newspaper
renderCite :: BibFormatter -> Citation -> ([HTMLBody], [HTMLBody])
renderCite f (Cite e Book cfs)      = ([RawText $ T.pack e],
  foldRaw (renderF cfs (useStyleBk    f)  ++ [RawText (T.pack $ sufxPrint cfs)]))
renderCite f (Cite e Article cfs)   = ([RawText $ T.pack e],
  foldRaw (renderF cfs (useStyleArtcl f)  ++ [RawText (T.pack $ sufxPrint cfs)]))
renderCite f (Cite e MThesis cfs)   = ([RawText $ T.pack e],
  foldRaw (renderF cfs (useStyleBk    f)  ++ [RawText (T.pack $ sufxPrint cfs)]))
renderCite f (Cite e PhDThesis cfs) = ([RawText $ T.pack e],
  foldRaw (renderF cfs (useStyleBk    f)  ++ [RawText (T.pack $ sufxPrint cfs)]))
renderCite f (Cite e Misc cfs)      = ([RawText $ T.pack e],
  renderF cfs (useStyleBk    f))
renderCite f (Cite e _ cfs)         = ([RawText $ T.pack e],
  renderF cfs (useStyleArtcl f)) --FIXME: Properly render these later.

-- | Render fields to be used in the document.
renderF :: [CiteField] -> (StyleGuide -> (CiteField -> [HTMLBody])) -> [HTMLBody]
renderF fields styl = foldRaw (concatMap (styl bibStyleH) (sortBy compCiteField fields))

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
useStyleBk :: BibFormatter -> StyleGuide -> (CiteField -> [HTMLBody])
useStyleBk f MLA     = bookMLA f
useStyleBk f APA     = bookAPA f
useStyleBk f Chicago = bookChicago f

-- | Renders citation as an article style.
useStyleArtcl :: BibFormatter -> StyleGuide -> (CiteField -> [HTMLBody])
useStyleArtcl f MLA     = artclMLA f
useStyleArtcl f APA     = artclAPA f
useStyleArtcl f Chicago = artclChicago f

-- FIXME: move these show functions and use tags, combinators
-- | Cite books in MLA format.
bookMLA :: BibFormatter -> CiteField -> [HTMLBody]
bookMLA f (Address   s) = foldRaw (spec f s ++ [RawText ": "])
bookMLA _ (Edition   s) = [RawText (T.pack (show s ++ sufxer s ++ " ed., "))]
bookMLA f (Series    s) = emph f (spec f s) ++ [RawText ". "]
bookMLA f (Title     s) = emph f (spec f s) ++ [RawText ". "] --If there is a series or collection, this should be in quotes, not italics
bookMLA _ (Volume    s) = [RawText ("vol. " <> T.pack (show s) <> ", ")]
bookMLA f (Publisher s) = foldRaw (spec f s ++ [RawText ", "])
bookMLA f (Author    p) = foldRaw (spec f (rendPeople' p) ++ [RawText ". "])
bookMLA _ (Year      y) = [RawText (T.pack (show y) <> ". ")]
--bookMLA _ (Date    d m y) = dot $ unwords [show d, show m, show y]
--bookMLA f (URLdate d m y) = "Web. " ++ bookMLA f (Date d m y) sm
bookMLA f (BookTitle s) = emph f (spec f s) ++ [RawText ". "]
bookMLA f (Journal   s) = emph f (spec f s) ++ [RawText ", "]
bookMLA _ (Pages   [p]) = [RawText ("pg. " <> T.pack (show p) <> ". ")]
bookMLA _ (Pages     p) = foldRaw [RawText "pp. ", foldPages p, RawText ". "]
bookMLA f (Note      s) = spec f s
bookMLA _ (Number    n) = [RawText ("no. " <> T.pack (show n) <> ", ")]
bookMLA f (School    s) = foldRaw (spec f s ++ [RawText ", "])
--bookMLA _ (Thesis     t)  = comm $ show t
--bookMLA f (URL        s)  = dot $ spec f s
bookMLA f (HowPublished (Verb s))      = foldRaw (spec f s ++ [RawText ", "])
bookMLA f (HowPublished (URL s)) = [Anchor (extractText s) [] (spec f s), RawText ". "]
bookMLA _ (Editor       p) = foldRaw [RawText "Edited by ", foldPeople p, RawText ", "]
bookMLA _ (Chapter      _) = []
bookMLA f (Institution  i) = foldRaw (spec f i ++ [RawText ", "])
bookMLA f (Organization i) = foldRaw (spec f i ++ [RawText ", "])
bookMLA _ (Month        m) = [RawText (T.pack (show m) <> ", ")]
bookMLA f (Type         t) = foldRaw (spec f t ++ [RawText ", "])

-- | Cite books in APA format.
bookAPA :: BibFormatter -> CiteField -> [HTMLBody] --FIXME: year needs to come after author in APA
bookAPA f (Author   p) = spec f (rendPeople rendPersLFM' p) --L.APA uses initals rather than full name
bookAPA _ (Year     y) = [RawText (T.pack (paren $ show y) <> ". ")]--APA puts "()" around the year
--bookAPA _ (Date _ _ y) = bookAPA (Year y) --LAPA doesn't care about the day or month
--bookAPA _ (URLdate d m y) = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA _ (Pages    p) = foldRaw [foldPages p, RawText ". "]
bookAPA _ (Editor   p) = foldRaw [foldPeople p, RawText " (Ed.). "]
bookAPA f i = bookMLA f i --Most items are rendered the same as MLA

-- | Cite books in Chicago format.
bookChicago :: BibFormatter -> CiteField -> [HTMLBody]
bookChicago f (Author   p) = spec f (rendPeople rendPersLFM'' p) -- APA uses middle initals rather than full name
bookChicago _ (Pages    p) = [foldPages p, RawText ". "]
bookChicago _ (Editor   p) = [foldPeople p, RawText (toPlural p " ed" <> ". ")]
bookChicago f i = bookMLA f i --Most items are rendered the same as MLA

-- for article renderings
-- | Cite articles in MLA format.
artclMLA :: BibFormatter -> CiteField -> [HTMLBody]
artclMLA f (Title s) = [RawText "\""] <> spec f s <> [RawText ".\" "]
artclMLA f i         = bookMLA f i

-- | Cite articles in APA format.
artclAPA :: BibFormatter -> CiteField -> [HTMLBody]
artclAPA f (Title  s)  = spec f s <> [RawText ". "]
artclAPA _ (Volume n)  = [emphasis [] (T.pack (show n))]
artclAPA _ (Number  n) = [RawText (", (" <> T.pack (show n) <> ") ")]
artclAPA f i           = bookAPA f i

-- | Cite articles in Chicago format.
artclChicago :: BibFormatter -> CiteField -> [HTMLBody]
artclChicago f i@(Title    _) = artclMLA f i
artclChicago _ (Volume     n) = [RawText (T.pack (show n) <> ", ")]
artclChicago _ (Number      n) = [RawText ("no. " <> T.pack (show n))]
artclChicago f i@(Year     _) = bookAPA f i
--artclChicago f i@(Date _ _ _) = bookAPA f i
artclChicago f i = bookChicago f i

-- PEOPLE RENDERING --
-- | Render a list of people (after applying a given function).
rendPeople :: (Person -> String) -> People -> Spec
rendPeople _ []  = S "N.a." -- "No authors given"
rendPeople f people = S . foldlList $ map f people --foldlList is in drasil-utils

-- | Render a list of people (of form FirstName LastName).
rendPeople' :: People -> Spec
rendPeople' []  = S "N.a." -- "No authors given"
rendPeople' people = S . foldlList $ map rendPersLFM (init people) ++  [rendPersL (last people)]

-- | Organize a list of pages.
foldPages :: [Int] -> HTMLBody
foldPages = RawText . T.pack . foldlList . numList "–"

-- | Organize a list of people.
foldPeople :: People -> HTMLBody
foldPeople p = RawText . foldlList $ map (T.pack . fullName) p

-- | Organize a list of Strings, separated by commas and inserting "and" before the last item.
foldlList :: (IsString a, Semigroup a) => [a] -> a
foldlList []    = ""
foldlList [a,b] = a <> " and " <> b
foldlList lst   = foldle1 (\a b -> a <> ", " <> b) (\a b -> a <> ", and " <> b) lst

-- | Similar to foldl, but applies a function to two arguments at a time.
foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g (f x y : xs)

-- | Renders a person's last name.
rendPersL :: Person -> String
rendPersL =
  (\n -> (if not (null n) && last n == '.' then init else id) n) . rendPersLFM

-- | adds an 's' if there is more than one person in a list.
toPlural :: People -> Text -> Text
toPlural (_:_) str = str <> "s"
toPlural _     str = str
