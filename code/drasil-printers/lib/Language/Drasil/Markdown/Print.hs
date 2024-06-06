-- | Defines .json printers to generate jupyter notebooks. For more information on each of the helper functions, please view the [source files](https://jacquescarette.github.io/Drasil/docs/full/drasil-printers-0.1.10.0/src/Language.Drasil.JSON.Print.html).
module Language.Drasil.Markdown.Print(genMD) where

import Prelude hiding (print, (<>))
import Text.PrettyPrint hiding (Str)
import Data.List (transpose)
import Data.Maybe (fromMaybe)

import qualified Language.Drasil as L

import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, 
  Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, Sp, S, E, (:+:)), Label, 
  LinkType(Internal, Cite2, External), OverSymb(Hat), Fonts(Emph, Bold), 
  Spacing(Thin), Fence(Abs))
import Language.Drasil.Printing.Citation (BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import Language.Drasil.Printing.Helpers (sqbrac, pipe, bslash, brace, unders, hat)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import qualified Language.Drasil.TeX.Print as TeX (spec, pExpr, fence, OpenClose(..))
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, PrintLaTeX(PL))
import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Print(renderCite)

import Language.Drasil.Markdown.Helpers (h, stripnewLine, image, li, pa, ba, refwrap, 
  reflink, reflinkURI, reflinkInfo, caption, bold, ul, br, stripTabs, docLength)

-- | Generate a Markdown SRS
genMD :: PrintingInformation -> L.Document -> Doc
genMD sm doc = build (makeDocument sm doc)

-- | Build the Markdown Document, called by genMD
build :: Document -> Doc
build (Document t a c) = 
  text "# " <> pSpec t $$
  text "## " <> pSpec a <> text "\n" $$
  print c 

-- | Called by build, uses 'printLO' to render the layout objects in Doc format.
print :: [LayoutObj] -> Doc
print = foldr (($$) . printLO) empty

-----------------------------------------------------------------
------------------- LAYOUT OBJECT PRINTING ----------------------
-----------------------------------------------------------------

-- | Helper for rendering LayoutObjects into JSON
-- printLO is used for generating SRS
printLO :: LayoutObj -> Doc
printLO (Header n contents _)   = empty $$ (h (n + 1) <> pSpec contents) <> text "\n"
printLO (Cell layoutObs)        = vcat (map printLO layoutObs)
printLO (HDiv _ layoutObs _)    = vcat (map printLO layoutObs)
printLO (Paragraph contents)    = empty $$ (stripnewLine (show(pSpec contents))) <> text "\n"
printLO (EqnBlock contents)     = mathEqn <> text "\n"
  where
    mjDelimDisp d  = text "\\\\[" <> stripnewLine (show d) <> text "\\\\]" 
    mathEqn = mjDelimDisp $ spec contents
printLO (Table _ rows r b t)    = empty $$ makeTable rows (pSpec r) b (pSpec t)
printLO (Definition _ ssPs l)   = empty $$ makeDefn ssPs (pSpec l) <> text "\n"
printLO (List t)                = empty $$ makeList t 0 <> text "\n"
printLO (Figure r c f _)        = makeFigure (pSpec r) (pSpec c) (text f)
printLO (Bib bib)               = makeBib bib
printLO Graph{}                 = empty 
printLO CodeBlock {}            = empty

printLO' :: LayoutObj -> Doc
printLO' e = stripnewLine $ show (printLO e)

-----------------------------------------------------------------
----------------------- SPEC PRINTING ---------------------------
-----------------------------------------------------------------

pSpec :: Spec -> Doc
pSpec (E e)                = text "\\\\(" <> pExpr e <> text "\\\\)" -- symbols used
pSpec (a :+: b)            = pSpec a <> pSpec b
pSpec (S s)                = either error (text . concatMap escapeChars) $ L.checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Sp s)               = text $ unPH $ L.special s
pSpec HARDNL               = empty
pSpec (Ref Internal  r a)  = reflink     (text r) (pSpec a)
pSpec (Ref (Cite2 n) r a)  = reflinkInfo (text r) (pSpec a) (pSpec n)
pSpec (Ref External  r a)  = reflinkURI  (text r) (pSpec a)
pSpec EmptyS               = text "" 
pSpec (Quote q)            = doubleQuotes $ pSpec q

spec :: Spec -> Doc
spec (E ex) = pExpr ex
spec e = printMath $ toMathHelper $ TeX.spec e
  where
    toMathHelper (PL g) = PL (\_ -> g Math)

-----------------------------------------------------------------
-------------------- EXPRESSION PRINTING ------------------------
-----------------------------------------------------------------

-- | Print an expression to a document.
pExpr :: Expr -> Doc
pExpr (Str s)        = text "\\text{" <> (quote (text s)) <> text "}"
pExpr (Div n d)      = command2D "frac" (pExpr n) (pExpr d)
pExpr (Case ps)      = mkEnv "cases" (cases ps) --
pExpr (Mtx a)        = stripTabs $ mkEnv "bmatrix" (pMatrix a)
pExpr (Row [x])      = br $ pExpr x -- FIXME: Hack needed for symbols with multiple subscripts, etc.
pExpr (Row l)        = foldl1 (<>) (map pExpr l)
pExpr (Sub e)        = unders <> br (pExpr e)
pExpr (Sup e)        = hat    <> br (pExpr e)
pExpr (Over Hat s)   = commandD "hat" (pExpr s)
pExpr (Fenced l r m) = fence TeX.Open l <> pExpr m <> fence TeX.Close r
pExpr (Font Bold e)  = commandD "boldsymbol" (pExpr e) --
pExpr (Font Emph e)  = pExpr e -- Emph is ignored here because we're in Math mode
pExpr (Spc Thin)     = text "\\\\," --
pExpr (Sqrt e)       = commandD "sqrt" (pExpr e)
pExpr e              = printMath $ toMath $ TeX.pExpr e

-- | Helper for rendering a D from Latex print
printMath :: D -> Doc
printMath = (`runPrint` Math)

commandD :: String -> Doc -> Doc
commandD s c = (bslash <> text s) <> br c

command2D :: String -> Doc -> Doc -> Doc
command2D s a0 a1 = (bslash <> text s) <> br a0 <> br a1

fence :: TeX.OpenClose -> Fence -> Doc
fence _ Abs = printMath $ pure $ text "\\|"
fence a b   = printMath $ TeX.fence a b

-- | For printing a Matrix.
pMatrix :: [[Expr]] -> Doc
pMatrix e = vpunctuate (text "\\\\\\\\") (map pIn e)
  where pIn x = hpunctuate (text " & ") (map pExpr x)

-- | Helper for printing case expression.
cases :: [(Expr,Expr)] -> Doc
cases [] = error "Attempt to create case expression without cases"
cases e  = vpunctuate (text "\\\\\\\\") (map _case e)
  where _case (x, y) = hpunctuate (text ", & ") (map pExpr [x, y])

-- Combine 'TP.vcat' and 'TP.punctuate'.
vpunctuate :: Doc -> [Doc] -> Doc
vpunctuate x = vcat . punctuate x

-- Combine 'TP.hcat' and 'TP.punctuate'.
hpunctuate :: Doc -> [Doc] -> Doc
hpunctuate x = hcat . punctuate x

-- | Helper for adding fencing symbols.
quote :: Doc -> Doc
quote x = lq <> x <> rq
  where
  lq = text "\\\\(\\``\\\\)"
  rq = text "''"

-- | Encapsulate environments.
mkEnv :: String -> Doc -> Doc
mkEnv nm d =
  (text ("\\begin" ++ brace nm)) $$ 
  d $$
  (text ("\\end" ++ brace nm))

-----------------------------------------------------------------
-------------------- TABLE PRINTING -----------------------------
-----------------------------------------------------------------

-- | Renders Markdown table, called by 'printLO'
makeTable :: [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable []      _ _ _  = error "No table to print"
makeTable ls r b t  = 
  refwrap r empty $$
  makeHeaderCols (matrix !! 0) sizes $$
  makeRows (tail matrix) sizes <> text "\n" $$
  capt
    where
      matrix = mkDocMatrix ls
      sizes = columnSize matrix
      capt = if b then bold (caption t) <> text "\n" else empty

-- | Helper for creating a Doc matrix
mkDocMatrix :: [[Spec]] -> [[Doc]]
mkDocMatrix ls = map (map pSpec) ls

-- | Helper for getting table column size
columnSize :: [[Doc]] -> [Int]
columnSize = map (maximum . map docLength) . transpose

-- | Helper for creating table rows
makeRows :: [[Doc]] -> [Int] -> Doc
makeRows lls sizes = foldr (($$) . (flip makeColumns sizes)) empty lls

-- | makeHeaderCols: Helper for creating table header row
-- | makeColumns: Helper for creating table columns
makeHeaderCols, makeColumns :: [Doc] -> [Int] -> Doc
makeHeaderCols l sizes = header $$ seperators
  where header     = pipe <> hcat (punctuate pipe (zipWith makeCell l sizes)) <> pipe        
        seperators = pipe <> hcat (punctuate pipe (map makeDashCell sizes)) <> pipe

makeColumns ls sizes = pipe <> hcat (punctuate pipe (zipWith makeCell ls sizes)) <> pipe

-- | Helper for making table seperation row
makeDashCell :: Int -> Doc
makeDashCell size = text ":" <> text (replicate (size - 1) '-')

-- | Helper for rendering a table cell
makeCell :: Doc -> Int -> Doc
makeCell content size = content <> spaces
  where
    numOfSpaces = size - docLength content
    spaces      = text $ replicate numOfSpaces ' '

-----------------------------------------------------------------
-------------------- DEFINITION PRINTING ------------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: [(String,[LayoutObj])] -> Doc -> Doc
makeDefn [] _ = error "L.Empty definition"
makeDefn ps l = 
  makeDHeaderText docDefn l $$ 
  makeDHeaderRow l size $$ 
  makeDRows docDefn size
  where
    docDefn = mkDocDefn ps
    size = defnCSize docDefn

-- | Helper for convering definition to Doc
mkDocDefn :: [(String,[LayoutObj])] -> [(Doc, Doc)]
mkDocDefn ps = map (\(f, d) -> (text f, makeLO (f,d))) ps

-- | Calculates the maximum cell size of each column in
-- the definition table
defnCSize :: [(Doc, Doc)] -> (Int, Int)
defnCSize ps = (longestLabel, longestLO)
  where
    longestLabel = maximum $ map (docLength . fst) ps
    longestLO    = maximum $ map (docLength . snd) ps

-- | Renders the title/header of the definition table
makeDHeaderText :: [(Doc, Doc)] -> Doc -> Doc
makeDHeaderText ps l = text "## " <> fromMaybe l lo <+> br (text "#" <> l) <> text "\n"
  where
    lo = lookup (text "Label") ps

-- | Renders the header rows of the definition table
makeDHeaderRow :: Doc -> (Int, Int) -> Doc
makeDHeaderRow lbl (l,r) = pipe <> lh <> pipe <> rh <> pipe $$ ls <> rs 
  where
    lh = text "Refname" <> text (replicate (l - 7) ' ')
    rh = lbl <> text (replicate (r - docLength lbl) ' ')
    ls = text "|:" <> text (replicate (l - 1) '-') <> pipe
    rs = text ":" <> text (replicate (r - 1) '-') <> pipe

-- | Renders the rows of the definition table
makeDRows :: [(Doc, Doc)] -> (Int, Int) -> Doc
makeDRows []         _    = error "No fields to create defn table"
makeDRows [(f,d)]    size = makeDRow (f,d) size
makeDRows ((f,d):ps) size = makeDRow (f,d) size $$ makeDRows ps size

-- | Renders a single row of the definition table
makeDRow :: (Doc, Doc) -> (Int, Int) -> Doc
makeDRow (f,d) (l,r) = pipe <> left <> pipe <> right <> pipe
  where
    left  = f <> text (replicate (l - docLength f) ' ')
    right = d <> text (replicate (r - docLength d) ' ')

-- | Converts the [LayoutObj] to a Doc
makeLO :: (String, [LayoutObj]) -> Doc
makeLO (f,d) =
      if f=="Notes" then ul (hcat $ map (processDefnLO f) d) 
      else (hcat $ map (processDefnLO f) d)

processDefnLO :: String -> LayoutObj -> Doc
processDefnLO "Notes" (Paragraph con) = li $ pSpec con
processDefnLO _ lo                    = printLO' lo

-----------------------------------------------------------------
----------------------- LIST PRINTING ---------------------------
-----------------------------------------------------------------

-- | Renders lists
makeList :: ListType -> Int -> Doc
makeList (Simple      items) _  = vcat $ 
  map (\(b,e,l) -> (mlref l empty) $$ (pSpec b <> text ": " <> sItem e <> text "\n") $$ empty) items
makeList (Desc        items) bl = vcat $ 
  map (\(b,e,l) -> pa $ ba $ pSpec b <> text ": " <> pItem e bl) items -- | LOOK INTO WHAT THIS IS
makeList (Ordered     items) bl = vcat $ map (\(i,l) -> mlref l $ pItem i bl) items
makeList (Unordered   items) bl = vcat $ map (\(i,l) -> mlref l $ pItem i bl) items
makeList (Definitions items) _  = ul $ hcat $ map (\(b,e,l) -> li $ mlref l $ pSpec b <> text " is the" <+> sItem e) items

-- | Helper for setting up reference anchors
mlref :: Maybe Label -> Doc -> Doc
mlref = maybe id $ refwrap . pSpec

-- | Helper for rendering bulleted markdown list items
pItem :: ItemType ->  Int -> Doc
pItem (Flat   s)   i = text (replicate i ' ') <> text "- " <> pSpec s
pItem (Nested s l) i = vcat [text (replicate i ' ') <> text "- " <> pSpec s, makeList l (i+2)]

-- | Helper for non-bulleted markdown list items
sItem :: ItemType -> Doc
sItem (Flat   s)   = pSpec s
sItem (Nested s l) = vcat [pSpec s, makeList l 0]

-----------------------------------------------------------------
---------------------- FIGURE PRINTING --------------------------
-----------------------------------------------------------------

-- | Renders figures in HTML
makeFigure :: Doc -> Doc -> Doc -> Doc
makeFigure r c f = (refwrap r empty) $$ (image f c)

-----------------------------------------------------------------
------------------ Bibliography Printing ------------------------
-----------------------------------------------------------------

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = refwrap l (i <> text ": " <> a)

-- | Renders the bibliography
-- | FIXME: currently uses HTML, change to Markdown
makeBib :: BibRef -> Doc
makeBib = vcat .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map renderCite
