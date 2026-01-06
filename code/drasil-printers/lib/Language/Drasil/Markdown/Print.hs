-- | Defines main Markdown printer functions.
module Language.Drasil.Markdown.Print(genMDBook, pSpec) where

import Prelude hiding (print, (<>))
import qualified Prelude as P ((<>))
import Text.PrettyPrint hiding (Str)
import Data.List (transpose)
import Data.List.Utils (replace)

import Language.Drasil.Printing.AST (ItemType(Flat, Nested),
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr,
  Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, E, (:+:), Tooltip), Label,
  LinkType(Internal, Cite2, External), OverSymb(Hat), Fonts(Emph, Bold),
  Spacing(Thin), Fence(Abs), Ops(Perc, Mul))
import Language.Drasil.Printing.Citation (BibRef)
import Language.Drasil.Printing.LayoutObj (Project(Project),
  LayoutObj(..), Filename, RefMap, File(File))
import Language.Drasil.Printing.Helpers (sqbrac, pipe, bslash, unders,
  hat, hyph, dot, ($^$), vsep)

import qualified Language.Drasil.TeX.Print as TeX (pExpr, fence, OpenClose(..),
  pMatrix, cases)
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, toText,
  hpunctuate)
import qualified Language.Drasil.HTML.Print as HTML (renderCite, pSpec)
import Language.Drasil.HTML.Helpers(BibFormatter(..))
import Language.Drasil.TeX.Helpers(commandD, command2D, mkEnv)

import Language.Drasil.Markdown.Helpers (heading, image, li, reflink,
  reflinkURI, reflinkInfo, caption, bold, ul, docLength, divTag, centeredDiv,
  em, h, h', centeredDivId)

-----------------------------------------------------------------
------------------------- mdBook SRS ----------------------------
-----------------------------------------------------------------

-- | Generate a mdBook SRS
genMDBook :: Project -> [(Filename, Doc)]
genMDBook = build'

-- | Build the mdBook Docs, called by genMD'
build' :: Project -> [(Filename, Doc)]
build' p@(Project _ _ rm d) =
  printSummary rm files : map (print' rm) files
  where
    files = createTitleFile p : d

-- | Called by build', uses 'printLO' to render a File
-- into a single Doc
print' :: RefMap -> File -> (Filename, Doc)
print' rm (File _ n _ c) = (n, print rm c)

-- | Uses 'printLO' to render the layout objects
-- into a single Doc
print :: RefMap -> [LayoutObj] -> Doc
print rm = vsep . map (printLO rm)

-- | Renders a 'SUMMARY.md' file
printSummary :: RefMap -> [File] -> (Filename, Doc)
printSummary rm f = ("SUMMARY", vcat $ map (summaryItem rm) f)

-- | Helper for rendering a 'SUMMARY.md' item
summaryItem :: RefMap -> File -> Doc
summaryItem rm (File t n d _) = bullet <+> lbl <> ref
  where
    bullet = text (replicate (d*2) ' ') <> text "-"
    lbl    = brackets $ pSpec rm t
    ref    = parens $ text $ "./" ++ n ++ ".md"

-- | Create a title page file for a Project
createTitleFile :: Project -> File
createTitleFile (Project t a _ _) = File t "title" 0 cons
  where
    cons = [Header 0 t EmptyS, Paragraph a]

-----------------------------------------------------------------
------------------- LAYOUT OBJECT PRINTING ----------------------
-----------------------------------------------------------------

-- | Helper for rendering LayoutObjects into Markdown
printLO :: RefMap -> LayoutObj -> Doc
printLO rm (Header n contents l) = h (n+1) <+> heading (pSpec rm contents) (pSpec rm l)
printLO rm (Cell layoutObs)      = print rm layoutObs
printLO rm (HDiv _ layoutObs _)  = print rm layoutObs
printLO rm (Paragraph contents)  = pSpec rm contents
printLO rm (EqnBlock contents)   = text "\\\\[" <> rndr contents <> text "\\\\]"
  where
    rndr (E e) = pExpr e
    rndr c = pSpec rm c
printLO rm (Table _ rows r b t)  = makeTable rm rows (pSpec rm r) b (pSpec rm t)
printLO rm (Definition _ ssPs l) = makeDefn rm ssPs (pSpec rm l)
printLO rm (List t)              = makeList rm t 0
printLO rm (Figure r c f _)      = makeFigure (pSpec rm r) (fmap (pSpec rm) c) (text f)
printLO rm (Bib bib)             = makeBib rm bib
printLO _ Graph {}               = empty
printLO _ CodeBlock {}           = empty

-----------------------------------------------------------------
----------------------- SPEC PRINTING ---------------------------
-----------------------------------------------------------------

-- | Helper for rendering Specs into Markdown
pSpec :: RefMap -> Spec -> Doc
pSpec _ (E e)      = text "\\\\(" <> pExpr e <> text "\\\\)"
pSpec rm (Tooltip _ s) = pSpec rm s
pSpec rm (a :+: b) = pSpec rm a <> pSpec rm b
pSpec _ HARDNL     = text "\n"
pSpec rm (Ref Internal       r a) = reflink     rm r (pSpec rm a)
pSpec rm (Ref (Cite2 EmptyS) r a) = reflink     rm r (pSpec rm a)
pSpec rm (Ref (Cite2 n)      r a) = reflinkInfo rm r (pSpec rm a) (pSpec rm n)
pSpec rm (Ref External       r a) = reflinkURI  (text r) (pSpec rm a)
pSpec rm (Quote q) = doubleQuotes $ pSpec rm q
pSpec _ s          = HTML.pSpec s

-----------------------------------------------------------------
-------------------- EXPRESSION PRINTING ------------------------
-----------------------------------------------------------------

-- | Helper for rendering Exprs into mdBook compatiable Mathjax
pExpr :: Expr -> Doc
pExpr (Str s)        = printMath $ toText $ pure $ lq <> text s <> rq
  where
    lq = text "\\\\(\\``\\\\)"
    rq = text "''"
pExpr (Div n d)      = printMath $ command2D "frac" (pExpr' n) (pExpr' d)
pExpr (Case ps)      = printMath $ mkEnv "cases" (P.<>) cases
  where
    cases = TeX.cases ps hpunctuate lnl pExpr'
pExpr (Mtx a)        = printMath $ mkEnv "bmatrix" (P.<>) matrix
  where
    matrix = TeX.pMatrix a hpunctuate lnl pExpr'
pExpr (Row [x])      = braces $ pExpr x
pExpr (Row l)        = foldl1 (<>) (map pExpr l)
pExpr (Label s)      = printMath $ TeX.pExpr (Label s')
  where s' = replace "*" "\\*" (replace "_" "\\_" s)
pExpr (Sub e)        = bslash <> unders <> braces (pExpr e)
pExpr (Sup e)        = hat    <> braces (pExpr e)
pExpr (Over Hat s)   = printMath $ commandD "hat" (pExpr' s)
pExpr (MO o)
  | o == Perc || o == Mul = bslash <> printMath (TeX.pExpr (MO o))
pExpr (Fenced l r m) = fence TeX.Open l <> pExpr m <> fence TeX.Close r
  where
    fence _ Abs = text "\\|"
    fence a b   = printMath $ TeX.fence a b
pExpr (Font Bold e)  = printMath $ commandD "boldsymbol" (pExpr' e)
pExpr (Font Emph e)  = pExpr e
pExpr (Spc Thin)     = text "\\\\,"
pExpr (Sqrt e)       = printMath $ commandD "sqrt" (pExpr' e)
pExpr e              = printMath $ TeX.pExpr e

-- | Print an expression to a LaTeX D
pExpr' :: Expr -> D
pExpr' = pure . pExpr

-- | Helper for rendering a D from LaTeX print
printMath :: D -> Doc
printMath = (`runPrint` Math) . toMath

-- | LaTeX newline command
lnl :: Doc
lnl = text "\\\\\\\\"

-----------------------------------------------------------------
-------------------- TABLE PRINTING -----------------------------
-----------------------------------------------------------------

-- | Renders Markdown table, called by 'printLO'
makeTable :: RefMap -> [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable _  [] _ _ _  = error "No table to print"
makeTable rm ls r b t  =
  divTag r $^$
  makeHeaderCols (head matrix) sizes $$
  makeRows (tail matrix) sizes $^$
  capt
    where
      matrix = mkDocMatrix rm ls
      sizes = columnSize matrix
      capt = if b then bold (caption t) else empty

-- | Helper for creating a Doc matrix
mkDocMatrix :: RefMap -> [[Spec]] -> [[Doc]]
mkDocMatrix rm = map $ map (pSpec rm)

-- | Helper for getting table column size
columnSize :: [[Doc]] -> [Int]
columnSize = map (maximum . map docLength) . transpose

-- | Helper for creating table rows
makeRows :: [[Doc]] -> [Int] -> Doc
makeRows lls sizes = foldr (($$) . (`makeColumns` sizes)) empty lls

-- | makeHeaderCols: Helper for creating table header row
-- | makeColumns: Helper for creating table columns
makeHeaderCols, makeColumns :: [Doc] -> [Int] -> Doc
makeHeaderCols l sizes = header $$ seperators
  where header     = pipe <> hcat (punctuate pipe (zipWith makeCell l sizes)) <> pipe
        seperators = pipe <> hcat (punctuate pipe (map makeDashCell sizes))   <> pipe

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
makeDefn :: RefMap -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _  [] _ = error "L.Empty definition"
makeDefn rm ps l =
  makeDHeaderText rm ps l $^$
  makeHeaderCols [text "Refname", l] size $$
  makeRows docDefn size
  where
    docDefn = mkDocDefn rm ps
    size = columnSize docDefn

-- | Helper for convering definition to Doc matrix
mkDocDefn :: RefMap -> [(String,[LayoutObj])] -> [[Doc]]
mkDocDefn rm = map (\(f, d) -> [text f, makeLO rm (f,d)])

-- | Renders the title/header of the definition table
makeDHeaderText :: RefMap -> [(String, [LayoutObj])] -> Doc -> Doc
makeDHeaderText rm ps l = centeredDiv header
  where
    lo = lookup "Label" ps
    c = maybe l (\lo' -> makeLO rm ("Label", lo')) lo
    header = h' 2 <+> heading c l

-- | Converts the [LayoutObj] to a Doc
makeLO :: RefMap -> (String, [LayoutObj]) -> Doc
makeLO rm (f,d) =
      if f=="Notes" then ul (hcat $ map (processDefnLO rm f) d)
      else hcat $ map (processDefnLO rm f) d

-- | Processes the LayoutObjs in the defn
processDefnLO :: RefMap -> String -> LayoutObj -> Doc
processDefnLO rm "Notes" (Paragraph con) = li $ pSpec rm con
processDefnLO rm _       lo              = printLO rm lo

-----------------------------------------------------------------
----------------------- LIST PRINTING ---------------------------
-----------------------------------------------------------------

-- | Renders lists into Markdown
makeList :: RefMap -> ListType -> Int -> Doc
makeList rm (Simple      items) _  = vsep $ map (sItem rm) items
makeList rm (Desc        items) _  = vsep $ map (descItem rm) items
makeList rm (Ordered     items) bl = vcat $
  zipWith (\(i,_) n -> oItem rm i bl n) items [1..]
makeList rm (Unordered   items) bl = vcat $
  map (\(i,_) -> uItem rm i bl) items
makeList rm (Definitions items) _  = ul $ hcat $
  map (\(b,e,_) -> li $ pSpec rm b <> text " is the" <+> item rm e) items

-- | Helper for setting up reference anchors
mlref :: RefMap -> Maybe Label -> Doc -> Doc
mlref _ Nothing = (empty $$)
mlref rm (Just l) = (divTag (pSpec rm l) $^$)

-- | Helper for rendering unordered list items
uItem :: RefMap -> ItemType -> Int -> Doc
uItem rm (Flat   s)   i = text (replicate i ' ')
  <> hyph <+> pSpec rm s
uItem rm (Nested s l) i = vcat [
  text (replicate i ' ') <> hyph <+> pSpec rm s,
  makeList rm l (i+2)
  ]

-- | Helper for rendering ordered list items
oItem :: RefMap -> ItemType -> Int -> Int -> Doc
oItem rm (Flat   s)   i n = text (replicate i ' ')
  <> dot (text $ show n) <+> pSpec rm s
oItem rm (Nested s l) i n = vcat [
  text (replicate i ' ') <> dot (text $ show n) <+> pSpec rm s,
  makeList rm l (i+3)
  ]

-- | Helper for Desc list items
descItem :: RefMap -> (Spec, ItemType, Maybe Label) -> Doc
descItem rm (b,e,l) = mlref rm l (bold (pSpec rm b) <> text ":" <+> item rm e)

-- | Helper for Simple list items
sItem :: RefMap -> (Spec, ItemType, Maybe Label) -> Doc
sItem rm (b,e,l) = mlref rm l (pSpec rm b <> text ":" <+> item rm e)

-- | Helper for Markdown list items
item :: RefMap -> ItemType -> Doc
item rm (Flat   s)   = pSpec rm s
item rm (Nested s l) = vcat [pSpec rm s, makeList rm l 0]

-----------------------------------------------------------------
---------------------- FIGURE PRINTING --------------------------
-----------------------------------------------------------------

-- | Renders figures in Markdown
makeFigure :: Doc -> Maybe Doc -> Doc -> Doc
makeFigure r c f = centeredDivId r (image f c)

-----------------------------------------------------------------
------------------ Bibliography Printing ------------------------
-----------------------------------------------------------------

-- | Markdown specific bib rendering functions
mdBibFormatter :: RefMap -> BibFormatter
mdBibFormatter rm = BibFormatter {
  emph = em,
  spec = pSpec rm
}

-- | Renders the reference list
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = divTag l $^$ (i <> text ": " <> a)

-- | Renders the bibliography
makeBib :: RefMap -> BibRef -> Doc
makeBib rm = vsep .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map (HTML.renderCite (mdBibFormatter rm))
