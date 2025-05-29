-- | Defines main Markdown printer functions.
module Language.Drasil.Markdown.Print(genMDBook, pSpec) where

import Prelude hiding (print, (<>))
import qualified Prelude as P ((<>))
import Text.PrettyPrint hiding (Str)
import Data.List (transpose)
import Data.List.Utils (replace)

import qualified Language.Drasil as L
import qualified Language.Drasil.Printing.Import.Sentence as L (spec)
import Language.Drasil.Printing.Import (makeProject)
import Language.Drasil.Printing.Import.Helpers (termStyleLookup)
import Language.Drasil.Printing.AST (ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, 
  Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, E, Ch, (:+:)), Label, 
  LinkType(Internal, Cite2, External), OverSymb(Hat), Fonts(Emph, Bold), 
  Spacing(Thin), Fence(Abs), Ops(Perc, Mul))
import Language.Drasil.Printing.Citation (BibRef)
import Language.Drasil.Printing.LayoutObj (Project(Project), 
  LayoutObj(..), Filename, RefMap, File(File))
import Language.Drasil.Printing.Helpers (sqbrac, pipe, bslash, unders, 
  hat, hyph, dot, ($^$), vsep)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, ckdb)

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
import Control.Lens ((^.))

-----------------------------------------------------------------
------------------------- mdBook SRS ----------------------------
-----------------------------------------------------------------

-- | Generate a mdBook SRS
genMDBook :: PrintingInformation -> L.Document -> [(Filename, Doc)]
genMDBook sm doc = build' sm $ makeProject sm doc

-- | Build the mdBook Docs, called by genMD'
build' :: PrintingInformation -> Project -> [(Filename, Doc)]
build' sm p@(Project _ _ rm d) = 
  printSummary sm rm files : map (print' sm rm) files
  where
    files = createTitleFile p : d

-- | Called by build', uses 'printLO' to render a File 
-- into a single Doc
print' :: PrintingInformation -> RefMap -> File -> (Filename, Doc)
print' sm rm (File _ n _ c) = (n, print sm rm c)

-- | Uses 'printLO' to render the layout objects 
-- into a single Doc
print :: PrintingInformation -> RefMap -> [LayoutObj] -> Doc
print sm rm = vsep . map (printLO sm rm)

-- | Renders a 'SUMMARY.md' file
printSummary :: PrintingInformation -> RefMap -> [File] -> (Filename, Doc)
printSummary sm rm f = ("SUMMARY", vcat $ map (summaryItem sm rm) f)

-- | Helper for rendering a 'SUMMARY.md' item
summaryItem :: PrintingInformation -> RefMap -> File -> Doc
summaryItem sm rm (File t n d _) = bullet <+> lbl <> ref
  where
    bullet = text (replicate (d*2) ' ') <> text "-"
    lbl    = brackets $ pSpec sm rm t
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
printLO :: PrintingInformation -> RefMap -> LayoutObj -> Doc
printLO sm rm (Header n contents l) = h (n+1) <+> heading (pSpec sm rm contents) (pSpec sm rm l)
printLO sm rm (Cell layoutObs)      = print sm rm layoutObs
printLO sm rm (HDiv _ layoutObs _)  = print sm rm layoutObs
printLO sm rm (Paragraph contents)  = pSpec sm rm contents
printLO sm rm (EqnBlock contents)   = text "\\\\[" <> rndr contents <> text "\\\\]"
  where
    rndr (E e) = pExpr e
    rndr c = pSpec sm rm c
printLO sm rm (Table _ rows r b t)  = makeTable sm rm rows (pSpec sm rm r) b (pSpec sm rm t)
printLO sm rm (Definition _ ssPs l) = makeDefn sm rm ssPs (pSpec sm rm l)
printLO sm rm (List t)              = makeList sm rm t 0
printLO sm rm (Figure r c f _)      = makeFigure (pSpec sm rm r) (fmap (pSpec sm rm) c) (text f)
printLO sm rm (Bib bib)             = makeBib sm rm bib
printLO _  _ Graph {}               = empty 
printLO _  _ CodeBlock {}           = empty

-----------------------------------------------------------------
----------------------- SPEC PRINTING ---------------------------
-----------------------------------------------------------------

-- | Helper for rendering Specs into Markdown
pSpec :: PrintingInformation -> RefMap -> Spec -> Doc
pSpec _  _ (E e)      = text "\\\\(" <> pExpr e <> text "\\\\)"
pSpec sm rm (Ch st caps s) = pSpec sm rm $ L.spec sm $ termStyleLookup st (sm ^. ckdb) s caps
pSpec sm rm (a :+: b) = pSpec sm rm a <> pSpec sm rm b
pSpec _  _ HARDNL     = text "\n"
pSpec sm rm (Ref Internal       r a) = reflink     rm r (pSpec sm rm a)
pSpec sm rm (Ref (Cite2 EmptyS) r a) = reflink     rm r (pSpec sm rm a)
pSpec sm rm (Ref (Cite2 n)      r a) = reflinkInfo rm r (pSpec sm rm a) (pSpec sm rm n)
pSpec sm rm (Ref External       r a) = reflinkURI  (text r) (pSpec sm rm a)
pSpec sm rm (Quote q) = doubleQuotes $ pSpec sm rm q
pSpec sm _ s          = HTML.pSpec sm s 

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
makeTable :: PrintingInformation -> RefMap -> [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable _  _  [] _ _ _  = error "No table to print"
makeTable sm rm ls r b t  = 
  divTag r $^$
  makeHeaderCols (head matrix) sizes $$
  makeRows (tail matrix) sizes $^$
  capt
    where
      matrix = mkDocMatrix sm rm ls
      sizes = columnSize matrix
      capt = if b then bold (caption t) else empty

-- | Helper for creating a Doc matrix
mkDocMatrix :: PrintingInformation -> RefMap -> [[Spec]] -> [[Doc]]
mkDocMatrix sm rm = map $ map (pSpec sm rm)

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
makeDefn :: PrintingInformation -> RefMap -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _  _  [] _ = error "L.Empty definition"
makeDefn sm rm ps l = 
  makeDHeaderText sm rm ps l $^$ 
  makeHeaderCols [text "Refname", l] size $$ 
  makeRows docDefn size
  where
    docDefn = mkDocDefn sm rm ps
    size = columnSize docDefn

-- | Helper for convering definition to Doc matrix
mkDocDefn :: PrintingInformation -> RefMap -> [(String,[LayoutObj])] -> [[Doc]]
mkDocDefn sm rm = map (\(f, d) -> [text f, makeLO sm rm (f,d)])

-- | Renders the title/header of the definition table
makeDHeaderText :: PrintingInformation -> RefMap -> [(String, [LayoutObj])] -> Doc -> Doc
makeDHeaderText sm rm ps l = centeredDiv header
  where
    lo = lookup "Label" ps
    c = maybe l (\lo' -> makeLO sm rm ("Label", lo')) lo 
    header = h' 2 <+> heading c l

-- | Converts the [LayoutObj] to a Doc
makeLO :: PrintingInformation -> RefMap -> (String, [LayoutObj]) -> Doc
makeLO sm rm (f,d) =
      if f=="Notes" then ul (hcat $ map (processDefnLO sm rm f) d) 
      else hcat $ map (processDefnLO sm rm f) d

-- | Processes the LayoutObjs in the defn
processDefnLO :: PrintingInformation -> RefMap -> String -> LayoutObj -> Doc
processDefnLO sm rm "Notes" (Paragraph con) = li $ pSpec sm rm con
processDefnLO sm rm _       lo              = printLO sm rm lo

-----------------------------------------------------------------
----------------------- LIST PRINTING ---------------------------
-----------------------------------------------------------------

-- | Renders lists into Markdown
makeList :: PrintingInformation -> RefMap -> ListType -> Int -> Doc
makeList sm rm (Simple      items) _  = vsep $ map (sItem sm rm) items
makeList sm rm (Desc        items) _  = vsep $ map (descItem sm rm) items
makeList sm rm (Ordered     items) bl = vcat $ 
  zipWith (\(i,_) n -> oItem sm rm i bl n) items [1..]
makeList sm rm (Unordered   items) bl = vcat $ 
  map (\(i,_) -> uItem sm rm i bl) items
makeList sm rm (Definitions items) _  = ul $ hcat $ 
  map (\(b,e,_) -> li $ pSpec sm rm b <> text " is the" <+> item sm rm e) items

-- | Helper for setting up reference anchors
mlref :: PrintingInformation -> RefMap -> Maybe Label -> Doc -> Doc
mlref _  _  Nothing  = (empty $$)
mlref sm rm (Just l) = (divTag (pSpec sm rm l) $^$)

-- | Helper for rendering unordered list items
uItem :: PrintingInformation -> RefMap -> ItemType -> Int -> Doc
uItem sm rm (Flat   s)   i = text (replicate i ' ') 
  <> hyph <+> pSpec sm rm s
uItem sm rm (Nested s l) i = vcat [
  text (replicate i ' ') <> hyph <+> pSpec sm rm s, 
  makeList sm rm l (i+2)
  ]

-- | Helper for rendering ordered list items
oItem :: PrintingInformation -> RefMap -> ItemType -> Int -> Int -> Doc
oItem sm rm (Flat   s)   i n = text (replicate i ' ') 
  <> dot (text $ show n) <+> pSpec sm rm s
oItem sm rm (Nested s l) i n = vcat [
  text (replicate i ' ') <> dot (text $ show n) <+> pSpec sm rm s, 
  makeList sm rm l (i+3)
  ]

-- | Helper for Desc list items
descItem :: PrintingInformation -> RefMap -> (Spec, ItemType, Maybe Label) -> Doc
descItem sm rm (b,e,l) = mlref sm rm l (bold (pSpec sm rm b) <> text ":" <+> item sm rm e)

-- | Helper for Simple list items
sItem :: PrintingInformation -> RefMap -> (Spec, ItemType, Maybe Label) -> Doc
sItem sm rm (b,e,l) = mlref sm rm l (pSpec sm rm b <> text ":" <+> item sm rm e)

-- | Helper for Markdown list items
item :: PrintingInformation -> RefMap -> ItemType -> Doc
item sm rm (Flat   s)   = pSpec sm rm s
item sm rm (Nested s l) = vcat [pSpec sm rm s, makeList sm rm l 0]

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
mdBibFormatter :: PrintingInformation -> RefMap -> BibFormatter
mdBibFormatter sm rm = BibFormatter {
  emph = em,
  spec = pSpec sm rm
}

-- | Renders the reference list
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = divTag l $^$ (i <> text ": " <> a)

-- | Renders the bibliography
makeBib :: PrintingInformation -> RefMap -> BibRef -> Doc
makeBib sm rm = vsep .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map (HTML.renderCite (mdBibFormatter sm rm))
