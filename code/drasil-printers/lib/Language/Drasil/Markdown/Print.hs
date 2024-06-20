-- | Defines main Markdown printer functions.
module Language.Drasil.Markdown.Print(genMD, genMD') where

import Prelude hiding (print, (<>))
import Text.PrettyPrint hiding (Str)
import Data.List (transpose)

import qualified Language.Drasil as L

import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, 
  Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, E, (:+:)), Label, 
  LinkType(Internal, Cite2, External), OverSymb(Hat), Fonts(Emph, Bold), 
  Spacing(Thin), Fence(Abs), Ops(Perc))
import Language.Drasil.Printing.Citation (BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..),
  Filepath)
import Language.Drasil.Printing.Helpers (sqbrac, pipe, bslash, unders, 
  hat, hyph, dot, nl, tab)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import qualified Language.Drasil.TeX.Print as TeX (pExpr, fence, OpenClose(..),
  pMatrix, cases)
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, toText)
import qualified Language.Drasil.HTML.Print as HTML (renderCite, pSpec)
import Language.Drasil.HTML.Helpers(BibFormatter(..))
import Language.Drasil.TeX.Helpers(commandD, command2D, mkEnv)

import Language.Drasil.Markdown.Helpers (h, stripStr, image, li, reflink, reflinkURI, 
  reflinkInfo, caption, bold, ul, br, docLength, divTag, defnHTag, em)

-- | Generate a single-page Markdown SRS
genMD :: PrintingInformation -> L.Document -> Doc
genMD sm doc = build (makeDocument sm doc)

-- | Generate a multi-page Markdown SRS
genMD' :: PrintingInformation -> L.Document -> [(Filepath, Doc)]
genMD' sm doc = build' $ makeDocument sm doc

-- | Build a single-page Markdown Document, called by genMD
build :: Document -> Doc
build (Document t a c) = 
  text "# " <> pSpec t $$
  text "## " <> pSpec a <> nl $$
  print c 

-- | Build multi-page Markdown Documents, called by genMD'
build' :: Document -> [(Filepath, Doc)]
build' (Document _ _ c) = print' c

-- | Called by build, uses 'printLO' to render the layout objects 
-- into a single Doc
print :: [LayoutObj] -> Doc
print = foldr (($$) . printLO) empty

-- | Called by build', uses 'printLO' to render the layout objects 
-- into a multiple Docs, seperated by sections
print' :: [LayoutObj] -> [(Filepath, Doc)]
print' = concatMap (sepSRS 1)

-- | Helper for seperating SRS section into seperate Docs
sepSRS :: Int -> LayoutObj -> [(Filepath, Doc)]
sepSRS d lo@(HDiv _ los l) 
  | d > 2     = [(show $ pSpec l, 
                printLO lo)]
  | otherwise = (show $ pSpec l, 
                vcat (map printLO (filter (not . isHDiv) los))) : 
                concatMap (sepSRS (d + 1)) los
sepSRS _ _ = []

-- | Helper for checking whether a LayoutObj is an HDiv
isHDiv :: LayoutObj -> Bool
isHDiv (HDiv _ _ _) = True
isHDiv _ = False

-----------------------------------------------------------------
------------------- LAYOUT OBJECT PRINTING ----------------------
-----------------------------------------------------------------

-- | Helper for rendering LayoutObjects into Markdown
printLO :: LayoutObj -> Doc
printLO (Header n contents _)   = (h (n + 1) <> pSpec contents) <> nl
printLO (Cell layoutObs)        = vcat (map printLO layoutObs)
printLO (HDiv _ layoutObs _)    = vcat (map printLO layoutObs)
printLO (Paragraph contents)    = (stripStr (pSpec contents) nl) <> nl
printLO (EqnBlock contents)     = mathEqn <> nl
  where
    mjDelimDisp d  = text "\\\\[" <> stripStr d nl <> text "\\\\]" 
    mathEqn = mjDelimDisp $ rndr contents
    rndr (E e) = pExpr e
    rndr c = pSpec c
printLO (Table _ rows r b t)    = makeTable rows (pSpec r) b (pSpec t)
printLO (Definition _ ssPs l)   = makeDefn ssPs (pSpec l) <> nl
printLO (List t)                = makeList t 0 <> nl
printLO (Figure r c f _)        = makeFigure (pSpec r) (pSpec c) (text f)
printLO (Bib bib)               = makeBib bib
printLO Graph{}                 = empty 
printLO CodeBlock {}            = empty

-- | Helper for rendering LayoutObj into Markdown Tables
printLO' :: LayoutObj -> Doc
printLO' e = stripStr (printLO e) nl

-----------------------------------------------------------------
----------------------- SPEC PRINTING ---------------------------
-----------------------------------------------------------------

-- | Helper for rendering Specs into Markdown
pSpec :: Spec -> Doc
pSpec (E e)     = text "\\\\(" <> pExpr e <> text "\\\\)" -- symbols used
pSpec (a :+: b) = pSpec a <> pSpec b
pSpec HARDNL    = nl
pSpec (Ref Internal       r a) = reflink     (text r) (pSpec a)
pSpec (Ref (Cite2 EmptyS) r a) = reflink     (text r) (pSpec a)
pSpec (Ref (Cite2 n)      r a) = reflinkInfo (text r) (pSpec a) (pSpec n)
pSpec (Ref External       r a) = reflinkURI  (text r) (pSpec a)
pSpec (Quote q) = doubleQuotes $ pSpec q
pSpec s         = HTML.pSpec s 

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
pExpr (Case ps)      = printMath $ mkEnv "cases" (TeX.cases ps lnl pExpr')
pExpr (Mtx a)        = stripStr (printMath $ mkEnv "bmatrix" (TeX.pMatrix a lnl pExpr')) tab
pExpr (Row [x])      = br $ pExpr x 
pExpr (Row l)        = foldl1 (<>) (map pExpr l)
pExpr (Sub e)        = bslash <> unders <> br (pExpr e)
pExpr (Sup e)        = hat    <> br (pExpr e)
pExpr (Over Hat s)   = printMath $ commandD "hat" (pExpr' s)
pExpr (MO Perc)      = bslash <> (printMath $ TeX.pExpr (MO Perc))
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
makeTable :: [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable [] _ _ _  = error "No table to print"
makeTable ls r b t  = 
  divTag r $$
  makeHeaderCols (matrix !! 0) sizes $$
  makeRows (tail matrix) sizes <> nl $$
  capt
    where
      matrix = mkDocMatrix ls
      sizes = columnSize matrix
      capt = if b then bold (caption t) <> nl else empty

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
makeDefn :: [(String,[LayoutObj])] -> Doc -> Doc
makeDefn [] _ = error "L.Empty definition"
makeDefn ps l = 
  makeDHeaderText ps l $$ 
  makeHeaderCols [text "Refname", l] size $$ 
  makeRows docDefn size
  where
    docDefn = mkDocDefn ps
    size = columnSize docDefn

-- | Helper for convering definition to Doc matrix
mkDocDefn :: [(String,[LayoutObj])] -> [[Doc]]
mkDocDefn ps = map (\(f, d) -> [text f, makeLO (f,d)]) ps

-- | Renders the title/header of the definition table
makeDHeaderText :: [(String, [LayoutObj])] -> Doc -> Doc
makeDHeaderText ps l = defnHTag header <> nl
  where
    lo = lookup "Label" ps
    c = maybe l (\lo' -> makeLO ("Label", lo')) lo 
    header = text "\n##" <+> c <+> br (text "#" <> l) <> nl

-- | Converts the [LayoutObj] to a Doc
makeLO :: (String, [LayoutObj]) -> Doc
makeLO (f,d) =
      if f=="Notes" then ul (hcat $ map (processDefnLO f) d) 
      else (hcat $ map (processDefnLO f) d)

-- | Processes the LayoutObjs in the defn
processDefnLO :: String -> LayoutObj -> Doc
processDefnLO "Notes" (Paragraph con) = li $ pSpec con
processDefnLO _ lo                    = printLO' lo

-----------------------------------------------------------------
----------------------- LIST PRINTING ---------------------------
-----------------------------------------------------------------

-- | Renders lists into Markdown
makeList :: ListType -> Int -> Doc
makeList (Simple      items) _  = vcat $ 
  map (\(b,e,l) -> (mlref l) $$ (pSpec b <> text ":" <+> sItem e <> nl)) items
makeList (Desc        items) _  = vcat $ 
  map (\(b,e,l) -> (mlref l) $$ (bold (pSpec b) <> text ":" <+> sItem e <> nl)) items
makeList (Ordered     items) bl = vcat $ 
  zipWith (\(i,_) n -> oItem i bl n) items [1..]
makeList (Unordered   items) bl = vcat $ 
  map (\(i,_) -> uItem i bl) items
makeList (Definitions items) _  = ul $ hcat $ 
  map (\(b,e,_) -> li $ pSpec b <> text " is the" <+> sItem e) items

-- | Helper for setting up reference anchors
mlref :: Maybe Label -> Doc
mlref ml = case ml of
  Just l -> divTag $ pSpec l
  Nothing -> empty

-- | Helper for rendering unordered list items
uItem :: ItemType -> Int -> Doc
uItem (Flat   s)   i = text (replicate i ' ') <> hyph <+> pSpec s
uItem (Nested s l) i = vcat [text (replicate i ' ') <> hyph <+> pSpec s, makeList l (i+2)]

-- | Helper for rendering ordered list items
oItem :: ItemType -> Int -> Int -> Doc
oItem (Flat   s)   i n = text (replicate i ' ') <> dot (text $ show n) <+> pSpec s
oItem (Nested s l) i n = vcat [text (replicate i ' ') <> dot (text $ show n) <+> pSpec s, makeList l (i+3)]

-- | Helper for non-bulleted markdown list items
sItem :: ItemType -> Doc
sItem (Flat   s)   = pSpec s
sItem (Nested s l) = vcat [pSpec s, makeList l 0]

-----------------------------------------------------------------
---------------------- FIGURE PRINTING --------------------------
-----------------------------------------------------------------

-- | Renders figures in Markdown
makeFigure :: Doc -> Doc -> Doc -> Doc
makeFigure r c f = divTag r $$ (image f c)

-----------------------------------------------------------------
------------------ Bibliography Printing ------------------------
-----------------------------------------------------------------

-- | Markdown specific bib rendering functions
mdBibFormatter :: BibFormatter
mdBibFormatter = BibFormatter {
  emph = em,
  spec = pSpec
}

-- | Renders the reference list
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = divTag l $$ (i <> text ": " <> a) <> nl

-- | Renders the bibliography
-- | FIXME: currently uses HTML, change to Markdown
makeBib :: BibRef -> Doc
makeBib = vcat .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map (HTML.renderCite mdBibFormatter)
