-- | Defines main Markdown printer functions.
module Language.Drasil.Markdown.Print(genMD, genMD') where

import Prelude hiding (print, (<>))
import Text.PrettyPrint hiding (Str)
import Data.List (transpose)
import Data.Map (Map, fromList)

import qualified Language.Drasil as L

import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, 
  Expr(..), Spec(Quote, EmptyS, Ref, HARDNL, Sp, S, E, (:+:)), Label, 
  LinkType(Internal, Cite2, External), OverSymb(Hat), Fonts(Emph, Bold), 
  Spacing(Thin), Fence(Abs))
import Language.Drasil.Printing.Citation (BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..),
  Filepath)
import Language.Drasil.Printing.Helpers (sqbrac, pipe, bslash, brace, 
  unders, hat, hyph, dot, nl, tab)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import qualified Language.Drasil.TeX.Print as TeX (spec, pExpr, fence, OpenClose(..))
import Language.Drasil.TeX.Monad (runPrint, MathContext(Math), D, toMath, PrintLaTeX(PL))
import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Print(renderCite)

import Language.Drasil.Markdown.Helpers (h, stripStr, image, li, 
  reflink, reflinkURI, reflinkInfo, caption, bold, ul, br, docLength, divTag, defnHTag)

-- | Generate a Markdown SRS
genMD :: PrintingInformation -> L.Document -> Doc
genMD sm doc = build (makeDocument sm doc)

genMD' :: PrintingInformation -> L.Document -> [(Filepath, Doc)]
genMD' sm doc = build' $ makeDocument sm doc

-- | Build the Markdown Document, called by genMD
build :: Document -> Doc
build (Document t a c) = 
  text "# " <> pSpec t $$
  text "## " <> pSpec a <> nl $$
  print c 

-- | Called by build, uses 'printLO' to render the layout objects in Doc format.
print :: [LayoutObj] -> Doc
print = foldr (($$) . printLO) empty

build' :: Document -> [(Filepath, Doc)]
build' (Document t a c) = convert c

extractHDivs :: Int -> LayoutObj -> [(Filepath, Doc)]
extractHDivs depth lo@(HDiv _ objs label) 
  | depth > 2 = [(show $ pSpec label, 
                printLO lo)]
  | otherwise = (show $ pSpec label, 
                vcat (map printLO (filter (not . isHDiv) objs))) : 
                concatMap (extractHDivs (depth + 1)) objs
extractHDivs _ _ = []

isHDiv :: LayoutObj -> Bool
isHDiv (HDiv _ _ _) = True
isHDiv _ = False

convert :: [LayoutObj] -> [(Filepath, Doc)]
convert = concatMap (extractHDivs 1)

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
    mathEqn = mjDelimDisp $ spec contents
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

pSpec :: Spec -> Doc
pSpec (E e)                = text "\\\\(" <> pExpr e <> text "\\\\)" -- symbols used
pSpec (a :+: b)            = pSpec a <> pSpec b
pSpec (S s)                = either error (text . concatMap escapeChars) $ L.checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Sp s)               = text $ unPH $ L.special s
pSpec HARDNL               = nl
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
pExpr (Mtx a)        = stripStr (mkEnv "bmatrix" (pMatrix a)) tab
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
fence _ Abs = text "\\|"
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

-- | Renders lists
makeList :: ListType -> Int -> Doc
makeList (Simple      items) _  = vcat $ 
  map (\(b,e,l) -> (mlref l) $$ (pSpec b <> text ":" <+> sItem e <> nl)) items
makeList (Desc        items) _  = vcat $ 
  map (\(b,e,l) -> (mlref l) $$ (bold (pSpec b) <> text ":" <+> sItem e <> nl)) items -- | LOOK INTO WHAT THIS IS
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

-- | Renders figures in HTML
makeFigure :: Doc -> Doc -> Doc -> Doc
makeFigure r c f = divTag r $$ (image f c)

-----------------------------------------------------------------
------------------ Bibliography Printing ------------------------
-----------------------------------------------------------------

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = divTag l $$ (i <> text ": " <> a) <> nl

-- | Renders the bibliography
-- | FIXME: currently uses HTML, change to Markdown
makeBib :: BibRef -> Doc
makeBib = vcat .
  zipWith (curry (\(x,(y,z)) -> makeRefList z y x))
  [text $ sqbrac $ show x | x <- [1..] :: [Int]] . map renderCite
