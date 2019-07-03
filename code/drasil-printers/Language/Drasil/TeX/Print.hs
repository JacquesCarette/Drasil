module Language.Drasil.TeX.Print(genTeX) where

import Prelude hiding (print)
import Data.List (intersperse, transpose, partition)
import Text.PrettyPrint (text, (<+>))
import qualified Text.PrettyPrint as TP
import Numeric (showEFloat)
import Control.Applicative (pure)
import Control.Arrow (second)

import qualified Language.Drasil as L (
  RenderSpecial(..), People, rendPersLFM,
  CitationKind(..), Month(..), Symbol(..), Sentence(S), (+:+), MaxWidthPercent,
  Decoration(Prime, Hat, Vector), Document, special, USymb(US)) 

import Language.Drasil.Config (colAwidth, colBwidth, bibStyleT, bibFname)
import Language.Drasil.Printing.AST (Spec, ItemType(Nested, Flat), 
  ListType(Ordered, Unordered, Desc, Definitions, Simple), 
  Spec(Quote, EmptyS, Ref, S, Sy, Sp, HARDNL, E, (:+:)), 
  Fence(Norm, Abs, Curly, Paren), Expr, 
  Ops(Inte, Prod, Summ, Mul, Add, Or, And, Subt, Iff, LEq, GEq, 
  NEq, Eq, Gt, Lt, Impl, Dot, Cross, Neg, Exp, Dim, Not, Arctan, Arccos, Arcsin,
  Cot, Csc, Sec, Tan, Cos, Sin, Log, Ln, Prime, Comma, Boolean, Real, Natural, 
  Rational, Integer, IsIn, Point, Perc), Spacing(Thin), Fonts(Emph, Bold), 
  Expr(Spc, Sqrt, Font, Fenced, MO, Over, Sup, Sub, Ident, Spec, Row, 
  Mtx, Div, Case, Str, Int, Dbl), OverSymb(Hat), Label,
  LinkType(Internal, Cite2, External))
import Language.Drasil.Printing.Citation (HP(Verb, URL), CiteField(HowPublished, 
  Year, Volume, Type, Title, Series, School, Publisher, Organization, Pages,
  Month, Number, Note, Journal, Editor, Chapter, Institution, Edition, BookTitle,
  Author, Address), Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import qualified Language.Drasil.Printing.Import as I
import Language.Drasil.Printing.Helpers hiding (paren, sqbrac)
import Language.Drasil.TeX.Helpers (label, caption, centering, mkEnv, item', description,
  includegraphics, center, figure, item, symbDescription, enumerate, itemize, toEqn, empty,
  newline, superscript, parens, fraction, quote, externalref,
  snref, cite, citeInfo, sec, newpage, maketoc, maketitle, document, author, title, bold)
import Language.Drasil.TeX.Monad (D, MathContext(Curr, Math, Text), vcat, (%%),
  toMath, switch, unPL, lub, hpunctuate, toText, ($+$), runPrint)
import Language.Drasil.TeX.Preamble (genPreamble)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

genTeX :: L.Document -> PrintingInformation -> TP.Doc
genTeX doc sm = runPrint (buildStd sm $ I.makeDocument sm doc) Text

buildStd :: PrintingInformation -> Document -> D
buildStd sm (Document t a c) =
  genPreamble c %%
  title (spec t) %%
  author (spec a) %%
  document (maketitle %% maketoc %% newpage %% print sm c)

-- clean until here; lo needs its sub-functions fixed first though
lo :: LayoutObj -> PrintingInformation -> D
lo (Header d t l)       _  = sec d (spec t) %% label (spec l)
lo (HDiv _ con _)       sm = print sm con -- FIXME ignoring 2 arguments?
lo (Paragraph contents) _  = toText $ spec contents
lo (EqnBlock contents)  _  = makeEquation contents
lo (Derivation h d)      sm = newline %% toText (bold $ spec h) %% newline %%
  foldr ((%%) . toText . (`lo` sm)) empty d
lo (Table _ rows r bl t) _  = toText $ makeTable rows (spec r) bl (spec t)
lo (Definition _ ssPs l) sm  = toText $ makeDefn sm ssPs $ spec l
lo (List l)               _  = toText $ makeList l
lo (Figure r c f wp)      _  = toText $ makeFigure (spec r) (spec c) f wp
lo (Bib bib)            sm = toText $ makeBib sm bib
lo (Graph ps w h c l)   _  = toText $ makeGraph
  (map (\(a,b) -> (spec a, spec b)) ps)
  (pure $ text $ maybe "" (\x -> "text width = " ++ show x ++ "em ,") w)
  (pure $ text $ maybe "" (\x -> "minimum height = " ++ show x ++ "em, ") h)
  (spec c) (spec l)

print :: PrintingInformation -> [LayoutObj] -> D
print sm = foldr (($+$) . (`lo` sm)) empty

------------------ Symbol ----------------------------
symbol :: L.Symbol -> String
symbol (L.Atomic s)  = s
symbol (L.Special s) = unPL $ L.special s
symbol (L.Concat sl) = concatMap symbol sl
--
-- handle the special cases first, then general case
symbol (L.Corners [] [] [x] [] s) = brace $ symbol s ++ "^" ++ brace (symbol x)
symbol (L.Corners [] [] [] [x] s) = brace $ symbol s ++ "_" ++ brace (symbol x)
symbol (L.Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (L.Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol L.Corners{}                = error "rendering of Corners (general)"
symbol (L.Atop f s)               = sFormat f s
symbol L.Empty                    = ""

sFormat :: L.Decoration -> L.Symbol -> String
sFormat L.Hat    s = "\\hat{" ++ symbol s ++ "}"
sFormat L.Vector s = "\\mathbf{" ++ symbol s ++ "}"
sFormat L.Prime  s = symbol s ++ "'"

data OpenClose = Open | Close

-----------------------------------------------------------------
------------------ EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- (Since this is all implicitly in Math, leave it as String for now)
pExpr :: Expr -> String
pExpr (Dbl d)    = showEFloat Nothing d ""
pExpr (Int i)    = show i
pExpr (Str s)    = s  -- FIXME this is probably the wrong way to print strings
pExpr (Div n d) = "\\frac{" ++ pExpr n ++ "}{" ++ pExpr d ++"}"
pExpr (Case ps)  = "\\begin{cases}\n" ++ cases ps ++ "\n\\end{cases}"
pExpr (Mtx a)    = "\\begin{bmatrix}\n" ++ pMatrix a ++ "\n\\end{bmatrix}"
pExpr (Row [x]) = brace $ pExpr x -- a bit of a hack...
pExpr (Row l) = concatMap pExpr l
pExpr (Ident s) = s
pExpr (Spec s) = unPL $ L.special s
--pExpr (Gr g) = unPL $ greek g
pExpr (Sub e) = "_" ++ brace (pExpr e)
pExpr (Sup e) = "^" ++ brace (pExpr e)
pExpr (Over Hat s)     = "\\hat{" ++ pExpr s ++ "}"
pExpr (MO o) = pOps o
pExpr (Fenced l r m)    = fence Open l ++ pExpr m ++ fence Close r
pExpr (Font Bold e) = "\\mathbf{" ++ pExpr e ++ "}"
pExpr (Font Emph e) = pExpr e -- Emph is ignored here because we're in Math mode
pExpr (Spc Thin) = "\\,"
pExpr (Sqrt e)  = "\\sqrt{" ++ pExpr e ++ "}"

pOps :: Ops -> String
pOps IsIn     = "\\in{}"
pOps Integer  = "\\mathbb{Z}"
pOps Rational = "\\mathbb{Q}"
pOps Real     = "\\mathbb{R}"
pOps Natural  = "\\mathbb{N}"
pOps Boolean  = "\\mathbb{B}"
pOps Comma    = ","
pOps Prime    = "'"
pOps Log      = "\\log"
pOps Ln       = "\\ln"
pOps Sin      = "\\sin"
pOps Cos      = "\\cos"
pOps Tan      = "\\tan"
pOps Sec      = "\\sec"
pOps Csc      = "\\csc"
pOps Cot      = "\\cot"
pOps Arcsin   = "\\arcsin"
pOps Arccos   = "\\arccos"
pOps Arctan   = "\\arctan"
pOps Not      = "\\neg{}"
pOps Dim      = "\\mathsf{dim}"
pOps Exp      = "e"
pOps Neg      = "-"
pOps Cross    = "\\times"
pOps Dot      = "\\cdot{}"
pOps Eq       = "="
pOps NEq      = "\\neq{}"
pOps Lt       = "<"
pOps Gt       = ">"
pOps GEq      = "\\geq{}"
pOps LEq      = "\\leq{}"
pOps Impl     = "\\implies{}"
pOps Iff      = "\\iff{}"
pOps Subt     = "-"
pOps And      = "\\land{}"
pOps Or       = "\\lor{}"
pOps Add      = "+"
pOps Mul      = " "
pOps Summ     = "\\displaystyle\\sum"
pOps Prod     = "\\displaystyle\\prod"
pOps Inte     = "\\int"
pOps Point    = "."
pOps Perc     = "\\%"

fence :: OpenClose -> Fence -> String
fence Open Paren = "\\left("
fence Close Paren = "\\right)"
fence Open Curly = "\\{"
fence Close Curly = "\\}"
fence _ Abs = "|"
fence _ Norm = "||"

-- | For printing Matrix
pMatrix :: [[Expr]] -> String
pMatrix [] = ""
pMatrix [x] = pIn x
pMatrix (x:xs) = pMatrix [x] ++ "\\\\\n" ++ pMatrix xs

pIn :: [Expr] -> String
pIn [] = ""
pIn [x] = pExpr x
pIn (x:xs) = pIn [x] ++ " & " ++ pIn xs

cases :: [(Expr,Expr)] -> String
cases []     = error "Attempt to create case expression without cases"
cases [p]    = pExpr (fst p) ++ ", & " ++ pExpr (snd p)
cases (p:ps) = cases [p] ++ "\\\\\n" ++ cases ps

-----------------------------------------------------------------
------------------ TABLE PRINTING---------------------------
-----------------------------------------------------------------

makeTable :: [[Spec]] -> D -> Bool -> D -> D
makeTable lls r bool t =
  pure (text ("\\begin{" ++ ltab ++ "}" ++ (brace . unwords . anyBig) lls))
  %% pure (text "\\toprule")
  %% makeRows [head lls]
  %% pure (text "\\midrule")
  %% pure (text "\\endhead")
  %% makeRows (tail lls)
  %% pure (text "\\bottomrule")
  %% (if bool then caption t else caption empty)
  %% label r
  %% pure (text ("\\end{" ++ ltab ++ "}"))
  where ltab = tabType $ anyLong lls
        tabType True  = ltabu
        tabType False = ltable
        ltabu  = "longtabu" --Only needed if "X[l]" is used
        ltable = "longtable"
        descr True  = "X[l]"
        descr False = "l"
  --returns "X[l]" for columns with long fields
        anyLong = any longColumn . transpose
        anyBig = map (descr . longColumn) . transpose
        longColumn = any (\x -> specLength x > 50)

-- | determines the length of a Spec
specLength :: Spec -> Int
specLength (S x)     = length x
specLength (E x)     = length $ filter (`notElem` dontCount) $ pExpr x
specLength (Sy _)    = 1
specLength (a :+: b) = specLength a + specLength b
specLength EmptyS    = 0
specLength _         = 0

dontCount :: String
dontCount = "\\/[]{}()_^$:"

makeRows :: [[Spec]] -> D
makeRows = foldr (\c -> (%%) (makeColumns c %% pure dbs)) empty

makeColumns :: [Spec] -> D
makeColumns ls = hpunctuate (text " & ") $ map spec ls

------------------ Spec -----------------------------------

needs :: Spec -> MathContext
needs (a :+: b) = needs a `lub` needs b
needs (S _)            = Text
needs (E _)            = Math
needs (Sy _)           = Text
needs (Sp _)           = Math
needs HARDNL           = Text
needs Ref{}            = Text
needs EmptyS           = Text
needs (Quote _)        = Text

-- print all Spec through here
spec :: Spec -> D
spec a@(s :+: t) = s' <> t'
  where
    ctx = const $ needs a
    s' = switch ctx $ spec s
    t' = switch ctx $ spec t
spec (E ex) = toMath $ pure $ text $ pExpr ex
spec (S s)  = pure $ text (concatMap escapeChars s)
spec (Sy s) = pUnit s
spec (Sp s) = pure $ text $ unPL $ L.special s
spec HARDNL = pure $ text "\\newline"
spec (Ref Internal r sn) = snref r $ spec sn
spec (Ref Cite2    r EmptyS) = cite (pure $ text r)
spec (Ref Cite2    r i)      = citeInfo (pure $ text r) (spec i)
spec (Ref External r sn) = externalref r $ spec sn
spec EmptyS              = empty
spec (Quote q)           = quote $ spec q

escapeChars :: Char -> String
escapeChars '_' = "\\_"
escapeChars c = [c]

symbolNeeds :: L.Symbol -> MathContext
symbolNeeds (L.Atomic _)          = Text
symbolNeeds (L.Special _)         = Math
symbolNeeds (L.Concat [])         = Math
symbolNeeds (L.Concat (s:_))      = symbolNeeds s
symbolNeeds L.Corners{}           = Math
symbolNeeds (L.Atop _ _)          = Math
symbolNeeds L.Empty               = Curr

pUnit :: L.USymb -> D
pUnit (L.US ls) = formatu t b
  where
    (t,b) = partition ((> 0) . snd) ls
    formatu :: [(L.Symbol,Integer)] -> [(L.Symbol,Integer)] -> D
    formatu [] l = line l
    formatu l [] = foldr ((<>) . pow) empty l
    formatu nu de = toMath $ fraction (line nu) $ line $ map (second negate) de
    line :: [(L.Symbol,Integer)] -> D
    line []  = empty
    line [n] = pow n
    line l   = parens $ foldr ((<>) . pow) empty l
    pow :: (L.Symbol,Integer) -> D
    pow (n,1) = p_symb n
    pow (n,p) = toMath $ superscript (p_symb n) (pure $ text $ show p)
    -- printing of unit symbols is done weirdly... FIXME?
    p_symb (L.Concat s) = foldl (<>) empty $ map p_symb s
    p_symb n = let cn = symbolNeeds n in switch (const cn) $ pure $ text $ symbol n

{-
pUnit :: L.USymb -> D
pUnit (UName (Concat s)) = foldl (<>) empty $ map (pUnit . UName) s
pUnit (UName n) =
  let cn = symbolNeeds n in
  switch (const cn) (pure $ text $ symbol n)
pUnit (UProd l) = foldr (<>) empty (map pUnit l)
pUnit (UPow n p) = toMath $ superscript (pUnit n) (pure $ text $ show p)
pUnit (UDiv n d) = toMath $
  case d of -- 4 possible cases, 2 need parentheses, 2 don't
    UProd _  -> fraction (pUnit n) (parens $ pUnit d)
    UDiv _ _ -> fraction (pUnit n) (parens $ pUnit d)
    _        -> fraction (pUnit n) (pUnit d)
-}

-----------------------------------------------------------------
------------------ DATA DEFINITION PRINTING-----------------
-----------------------------------------------------------------

makeDefn :: PrintingInformation -> [(String,[LayoutObj])] -> D -> D
makeDefn _  [] _ = error "Empty definition"
makeDefn sm ps l = beginDefn %% makeDefTable sm ps l %% endDefn

beginDefn :: D
beginDefn = newline
  %% pure (text "\\noindent \\begin{minipage}{\\textwidth}")

endDefn :: D
endDefn = pure (text "\\end{minipage}")

makeDefTable :: PrintingInformation -> [(String,[LayoutObj])] -> D -> D
makeDefTable _ [] _ = error "Trying to make empty Data Defn"
makeDefTable sm ps l = vcat [
  pure $ text 
  $ "\\begin{tabular}{p{"++show colAwidth++"\\textwidth} p{"++show colBwidth++"\\textwidth}}",
  pure (text "\\toprule \\textbf{Refname} & \\textbf{") <> l <> pure (text "}"), --shortname instead of refname?
  pure (text "\\phantomsection "), label l,
  makeDRows sm ps,
  pure $ dbs <+> text "\\bottomrule \\end{tabular}"
  ]

makeDRows :: PrintingInformation -> [(String,[LayoutObj])] -> D
makeDRows _  []         = error "No fields to create Defn table"
makeDRows sm [(f,d)]    = dBoilerplate %% pure (text (f ++ " & ")) <>
  vcat (map (`lo` sm) d)
makeDRows sm ((f,d):ps) = dBoilerplate %% (pure (text (f ++ " & ")) <>
  vcat (map (`lo` sm) d))
                       %% makeDRows sm ps
dBoilerplate :: D
dBoilerplate = pure $ dbs <+> text "\\midrule" <+> dbs

-----------------------------------------------------------------
------------------ EQUATION PRINTING------------------------
-----------------------------------------------------------------

makeEquation :: Spec -> D
makeEquation contents = toEqn (spec contents)

  --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  --  on chunk (i.e. "eq:h_g" for h_g = ...

-----------------------------------------------------------------
------------------ LIST PRINTING----------------------------
-----------------------------------------------------------------

makeList :: ListType -> D
makeList (Simple items)      = itemize     $ vcat $ simItem items
makeList (Desc items)        = description $ vcat $ simItem items
makeList (Unordered items)   = itemize     $ vcat $ map plItem items
makeList (Ordered items)     = enumerate   $ vcat $ map plItem items
makeList (Definitions items) = symbDescription $ vcat $ defItem items

plItem :: (ItemType,Maybe Label) -> D
plItem (i, l) = mlref l <> pItem i

lspec :: Spec -> D  -- FIXME: Should be option rolled in to spec
lspec (S s) = pure $ text s
lspec r = spec r

mlref :: Maybe Label -> D
mlref = maybe empty $ (<>) (pure $ text "\\phantomsection") . label . lspec

pItem :: ItemType -> D
pItem (Flat s) = item $ spec s
pItem (Nested t s) = vcat [item $ spec t, makeList s]

simItem :: [(Spec,ItemType,Maybe Label)] -> [D]
simItem = map (\(x,y,l) -> item' (spec (x :+: S ":") <> mlref l) $ sp_item y)
  where sp_item (Flat s) = spec s
        sp_item (Nested t s) = vcat [spec t, makeList s]

defItem :: [(Spec, ItemType,Maybe Label)] -> [D]
defItem = map (\(x,y,l) -> item $ mlref l <> spec (x :+: S " is the " :+: d_item y))
  where d_item (Flat s) = s
        d_item (Nested _ _) = error "Cannot use sublists in definitions"
-----------------------------------------------------------------
------------------ FIGURE PRINTING--------------------------
-----------------------------------------------------------------

makeFigure :: D -> D -> String -> L.MaxWidthPercent -> D
makeFigure r c f wp =
  figure (center (
  vcat [
    includegraphics wp f,
    caption c,
    label r
  ] ) )

-----------------------------------------------------------------
------------------ EXPR OP PRINTING-------------------------
-----------------------------------------------------------------
-- p_op :: Functional -> Expr -> String
-- p_op f@(Summation bs) x = oper f ++ makeBound bs ++ brace (sqbrac (pExpr x))
-- p_op f@(Product bs) x = oper f ++ makeBound bs ++ brace (pExpr x)
-- p_op f@(Integral bs wrtc) x = oper f ++ makeIBound bs ++ 
--   brace (pExpr x ++ "d" ++ symbol wrtc) -- HACK alert.
-- 
-- makeBound :: Maybe ((Symbol, Expr),Expr) -> String
-- makeBound (Just ((s,v),hi)) = "_" ++ brace ((symbol s ++"="++ pExpr v)) ++
--                               "^" ++ brace (pExpr hi)
-- makeBound Nothing = ""
-- 
-- makeIBound :: (Maybe Expr, Maybe Expr) -> String
-- makeIBound (Just low, Just high) = "_" ++ brace (pExpr low) ++
--                                    "^" ++ brace (pExpr high)
-- makeIBound (Just low, Nothing)   = "_" ++ brace (pExpr low)
-- makeIBound (Nothing, Just high)  = "^" ++ brace (pExpr high)
-- makeIBound (Nothing, Nothing)    = ""

-----------------------------------------------------------------
------------------ MODULE PRINTING----------------------------
-----------------------------------------------------------------

makeGraph :: [(D,D)] -> D -> D -> D -> D -> D
makeGraph ps w h c l =
  mkEnv "figure" $
  vcat $ [ centering,
           pure $ text "\\begin{adjustbox}{max width=\\textwidth}",
           pure $ text "\\begin{tikzpicture}[>=latex,line join=bevel]",
           pure (text "\\tikzstyle{n} = [draw, shape=rectangle, ") <>
             w <> h <> pure (text "font=\\Large, align=center]"),
           pure $ text "\\begin{dot2tex}[dot, codeonly, options=-t raw]",
           pure $ text "digraph G {",
           pure $ text "graph [sep = 0. esep = 0, nodesep = 0.1, ranksep = 2];",
           pure $ text "node [style = \"n\"];"
         ]
     ++  map (\(a,b) -> q a <> pure (text " -> ") <> q b <> pure (text ";")) ps
     ++  [ pure $ text "}",
           pure $ text "\\end{dot2tex}",
           pure $ text "\\end{tikzpicture}",
           pure $ text "\\end{adjustbox}",
           caption c,
           label l
         ]
  where q x = pure (text "\"") <> x <> pure (text "\"")

---------------------------
-- Bibliography Printing --
---------------------------
-- **THE MAIN FUNCTION** --
makeBib :: PrintingInformation -> BibRef -> D
makeBib sm bib = spec $
  S ("\\begin{filecontents*}{"++bibFname++".bib}\n") :+:
  mkBibRef sm bib :+:
  S "\n\\end{filecontents*}\n" :+:
  S bibLines

bibLines :: String
bibLines =
  "\\nocite{*}\n" ++
  "\\bibstyle{" ++ bibStyleT ++ "}\n" ++
  "\\printbibliography[heading=none]"

mkBibRef :: PrintingInformation -> BibRef -> Spec
mkBibRef sm = foldl1 (\x y -> x :+: S "\n\n" :+: y) . map (renderF sm)

renderF :: PrintingInformation -> Citation -> Spec
renderF sm (Cite cid refType fields) =
  S (showT refType) :+: S ("{" ++ cid ++ ",\n") :+:
  (foldl1 (:+:) . intersperse (S ",\n") . map (showBibTeX sm)) fields :+: S "}"

showT :: L.CitationKind -> String
showT L.Article       = "@article"
showT L.Book          = "@book"
showT L.Booklet       = "@booklet"
showT L.InBook        = "@inbook"
showT L.InCollection  = "@incollection"
showT L.InProceedings = "@inproceedings"
showT L.Manual        = "@manual"
showT L.MThesis       = "@mastersthesis"
showT L.Misc          = "@misc"
showT L.PhDThesis     = "@phdthesis"
showT L.Proceedings   = "@proceedings"
showT L.TechReport    = "@techreport"
showT L.Unpublished   = "@unpublished"

showBibTeX :: PrintingInformation -> CiteField -> Spec
showBibTeX  _ (Address      s) = showField "address" s
showBibTeX sm (Author       p) = showField "author" (rendPeople sm p)
showBibTeX  _ (BookTitle    b) = showField "booktitle" b
showBibTeX  _ (Chapter      c) = showField "chapter" (wrapS c)
showBibTeX  _ (Edition      e) = showField "edition" (wrapS e)
showBibTeX sm (Editor       e) = showField "editor" (rendPeople sm e)
showBibTeX  _ (Institution  i) = showField "institution" i
showBibTeX  _ (Journal      j) = showField "journal" j
showBibTeX  _ (Month        m) = S "month=" :+: bibTeXMonth m
showBibTeX  _ (Note         n) = showField "note" n
showBibTeX  _ (Number       n) = showField "number" (wrapS n)
showBibTeX  _ (Organization o) = showField "organization" o
showBibTeX  _ (Pages        p) = showField "pages" (pages p)
showBibTeX  _ (Publisher    p) = showField "publisher" p
showBibTeX  _ (School       s) = showField "school" s
showBibTeX  _ (Series       s) = showField "series" s
showBibTeX  _ (Title        t) = showField "title" t
showBibTeX  _ (Type         t) = showField "type" t
showBibTeX  _ (Volume       v) = showField "volume" (wrapS v)
showBibTeX  _ (Year         y) = showField "year" (wrapS y)
showBibTeX  _ (HowPublished (URL  u)) =
  showField "howpublished" (S "\\url{" :+: u :+: S "}")
showBibTeX  _ (HowPublished (Verb v)) = showField "howpublished" v

--showBibTeX sm (Author p@(Person {_convention=Mono}:_)) = showField "author"
  -- (LS.spec sm (rendPeople p)) :+: S ",\n" :+:
  -- showField "sortkey" (LS.spec sm (rendPeople p))
-- showBibTeX sm (Author    p) = showField "author" $ LS.spec sm (rendPeople p)

showField :: String -> Spec -> Spec
showField f s = S f :+: S "={" :+: s :+: S "}"

rendPeople :: PrintingInformation -> L.People -> Spec
rendPeople _ []  = S "N.a." -- "No authors given"
rendPeople sm people = I.spec sm $
  foldl1 (\x y -> x L.+:+ L.S "and" L.+:+ y) $ map (L.S . L.rendPersLFM) people

bibTeXMonth :: L.Month -> Spec
bibTeXMonth L.Jan = S "jan"
bibTeXMonth L.Feb = S "feb"
bibTeXMonth L.Mar = S "mar"
bibTeXMonth L.Apr = S "apr"
bibTeXMonth L.May = S "may"
bibTeXMonth L.Jun = S "jun"
bibTeXMonth L.Jul = S "jul"
bibTeXMonth L.Aug = S "aug"
bibTeXMonth L.Sep = S "sep"
bibTeXMonth L.Oct = S "oct"
bibTeXMonth L.Nov = S "nov"
bibTeXMonth L.Dec = S "dec"

wrapS :: Show a => a -> Spec
wrapS = S . show

pages :: [Int] -> Spec
pages []  = error "Empty list of pages"
pages [x] = wrapS x
pages [x,x2] = wrapS $ show x ++ "-" ++ show x2
pages xs = error $ "Too many pages given in reference. Received: " ++ show xs
