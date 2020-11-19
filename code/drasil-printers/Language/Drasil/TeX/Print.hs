module Language.Drasil.TeX.Print(genTeX, pExpr, pUnit, spec) where

import Prelude hiding (print)
import Data.Bifunctor (bimap)
import Data.List (transpose, partition)
import Text.PrettyPrint (integer, text, (<+>))
import qualified Text.PrettyPrint as TP
import Numeric (showEFloat)
import Control.Applicative (pure)
import Control.Arrow (second)

import qualified Language.Drasil as L (CitationKind(..), Decoration(Prime, Hat, Vector),
  Document, MaxWidthPercent, Month(..), People, RenderSpecial(..), Sentence(S),
  Symbol(..), USymb(US), (+:+), rendPersLFM, special)
import Utils.Drasil (checkValidStr, foldNums)

import Language.Drasil.Config (colAwidth, colBwidth, bibStyleT, bibFname)
import Language.Drasil.Printing.AST (Spec, ItemType(Nested, Flat), 
  ListType(Ordered, Unordered, Desc, Definitions, Simple), 
  Spec(Quote, EmptyS, Ref, S, Sp, HARDNL, E, (:+:)), 
  Fence(Norm, Abs, Curly, Paren), Expr, 
  Ops(Inte, Prod, Summ, Mul, Add, Or, And, Subt, Iff, LEq, GEq, 
  NEq, Eq, Gt, Lt, Impl, Dot, Cross, Neg, Exp, Dim, Not, Arctan, Arccos, Arcsin,
  Cot, Csc, Sec, Tan, Cos, Sin, Log, Ln, Prime, Comma, Boolean, Real, Natural, 
  Rational, Integer, IsIn, Point, Perc), Spacing(Thin), Fonts(Emph, Bold), 
  Expr(..), OverSymb(Hat), Label,
  LinkType(Internal, Cite2, External))
import Language.Drasil.Printing.Citation (HP(Verb, URL), CiteField(HowPublished, 
  Year, Volume, Type, Title, Series, School, Publisher, Organization, Pages,
  Month, Number, Note, Journal, Editor, Chapter, Institution, Edition, BookTitle,
  Author, Address), Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import qualified Language.Drasil.Printing.Import as I
import Language.Drasil.Printing.Helpers hiding (br, paren, sq, sqbrac)
import Language.Drasil.TeX.Helpers (author, bold, br, caption, center, centering,
  cite, command, command0, commandD, command2D, description, document, empty,
  enumerate, externalref, figure, fraction, includegraphics, item, item',
  itemize, label, maketitle, maketoc, mathbb, mkEnv, mkEnvArgBr, mkEnvArgSq,
  mkMinipage, newline, newpage, parens, quote, sec, snref, sq, superscript,
  symbDescription, texSym, title, toEqn)
import Language.Drasil.TeX.Monad (D, MathContext(Curr, Math, Text), (%%), ($+$),
  hpunctuate, lub, runPrint, switch, toMath, toText, unPL, vcat, vpunctuate)
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
lo (Header d t l)         _ = sec d (spec t) %% label (spec l)
lo (HDiv _ con _)        sm = print sm con -- FIXME ignoring 2 arguments?
lo (Paragraph contents)   _ = toText $ newline (spec contents)
lo (EqnBlock contents)    _ = makeEquation contents
lo (Table _ rows r bl t)  _ = toText $ makeTable rows (spec r) bl (spec t)
lo (Definition _ ssPs l) sm = toText $ makeDefn sm ssPs $ spec l
lo (List l)               _ = toText $ makeList l
lo (Figure r c f wp)      _ = toText $ makeFigure (spec r) (spec c) f wp
lo (Bib bib)             sm = toText $ makeBib sm bib
lo (Graph ps w h c l)    _  = toText $ makeGraph
  (map (bimap spec spec) ps)
  (pure $ text $ maybe "" (\x -> "text width = " ++ show x ++ "em ,") w)
  (pure $ text $ maybe "" (\x -> "minimum height = " ++ show x ++ "em, ") h)
  (spec c) (spec l)

print :: PrintingInformation -> [LayoutObj] -> D
print sm = foldr (($+$) . (`lo` sm)) empty

------------------ Symbol ----------------------------
symbol :: L.Symbol -> D
symbol (L.Variable s) = pure $ text s
symbol (L.Label    s) = pure $ text s
symbol (L.Integ    n) = pure $ text $ show n
symbol (L.Special  s) = pure $ text $ unPL $ L.special s
symbol (L.Concat  sl) = foldl (<>) empty $ map symbol sl
--
-- handle the special cases first, then general case
symbol (L.Corners [] [] [x] [] s) = br $ symbol s <> pure hat <> br (symbol x)
symbol (L.Corners [] [] [] [x] s) = br $ symbol s <> pure unders <> br (symbol x)
symbol (L.Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (L.Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol L.Corners{}                = error "rendering of Corners (general)"
symbol (L.Atop f s)               = sFormat f s
symbol L.Empty                    = empty

sFormat :: L.Decoration -> L.Symbol -> D
sFormat L.Hat    s = commandD "hat" (symbol s)
sFormat L.Vector s = commandD "mathbf" (symbol s)
sFormat L.Prime  s = symbol s <> pure (text "'")

data OpenClose = Open | Close

-----------------------------------------------------------------
------------------ EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- (Since this is all implicitly in Math, leave it as String for now)
pExpr :: Expr -> D
pExpr (Dbl d)        = pure . text $ showEFloat Nothing d ""
pExpr (Int i)        = pure (integer i)
pExpr (Str s)        = toText . quote . pure $ text s
pExpr (Div n d)      = command2D "frac" (pExpr n) (pExpr d)
pExpr (Case ps)      = mkEnv "cases" (cases ps)
pExpr (Mtx a)        = mkEnv "bmatrix" (pMatrix a)
pExpr (Row [x])      = br $ pExpr x -- FIXME: Hack needed for symbols with multiple subscripts, etc.
pExpr (Row l)        = foldl1 (<>) (map pExpr l)
pExpr (Ident s)      = pure . text $ s
pExpr (Label s)      = command "text" s
pExpr (Spec s)       = pure . text $ unPL $ L.special s
--pExpr (Gr g)         = unPL $ greek g
pExpr (Sub e)        = pure unders <> br (pExpr e)
pExpr (Sup e)        = pure hat    <> br (pExpr e)
pExpr (Over Hat s)   = commandD "hat" (pExpr s)
pExpr (MO o)         = pOps o
pExpr (Fenced l r m) = fence Open l <> pExpr m <> fence Close r
pExpr (Font Bold e)  = commandD "mathbf" (pExpr e)
pExpr (Font Emph e)  = pExpr e -- Emph is ignored here because we're in Math mode
pExpr (Spc Thin)     = pure . text $ "\\,"
pExpr (Sqrt e)       = commandD "sqrt" (pExpr e)

pOps :: Ops -> D
pOps IsIn     = commandD "in" empty
pOps Integer  = mathbb "Z"
pOps Rational = mathbb "Q"
pOps Real     = mathbb "R"
pOps Natural  = mathbb "N"
pOps Boolean  = mathbb "B"
pOps Comma    = pure $ text ","
pOps Prime    = pure $ text "'"
pOps Log      = texSym "log"
pOps Ln       = texSym "ln"
pOps Sin      = texSym "sin"
pOps Cos      = texSym "cos"
pOps Tan      = texSym "tan"
pOps Sec      = texSym "sec"
pOps Csc      = texSym "csc"
pOps Cot      = texSym "cot"
pOps Arcsin   = texSym "arcsin"
pOps Arccos   = texSym "arccos"
pOps Arctan   = texSym "arctan"
pOps Not      = commandD "neg" empty
pOps Dim      = command "mathsf" "dim"
pOps Exp      = pure $ text "e"
pOps Neg      = pure hyph
pOps Cross    = texSym "times"
pOps Dot      = commandD "cdot" empty
pOps Eq       = pure assign
pOps NEq      = commandD "neq" empty
-- Old way of doing less than and greater than
-- pOps Lt       = pure lt
-- pOps Gt       = pure gt
pOps Lt       = commandD "lt" empty
pOps Gt       = commandD "gt" empty
pOps GEq      = commandD "geq" empty
pOps LEq      = commandD "leq" empty
pOps Impl     = commandD "implies" empty
pOps Iff      = commandD "iff" empty
pOps Subt     = pure hyph
pOps And      = commandD "land" empty
pOps Or       = commandD "lor" empty
pOps Add      = pure pls
pOps Mul      = pure $ text " "
pOps Summ     = command0 "displaystyle" <> command0 "sum"
pOps Prod     = command0 "displaystyle" <> command0 "prod"
pOps Inte     = texSym "int"
pOps Point    = pure $ text "."
pOps Perc     = texSym "%"

fence :: OpenClose -> Fence -> D
fence Open Paren  = texSym "left("
fence Close Paren = texSym "right)"
fence Open Curly  = texSym "{"
fence Close Curly = texSym "}"
fence _ Abs       = pure $ text "|"
fence _ Norm      = pure $ text "||"

-- | For printing Matrix
pMatrix :: [[Expr]] -> D
pMatrix e = vpunctuate dbs (map pIn e)
  where pIn x = hpunctuate (text " & ") (map pExpr x)

cases :: [(Expr,Expr)] -> D
cases [] = error "Attempt to create case expression without cases"
cases e  = vpunctuate dbs (map _case e)
  where _case (x, y) = hpunctuate (text ", & ") (map pExpr [x, y])

-----------------------------------------------------------------
------------------ TABLE PRINTING---------------------------
-----------------------------------------------------------------

makeTable :: [[Spec]] -> D -> Bool -> D -> D
makeTable [] _ _ _ = error "Completely empty table (not even header)"
makeTable [_] _ _ _ = empty -- table with no actual contents... don't error
makeTable lls@(h:tlines) r bool t = mkEnvArgBr ltab (unwords $ anyBig lls) $
  command0 "toprule"
  %% makeHeaders h
  %% command0 "midrule"
  %% command0 "endhead"
  %% makeRows tlines
  %% command0 "bottomrule"
  %% (if bool then caption t else caption empty)
  %% label r
  where
    --Only needed if "X[l]" is used
    ltab = if anyLong lls then "longtabu" else "longtable"
    descr True  = "X[l]"
    descr False = "l"
    --returns "X[l]" for columns with long fields
    anyLong = any longColumn . transpose
    anyBig = map (descr . longColumn) . transpose
    longColumn = any (\x -> specLength x > 50)

-- | determines the length of a Spec
specLength :: Spec -> Int
specLength (E x)       = length $ filter (`notElem` dontCount) $ TP.render $ runPrint (pExpr x) Curr
specLength (S x)       = length x
specLength (a :+: b)   = specLength a + specLength b
specLength (Sp _)      = 1
specLength (Ref Internal r _) = length r
specLength (Ref Cite2    r i) = length r + specLength i
specLength (Ref External _ t) = specLength t
specLength EmptyS      = 0
specLength (Quote q)   = 4 + specLength q
specLength HARDNL      = 0

dontCount :: String
dontCount = "\\/[]{}()_^$:"

makeHeaders :: [Spec] -> D
makeHeaders ls = hpunctuate (text " & ") (map (bold . spec) ls) %% pure dbs

makeRows :: [[Spec]] -> D
makeRows = mconcat . map (\c -> makeColumns c %% pure dbs)

makeColumns :: [Spec] -> D
makeColumns ls = hpunctuate (text " & ") $ map spec ls

------------------ Spec -----------------------------------

needs :: Spec -> MathContext
needs (a :+: b) = needs a `lub` needs b
needs (S _)     = Text
needs (E _)     = Math
needs (Sp _)    = Math
needs HARDNL    = Text
needs Ref{}     = Text
needs EmptyS    = Text
needs (Quote _) = Text

-- print all Spec through here
spec :: Spec -> D
spec a@(s :+: t) = s' <> t'
  where
    ctx = const $ needs a
    s' = switch ctx $ spec s
    t' = switch ctx $ spec t
spec (E ex) = toMath $ pExpr ex
spec (S s)  = either error (pure . text . concatMap escapeChars) $ checkValidStr s invalid
  where
    invalid = ['&', '#', '$', '%', '&', '~', '^', '\\', '{', '}']
    escapeChars '_' = "\\_"
    escapeChars '&' = "\\&"
    escapeChars c = [c]
spec (Sp s) = pure $ text $ unPL $ L.special s
spec HARDNL = command0 "newline"
spec (Ref Internal r sn) = snref r (spec sn)
spec (Ref Cite2    r i)  = cite r (info i)
  where
    info EmptyS = Nothing
    info x      = Just (spec x)
spec (Ref External r sn) = externalref r (spec sn)
spec EmptyS              = empty
spec (Quote q)           = quote $ spec q

symbolNeeds :: L.Symbol -> MathContext
symbolNeeds (L.Variable   _) = Text
symbolNeeds (L.Label      _) = Text
symbolNeeds (L.Integ      _) = Math
symbolNeeds (L.Special    _) = Math
symbolNeeds (L.Concat    []) = Math
symbolNeeds (L.Concat (s:_)) = symbolNeeds s
symbolNeeds L.Corners{}      = Math
symbolNeeds (L.Atop     _ _) = Math
symbolNeeds L.Empty          = Curr

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
    p_symb n = let cn = symbolNeeds n in switch (const cn) $ symbol n

-----------------------------------------------------------------
------------------ DATA DEFINITION PRINTING-----------------
-----------------------------------------------------------------

makeDefn :: PrintingInformation -> [(String,[LayoutObj])] -> D -> D
makeDefn _  [] _ = error "Empty definition"
makeDefn sm ps l = mkMinipage (makeDefTable sm ps l)

makeDefTable :: PrintingInformation -> [(String,[LayoutObj])] -> D -> D
makeDefTable _ [] _ = error "Trying to make empty Data Defn"
makeDefTable sm ps l = mkEnvArgBr "tabular" (col rr colAwidth ++ col (rr ++ "\\arraybackslash") colBwidth) $ vcat [
  command0 "toprule " <> bold (pure $ text "Refname") <> pure (text " & ") <> bold l, --shortname instead of refname?
  command0 "phantomsection ", label l,
  makeDRows sm ps,
  pure $ dbs <+> text "\\bottomrule"
  ]
  where
    col s x = ">" ++ brace s ++ "p" ++ brace (show x ++ tw)
    rr = "\\raggedright"
    tw = "\\textwidth"

makeDRows :: PrintingInformation -> [(String,[LayoutObj])] -> D
makeDRows _  []         = error "No fields to create Defn table"
makeDRows sm ls    = foldl1 (%%) $ map (\(f, d) -> dBoilerplate %%  pure (text (f ++ " & ")) <> print sm d) ls
  where dBoilerplate = pure $ dbs <+> text "\\midrule" <+> dbs

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

-- latex doesn't like empty lists, so don't put anything out for them.
-- empty lists here isn't quite wrong (though there should probably be
-- a warning higher up), so don't generate bad latex.
makeList :: ListType -> D
makeList (Simple []   )      = empty
makeList (Desc []   )        = empty
makeList (Unordered []   )   = empty
makeList (Ordered []   )     = empty
makeList (Definitions []   ) = empty
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
mlref = maybe empty $ (<>) (command0 "phantomsection") . label . lspec

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
------------------ MODULE PRINTING----------------------------
-----------------------------------------------------------------

makeGraph :: [(D,D)] -> D -> D -> D -> D -> D
makeGraph ps w h c l =
  mkEnv "figure" $ centering %%
  mkEnvArgBr "adjustbox" "max width=\\textwidth" (
  mkEnvArgSq "tikzpicture" ">=latex,line join=bevel" (
  vcat [command "tikzstyle" "n" <> pure (text " = ") <> sq (
          pure (text "draw, shape=rectangle, ") <> w <> h <>
          pure (text "font=\\Large, align=center]")),
        mkEnvArgSq "dot2tex" "dot, codeonly, options=-t raw" (
        pure (text "digraph G ") <> br ( vcat (
         pure (text "graph [sep = 0. esep = 0, nodesep = 0.1, ranksep = 2];") :
         pure (text "node [style = \"n\"];") :
         map (\(a,b) -> q a <> pure (text " -> ") <> q b <> pure (text ";")) ps)
        ))
       ])) %% caption c %% label l
  where q x = pure (text "\"") <> x <> pure (text "\"")

---------------------------
-- Bibliography Printing --
---------------------------
-- **THE MAIN FUNCTION** --
makeBib :: PrintingInformation -> BibRef -> D
makeBib sm bib = mkEnvArgBr "filecontents*" (bibFname ++ ".bib") (mkBibRef sm bib) %%
  command "nocite" "*" %% command "bibstyle" bibStyleT %%
  command0 "printbibliography" <> sq (pure $ text "heading=none")

mkBibRef :: PrintingInformation -> BibRef -> D
mkBibRef sm = mconcat . map (renderF sm)

renderF :: PrintingInformation -> Citation -> D
renderF sm (Cite cid refType fields) = pure (text (showT refType)) <>
  br (hpunctuate (text ",\n") $ pure (text cid) : map (showBibTeX sm) fields)

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

showBibTeX :: PrintingInformation -> CiteField -> D
showBibTeX  _ (Address      s) = showField "address" s
showBibTeX sm (Author       p) = showField "author" (rendPeople sm p)
showBibTeX  _ (BookTitle    b) = showField "booktitle" b
showBibTeX  _ (Chapter      c) = showField "chapter" (wrapS c)
showBibTeX  _ (Edition      e) = showField "edition" (wrapS e)
showBibTeX sm (Editor       e) = showField "editor" (rendPeople sm e)
showBibTeX  _ (Institution  i) = showField "institution" i
showBibTeX  _ (Journal      j) = showField "journal" j
showBibTeX  _ (Month        m) = showFieldRaw "month" (bibTeXMonth m)
showBibTeX  _ (Note         n) = showField "note" n
showBibTeX  _ (Number       n) = showField "number" (wrapS n)
showBibTeX  _ (Organization o) = showField "organization" o
showBibTeX sm (Pages        p) = showField "pages" (I.spec sm $ foldNums "--" p)
showBibTeX  _ (Publisher    p) = showField "publisher" p
showBibTeX  _ (School       s) = showField "school" s
showBibTeX  _ (Series       s) = showField "series" s
showBibTeX  _ (Title        t) = showField "title" t
showBibTeX  _ (Type         t) = showField "type" t
showBibTeX  _ (Volume       v) = showField "volume" (wrapS v)
showBibTeX  _ (Year         y) = showField "year" (wrapS y)
showBibTeX  _ (HowPublished (URL  u)) = showFieldCom "url" "howpublished" u
showBibTeX  _ (HowPublished (Verb v)) = showField "howpublished" v

--showBibTeX sm (Author p@(Person {_convention=Mono}:_)) = showField "author"
  -- (LS.spec sm (rendPeople p)) :+: S ",\n" :+:
  -- showField "sortkey" (LS.spec sm (rendPeople p))
-- showBibTeX sm (Author    p) = showField "author" $ LS.spec sm (rendPeople p)

data FieldWrap = Braces | NoDelimiters | Command String

wrapField :: FieldWrap -> String -> Spec -> D
wrapField fw f s = pure (text (f ++ "=")) <> resolve fw (spec s)
  where
    resolve Braces       = br
    resolve NoDelimiters = id
    resolve (Command st) = br . commandD st

showField, showFieldRaw :: String -> Spec -> D
showField    = wrapField Braces
showFieldRaw = wrapField NoDelimiters

showFieldCom   :: String -> String -> Spec -> D
showFieldCom s = wrapField (Command s)

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
