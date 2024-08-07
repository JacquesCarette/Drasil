-- | Defines main LaTeX printer functions. For more information on each of the helper functions, please view the [source files](https://jacquescarette.github.io/Drasil/docs/full/drasil-printers-0.1.10.0/src/Language.Drasil.TeX.Print.html).
module Language.Drasil.TeX.Print(genTeX, pExpr, pUnit, spec, fence, OpenClose(..),
  pMatrix, cases) where

import Prelude hiding (print)
import Data.Bifunctor (bimap)
import Data.List (transpose, partition)
import Text.PrettyPrint (integer, text, (<+>))
import qualified Text.PrettyPrint as TP
import Numeric (showEFloat)
import Control.Arrow (second)

import qualified Language.Drasil as L
import qualified Language.Drasil.Display as LD

import Language.Drasil.Config (colAwidth, colBwidth, bibStyleT, bibFname)
import Language.Drasil.Printing.AST (Spec, ItemType(Nested, Flat), 
  ListType(Ordered, Unordered, Desc, Definitions, Simple), 
  Spec(Quote, EmptyS, Ref, S, Sp, HARDNL, E, (:+:)), 
  Fence(Norm, Abs, Curly, Paren), Expr, 
  Ops(..), Spacing(Thin), Fonts(Emph, Bold), 
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
  cite, command, command0, commandD, command2D, description, description', document, 
  empty, enumerate, externalref, figure, fraction, includegraphics, item, item',
  itemize, label, maketitle, maketoc, mathbb, mkEnv, mkEnvArgBr, mkEnvArgSq,
  mkMinipage, newline, newpage, parens, quote, sec, snref, sq, superscript,
  symbDescription, texSym, title, toEqn)
import Language.Drasil.TeX.Monad (D, MathContext(Curr, Math, Text), (%%), ($+$),
  hpunctuate, lub, runPrint, switch, toMath, toText, unPL, vcat, vpunctuate)
import Language.Drasil.TeX.Preamble (genPreamble)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)
import Data.Foldable (foldl')

-- | Generates a LaTeX document.
genTeX :: L.Document -> PrintingInformation -> TP.Doc
genTeX doc@(L.Document _ _ toC _) sm = 
  runPrint (buildStd sm toC $ I.makeDocument sm $ L.checkToC doc) Text
genTeX L.Notebook{} _ = TP.empty

-- | Helper to build the document.
buildStd :: PrintingInformation -> L.ShowTableOfContents -> Document -> D
buildStd sm toC (Document t a c) =
  genPreamble c %%
  title (spec t) %%
  author (spec a) %%
  case toC of 
    L.ToC -> document (maketitle %% maketoc %% newpage %% print sm c) -- includes ToC generation
    _ -> document (maketitle %% newpage %% print sm c) -- omits ToC generation

-- clean until here; lo needs its sub-functions fixed first though
-- | Helper for converting layout objects into a more printable form.
lo :: LayoutObj -> PrintingInformation -> D
lo (Header d t l)         _ = sec d (spec t) %% label (spec l)
lo (HDiv _ con _)        sm = print sm con -- FIXME ignoring 2 arguments?
lo (Paragraph contents)   _ = toText $ newline (spec contents)
lo (EqnBlock contents)    _ = makeEquation contents 1
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
lo (Cell _) _               = empty
lo (CodeBlock _) _          = empty

-- | Helper for converting layout objects into a more printable form.
-- This function is specific to definitions.
lo' :: LayoutObj -> PrintingInformation -> D
lo' (HDiv _ con _)      sm = printDef sm con
lo' (EqnBlock contents) _  = makeEquation contents 0.8
lo' obj                 sm = lo obj sm

-- | Converts layout objects into a document form.
print :: PrintingInformation -> [LayoutObj] -> D
print sm = foldr (($+$) . (`lo` sm)) empty

-- | Converts layout objects into a document form.
-- Specific to Definitions.
printDef :: PrintingInformation -> [LayoutObj] -> D
printDef sm = foldr (($+$) . (`lo'` sm)) empty

-- | Determine wether braces and brackets are opening or closing.
data OpenClose = Open | Close

-----------------------------------------------------------------
------------------ EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- (Since this is all implicitly in Math, leave it as String for now)

-- | Escape all special TeX characters.
-- TODO: This function should be improved. It should escape all special
--       TeX symbols that would affect rendering. For example, `_`
--       turns the RHS of text into subscript, and `^` would turn it
--       into superscript. This will need to be much more comprehensive.
--       e.g., `%`, `&`, `#`, etc
escapeIdentSymbols :: String -> String
escapeIdentSymbols ('_':ss) = '\\' : '_' : escapeIdentSymbols ss
escapeIdentSymbols (s:ss) = s : escapeIdentSymbols ss
escapeIdentSymbols [] = []

-- | Print an expression to a document.
pExpr :: Expr -> D
pExpr (Dbl d)        = pure . text $ showEFloat Nothing d ""
pExpr (Int i)        = pure (integer i)
pExpr (Str s)        = toText . quote . pure $ text s
pExpr (Div n d)      = command2D "frac" (pExpr n) (pExpr d)
pExpr (Case ps)      = mkEnv "cases" ($+$) (cases ps vpunctuate dbs pExpr)
pExpr (Mtx a)        = mkEnv "bmatrix" ($+$) (pMatrix a vpunctuate dbs pExpr)
pExpr (Row [x])      = br $ pExpr x -- FIXME: Hack needed for symbols with multiple subscripts, etc.
pExpr (Row l)        = foldl1 (<>) (map pExpr l)
pExpr (Ident s@[_])  = pure . text . escapeIdentSymbols $ s
pExpr (Ident s)      = commandD "mathit" (pure . text . escapeIdentSymbols $ s)
pExpr (Label s)      = command "text" s
pExpr (Spec s)       = pure . text $ unPL $ L.special s
--pExpr (Gr g)         = unPL $ greek g
pExpr (Sub e)        = pure unders <> br (pExpr e)
pExpr (Sup e)        = pure hat    <> br (pExpr e)
pExpr (Over Hat s)   = commandD "hat" (pExpr s)
pExpr (MO o)         = pOps o
pExpr (Fenced l r m) = fence Open l <> pExpr m <> fence Close r
pExpr (Font Bold e)  = commandD "symbf" (pExpr e)
pExpr (Font Emph e)  = pExpr e -- Emph is ignored here because we're in Math mode
pExpr (Spc Thin)     = pure . text $ "\\,"
pExpr (Sqrt e)       = commandD "sqrt" (pExpr e)

-- | Prints operators.
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
pOps VAdd     = pure pls
pOps VSub     = pure hyph -- unfortunately, hyphen and - are the same
pOps Dot      = commandD "cdot" empty
pOps Scale    = pure $ text " "
pOps Eq       = pure assign
pOps NEq      = commandD "neq" empty
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
pOps Mul      = pure $ text "\\,"
pOps Summ     = command0 "displaystyle" <> command0 "sum"
pOps Prod     = command0 "displaystyle" <> command0 "prod"
pOps Inte     = texSym "int"
pOps Point    = pure $ text "."
pOps Perc     = texSym "%"
pOps LArrow   = commandD "leftarrow"  empty
pOps RArrow   = commandD "rightarrow" empty
pOps ForAll   = commandD "ForAll"     empty
pOps Partial  = commandD "partial"    empty

-- | Prints fencing notation ("(),{},|,||").
fence :: OpenClose -> Fence -> D
fence Open Paren  = texSym "left("
fence Close Paren = texSym "right)"
fence Open Curly  = texSym "{"
fence Close Curly = texSym "}"
fence _ Abs       = pure $ text "|"
fence _ Norm      = pure $ text "\\|"

-- | For printing a Matrix.
pMatrix :: [[Expr]] -> (TP.Doc -> [D] -> D) -> TP.Doc -> (Expr -> D) -> D
pMatrix e catf esc f = catf esc (map pIn e)
  where pIn x = hpunctuate (text " & ") (map f x)

-- | Helper for printing case expression.
cases :: [(Expr,Expr)] -> (TP.Doc -> [D] -> D) -> TP.Doc -> (Expr -> D) -> D
cases [] _ _ _ = error "Attempt to create case expression without cases"
cases e catf esc f = catf esc (map _case e)
  where _case (x, y) = hpunctuate (text ", & ") (map f [x, y])

-----------------------------------------------------------------
------------------ TABLE PRINTING---------------------------
-----------------------------------------------------------------

-- | Prints a table. Takes in data for the table, a label,
-- a boolean that determines if the caption is shown, and a caption.
makeTable :: [[Spec]] -> D -> Bool -> D -> D
makeTable [] _ _ _ = error "Completely empty table (not even header)"
makeTable [_] _ _ _ = empty -- table with no actual contents... don't error
makeTable lls@(h:tlines) r bool t = mkEnv "longtblr" ($+$) $
  (if bool then sq $ pure (text "caption=") <> br t else empty)
  %% br (pure (text "colspec=") <> br (pure $ text $ unwords $ anyBig lls)
    <> pure (text ", rowhead=1, hline{1,Z}=\\heavyrulewidth, hline{2}=\\lightrulewidth"))
  %% makeHeaders h
  %% makeRows tlines
  %% label r
  where
    descr True  = "X[l]"
    descr False = "l"
    --returns "X[l]" for columns with long fields
    anyBig = map (descr . longColumn) . transpose
    longColumn = any (\x -> specLength x > 50)

-- | Determines the length of a 'Spec'.
specLength :: Spec -> Int
specLength (E x)       = length $ filter (`notElem` dontCount) $ TP.render $ runPrint (pExpr x) Curr
specLength (S x)       = length x
specLength (a :+: b)   = specLength a + specLength b
specLength (Sp _)      = 1
specLength (Ref Internal r _) = length r
specLength (Ref (Cite2 n)   r i ) = length r + specLength i + specLength n --may need to change?
specLength (Ref External _ t) = specLength t
specLength EmptyS      = 0
specLength (Quote q)   = 4 + specLength q
specLength HARDNL      = 0

-- | Invalid characters, not included in an expression.
dontCount :: String
dontCount = "\\/[]{}()_^$:"

-- | Creates the header for a table.
makeHeaders :: [Spec] -> D
makeHeaders ls = hpunctuate (text " & ") (map (bold . spec) ls) %% pure dbs

-- | Create rows for a table with a single line break between them.
makeRows :: [[Spec]] -> D
makeRows [] = mempty
makeRows lls = foldr1 ((%%) . (%% pure dbs)) $ map makeColumns lls

-- | Creates the columns for a table.
makeColumns :: [Spec] -> D
makeColumns ls = hpunctuate (text " & ") $ map spec ls

------------------ Spec -----------------------------------

-- | Helper that determines the printing context based on the kind of 'Spec'.
needs :: Spec -> MathContext
needs (a :+: b) = needs a `lub` needs b
needs (S _)     = Text
needs (E _)     = Math
needs (Sp _)    = Math
needs HARDNL    = Text
needs Ref{}     = Text
needs EmptyS    = Text
needs (Quote _) = Text

-- | Prints all 'Spec's.
spec :: Spec -> D
spec a@(s :+: t) = s' <> t'
  where
    ctx = const $ needs a
    s' = switch ctx $ spec s
    t' = switch ctx $ spec t
spec (E ex) = toMath $ pExpr ex
spec (S s)  = either error (pure . text . concatMap escapeChars) $ L.checkValidStr s invalid
  where
    invalid = ['&', '#', '$', '%', '&', '~', '^', '\\', '{', '}']
    escapeChars '_' = "\\_"
    escapeChars '&' = "\\&"
    escapeChars c = [c]
spec (Sp s) = pure $ text $ unPL $ L.special s
spec HARDNL = command0 "newline"
spec (Ref Internal r sn) = snref r (spec sn)
spec (Ref (Cite2 n) r _) = cite r (info n)
  where
    info EmptyS = Nothing
    info x      = Just (spec x)
spec (Ref External r sn) = externalref r (spec sn)
spec EmptyS              = empty
spec (Quote q)           = quote $ spec q

-- | Determines the needed context of a symbol.
symbolNeeds :: LD.Symbol -> MathContext
symbolNeeds (LD.Variable   _) = Text
symbolNeeds (LD.Label      _) = Text
symbolNeeds (LD.Integ      _) = Math
symbolNeeds (LD.Special    _) = Math
symbolNeeds (LD.Concat    []) = Math
symbolNeeds (LD.Concat (s:_)) = symbolNeeds s
symbolNeeds LD.Corners{}      = Math
symbolNeeds (LD.Atop     _ _) = Math
symbolNeeds LD.Empty          = Curr

-- | Prints units.
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
    p_symb (LD.Concat s) = foldl' (<>) empty $ map p_symb s
    p_symb n = let cn = symbolNeeds n in switch (const cn) $ pExpr $ I.symbol n

-----------------------------------------------------------------
------------------ DATA DEFINITION PRINTING-----------------
-----------------------------------------------------------------

-- | Prints a (data) definition.
makeDefn :: PrintingInformation -> [(String,[LayoutObj])] -> D -> D
makeDefn _  [] _ = error "Empty definition"
makeDefn sm ps l = mkMinipage (makeDefTable sm ps l)

-- | Helper that creates the definition and associated table.
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

-- | Helper that makes the rows of a definition table.
makeDRows :: PrintingInformation -> [(String,[LayoutObj])] -> D
makeDRows _  []      = error "No fields to create Defn table"
makeDRows sm ls      = foldl1 (%%) $ map (\(f, d) -> 
  pure (dbs <+> text "\\midrule") %% 
  pure (text (f ++ " & ")) <> printDef sm d) ls

-----------------------------------------------------------------
------------------ EQUATION PRINTING------------------------
-----------------------------------------------------------------

-- | Prints an equation with a max width of scale * page width.
makeEquation :: Spec -> Double -> D
makeEquation contents scale = toEqn scale (spec contents)

  --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  --  on chunk (i.e. "eq:h_g" for h_g = ...

-----------------------------------------------------------------
------------------ LIST PRINTING----------------------------
-----------------------------------------------------------------

-- latex doesn't like empty lists, so don't put anything out for them.
-- empty lists here isn't quite wrong (though there should probably be
-- a warning higher up), so don't generate bad latex.
-- | Prints a list. LaTeX doesn't like empty lists, so those are rendered as 'empty'.
makeList :: ListType -> D
makeList (Simple []   )      = empty
makeList (Desc []   )        = empty
makeList (Unordered []   )   = empty
makeList (Ordered []   )     = empty
makeList (Definitions []   ) = empty
makeList (Simple items)      = description' $ vcat $ simItem items
makeList (Desc items)        = description  $ vcat $ simItem items
makeList (Unordered items)   = itemize      $ vcat $ map plItem items
makeList (Ordered items)     = enumerate    $ vcat $ map plItem items
makeList (Definitions items) = symbDescription $ vcat $ defItem items

-- | Helper that renders items in 'makeList'.
plItem :: (ItemType,Maybe Label) -> D
plItem (i, l) = mlref l <> pItem i

-- | Helper that renders the 'Spec' part of labels in 'mlref'.
lspec :: Spec -> D  -- FIXME: Should be option rolled in to spec
lspec (S s) = pure $ text s
lspec r = spec r

-- | Helper that renders labels in 'plItem'. 
mlref :: Maybe Label -> D
mlref = maybe empty $ (<>) (command0 "phantomsection") . label . lspec

-- | Helper that renders items in 'plItem'.
pItem :: ItemType -> D
pItem (Flat s) = item $ spec s
pItem (Nested t s) = vcat [item $ spec t, makeList s]

-- | Helper that renders simple and descriptive items in 'makeList'.
simItem :: [(Spec,ItemType,Maybe Label)] -> [D]
simItem = map (\(x,y,l) -> item' (spec (x :+: S ":") <> mlref l) $ sp_item y)
  where sp_item (Flat s) = spec s
        sp_item (Nested t s) = vcat [spec t, makeList s]

-- | Helper that renders definitions in 'makeList'.
defItem :: [(Spec, ItemType,Maybe Label)] -> [D]
defItem = map (\(x,y,l) -> item $ mlref l <> spec (x :+: S " is the " :+: d_item y))
  where d_item (Flat s) = s
        d_item (Nested _ _) = error "Cannot use sublists in definitions"
-----------------------------------------------------------------
------------------ FIGURE PRINTING--------------------------
-----------------------------------------------------------------

-- | Prints figures. Takes in a label and caption along with information for 'includegraphics'.
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

-- | Prints graphs.
makeGraph :: [(D,D)] -> D -> D -> D -> D -> D
makeGraph ps w h c l =
  mkEnv "figure" ($+$) $ centering %%
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
-- | Prints a bibliography.
makeBib :: PrintingInformation -> BibRef -> D
makeBib sm bib = mkEnvArgBr "filecontents*" (bibFname ++ ".bib") (mkBibRef sm bib) %%
  command "nocite" "*" %% command "bibstyle" bibStyleT %%
  command0 "printbibliography" <> sq (pure $ text "heading=none")

-- | Renders a bibliographical reference with a single line break between
-- entries.
mkBibRef :: PrintingInformation -> BibRef -> D
mkBibRef sm = foldr ((%%) . renderF sm) mempty

-- | Helper that renders a citation.
renderF :: PrintingInformation -> Citation -> D
renderF sm (Cite cid refType fields) = pure (text (showT refType)) <>
  br (hpunctuate (text ",\n") $ pure (text cid) : map (showBibTeX sm) fields)

-- | Renders different kinds of citation mediums.
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

-- | Renders different citation fields.
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
showBibTeX sm (Pages        p) = showField "pages" (I.spec sm $ L.foldNums "--" p)
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

-- | Citation fields may be wrapped with braces, nothing, or a command.
data FieldWrap = Braces | NoDelimiters | Command String

-- | Helper that renders citation fields with a wrapper.
wrapField :: FieldWrap -> String -> Spec -> D
wrapField fw f s = pure (text (f ++ "=")) <> resolve fw (spec s)
  where
    resolve Braces       = br
    resolve NoDelimiters = id
    resolve (Command st) = br . commandD st

showField, showFieldRaw :: String -> Spec -> D
-- | Helper that renders citation fields wrapped with braces.
showField    = wrapField Braces
-- | Helper that renders citation fields with no delimiters.
showFieldRaw = wrapField NoDelimiters

-- | Helper that renders citation fields with a command.
showFieldCom   :: String -> String -> Spec -> D
showFieldCom s = wrapField (Command s)

-- | Helper that renders people for citations.
rendPeople :: PrintingInformation -> L.People -> Spec
rendPeople _ []  = S "N.a." -- "No authors given"
rendPeople sm people = I.spec sm $
  foldl1 (\x y -> x L.+:+ L.S "and" L.+:+ y) $ map (L.S . L.rendPersLFM) people

-- | Helper that renders months for citations.
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

-- | Helper that lifts something showable into a 'Spec'.
wrapS :: Show a => a -> Spec
wrapS = S . show
