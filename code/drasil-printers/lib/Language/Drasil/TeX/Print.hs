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
  Spec(Quote, EmptyS, Ref, S, Ch, Sp, HARDNL, E, (:+:)), 
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
import Language.Drasil.Printing.Import.Helpers (termStyleLookup)
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
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, ckdb)
import Data.Foldable (foldl')

import Control.Lens ((^.))

-- | Generates a LaTeX document.
genTeX :: L.Document -> PrintingInformation -> TP.Doc
genTeX doc@(L.Document _ _ toC _) sm = 
  runPrint (buildStd sm toC $ I.makeDocument sm $ L.checkToC doc) Text
genTeX L.Notebook{} _ = TP.empty

-- | Helper to build the document.
buildStd :: PrintingInformation -> L.ShowTableOfContents -> Document -> D
buildStd sm toC (Document t a c) =
  genPreamble c %%
  title (spec sm t) %%
  author (spec sm a) %%
  case toC of 
    L.ToC -> document (maketitle %% maketoc %% newpage %% print sm c) -- includes ToC generation
    _ -> document (maketitle %% newpage %% print sm c) -- omits ToC generation

-- clean until here; lo needs its sub-functions fixed first though
-- | Helper for converting layout objects into a more printable form.
lo :: LayoutObj -> PrintingInformation -> D
lo (Header d t l)        sm = sec d (spec sm t) %% label (spec sm l)
lo (HDiv _ con _)        sm = print sm con -- FIXME ignoring 2 arguments?
lo (Paragraph contents)  sm = toText $ newline (spec sm contents)
lo (EqnBlock contents)   sm = makeEquation sm contents
lo (Table _ rows r bl t) sm = toText $ makeTable sm rows (spec sm r) bl (spec sm t)
lo (Definition _ ssPs l) sm = toText $ makeDefn sm ssPs $ spec sm l
lo (List l)              sm = toText $ makeList sm l
lo (Figure r c f wp)     sm = toText $ makeFigure (spec sm r) (maybe empty (spec sm) c) f wp
lo (Bib bib)             sm = toText $ makeBib sm bib
lo (Graph ps w h c l)    sm = toText $ makeGraph
  (map (bimap (spec sm) (spec sm)) ps)
  (pure $ text $ maybe "" (\x -> "text width = " ++ show x ++ "em ,") w)
  (pure $ text $ maybe "" (\x -> "minimum height = " ++ show x ++ "em, ") h)
  (spec sm c) (spec sm l)
lo (Cell _) _               = empty
lo (CodeBlock _) _          = empty

-- | Converts layout objects into a document form.
print :: PrintingInformation -> [LayoutObj] -> D
print sm = foldr (($+$) . (`lo` sm)) empty

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
pExpr (Set l)        = foldl1 (<>) (map pExpr l)
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
pOps SAdd     = pure pls
pOps SRemove  = pure hyph
pOps SContains = commandD " in " empty
pOps SUnion   = commandD "+" empty
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
makeTable :: PrintingInformation -> [[Spec]] -> D -> Bool -> D -> D
makeTable _  [] _ _ _ = error "Completely empty table (not even header)"
makeTable _  [_] _ _ _ = empty -- table with no actual contents... don't error
makeTable sm lls@(h:tlines) r bool t = mkEnv "longtblr" ($+$) $
  (if bool then sq $ pure (text "caption=") <> br t else empty)
  %% br (pure (text "colspec=") <> br (pure $ text $ unwords $ anyBig lls)
    <> pure (text ", rowhead=1, hline{1,Z}=\\heavyrulewidth, hline{2}=\\lightrulewidth"))
  %% makeHeaders sm h
  %% makeRows sm tlines
  %% label r
  where
    descr True  = "X[l]"
    descr False = "l"
    --returns "X[l]" for columns with long fields
    anyBig = map (descr . longColumn) . transpose
    longColumn = any (\x -> specLength sm x > 50)

-- | Determines the length of a 'Spec'.
specLength :: PrintingInformation -> Spec -> Int
specLength _  (E x)       = length $ filter (`notElem` dontCount) $ TP.render $ runPrint (pExpr x) Curr
specLength _  (S x)       = length x
specLength sm (Ch st caps s) = specLength sm $ I.spec sm $ termStyleLookup st (sm ^. ckdb) s caps
specLength sm (a :+: b)   = specLength sm a + specLength sm b
specLength _  (Sp _)      = 1
specLength _  (Ref Internal r _) = length r
specLength sm (Ref (Cite2 n)   r i ) = length r + specLength sm i + specLength sm n --may need to change?
specLength sm (Ref External _ t) = specLength sm t
specLength _  EmptyS      = 0
specLength sm (Quote q)   = 4 + specLength sm q
specLength _  HARDNL      = 0

-- | Invalid characters, not included in an expression.
dontCount :: String
dontCount = "\\/[]{}()_^$:"

-- | Creates the header for a table.
makeHeaders :: PrintingInformation -> [Spec] -> D
makeHeaders sm ls = hpunctuate (text " & ") (map (bold . spec sm) ls) %% pure dbs

-- | Create rows for a table with a single line break between them.
makeRows :: PrintingInformation -> [[Spec]] -> D
makeRows _  [] = mempty
makeRows sm lls = foldr1 ((%%) . (%% pure dbs)) $ map (makeColumns sm) lls

-- | Creates the columns for a table.
makeColumns :: PrintingInformation -> [Spec] -> D
makeColumns sm ls = hpunctuate (text " & ") $ map (spec sm) ls

------------------ Spec -----------------------------------

-- | Helper that determines the printing context based on the kind of 'Spec'.
needs :: Spec -> MathContext
needs (a :+: b) = needs a `lub` needs b
needs (S _)     = Text
needs Ch {}    = Text
needs (E _)     = Math
needs (Sp _)    = Math
needs HARDNL    = Text
needs Ref{}     = Text
needs EmptyS    = Text
needs (Quote _) = Text

-- | Prints all 'Spec's.
spec :: PrintingInformation -> Spec -> D
spec sm a@(s :+: t) = s' <> t'
  where
    ctx = const $ needs a
    s' = switch ctx $ spec sm s
    t' = switch ctx $ spec sm t
spec _  (E ex) = toMath $ pExpr ex
spec _  (S s)  = either error (pure . text . concatMap escapeChars) $ L.checkValidStr s invalid
  where
    invalid = ['&', '#', '$', '%', '&', '~', '^', '\\', '{', '}']
    escapeChars '_' = "\\_"
    escapeChars '&' = "\\&"
    escapeChars c = [c]
spec sm  (Ch st caps s) = spec sm $ I.spec sm $ termStyleLookup st (sm ^. ckdb) s caps 
spec _  (Sp s) = pure $ text $ unPL $ L.special s
spec _  HARDNL = command0 "newline"
spec sm  (Ref Internal r sn) = snref r (spec sm sn)
spec sm  (Ref (Cite2 n) r _) = cite r (info sm n)
  where
    info _ EmptyS = Nothing
    info a x      = Just (spec a x)
spec sm (Ref External r sn) = externalref r (spec sm sn)
spec _  EmptyS              = empty
spec sm (Quote q)           = quote $ spec sm q

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
  pure (text (f ++ " & ")) <> print sm d) ls

-----------------------------------------------------------------
------------------ EQUATION PRINTING------------------------
-----------------------------------------------------------------

-- | Prints an equation.
makeEquation :: PrintingInformation -> Spec -> D
makeEquation sm contents = toEqn $ spec sm contents

  --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  --  on chunk (i.e. "eq:h_g" for h_g = ...

-----------------------------------------------------------------
------------------ LIST PRINTING----------------------------
-----------------------------------------------------------------

-- latex doesn't like empty lists, so don't put anything out for them.
-- empty lists here isn't quite wrong (though there should probably be
-- a warning higher up), so don't generate bad latex.
-- | Prints a list. LaTeX doesn't like empty lists, so those are rendered as 'empty'.
makeList :: PrintingInformation -> ListType -> D
makeList _  (Simple []   )      = empty
makeList _  (Desc []   )        = empty
makeList _  (Unordered []   )   = empty
makeList _  (Ordered []   )     = empty
makeList _  (Definitions []   ) = empty
makeList sm (Simple items)      = description' $ vcat $ simItem sm items
makeList sm (Desc items)        = description  $ vcat $ simItem sm items
makeList sm (Unordered items)   = itemize      $ vcat $ map (plItem sm) items
makeList sm (Ordered items)     = enumerate    $ vcat $ map (plItem sm) items
makeList sm (Definitions items) = symbDescription $ vcat $ defItem sm items

-- | Helper that renders items in 'makeList'.
plItem :: PrintingInformation -> (ItemType,Maybe Label) -> D
plItem sm (i, l) = mlref sm l <> pItem sm i

-- | Helper that renders the 'Spec' part of labels in 'mlref'.
lspec :: PrintingInformation -> Spec -> D  -- FIXME: Should be option rolled in to spec
lspec _  (S s) = pure $ text s
lspec sm r = spec sm r

-- | Helper that renders labels in 'plItem'. 
mlref :: PrintingInformation -> Maybe Label -> D
mlref sm = maybe empty $ (<>) (command0 "phantomsection") . label . lspec sm

-- | Helper that renders items in 'plItem'.
pItem :: PrintingInformation -> ItemType -> D
pItem sm (Flat s) = item $ spec sm s
pItem sm (Nested t s) = vcat [item $ spec sm t, makeList sm s]

-- | Helper that renders simple and descriptive items in 'makeList'.
simItem :: PrintingInformation -> [(Spec,ItemType,Maybe Label)] -> [D]
simItem sm = map (\(x,y,l) -> item' (spec sm (x :+: S ":") <> mlref sm l) $ sp_item y)
  where sp_item (Flat s) = spec sm s
        sp_item (Nested t s) = vcat [spec sm t, makeList sm s]

-- | Helper that renders definitions in 'makeList'.
defItem :: PrintingInformation -> [(Spec, ItemType,Maybe Label)] -> [D]
defItem sm = map (\(x,y,l) -> item $ mlref sm l <> spec sm (x :+: S " is the " :+: d_item y))
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
showBibTeX sm (Address      s) = showField sm    "address" s
showBibTeX sm (Author       p) = showField sm    "author" (rendPeople sm p)
showBibTeX sm (BookTitle    b) = showField sm    "booktitle" b
showBibTeX sm (Chapter      c) = showField sm    "chapter" (wrapS c)
showBibTeX sm (Edition      e) = showField sm    "edition" (wrapS e)
showBibTeX sm (Editor       e) = showField sm    "editor" (rendPeople sm e)
showBibTeX sm (Institution  i) = showField sm    "institution" i
showBibTeX sm (Journal      j) = showField sm    "journal" j
showBibTeX sm (Month        m) = showFieldRaw sm "month" (bibTeXMonth m)
showBibTeX sm (Note         n) = showField sm    "note" n
showBibTeX sm (Number       n) = showField sm    "number" (wrapS n)
showBibTeX sm (Organization o) = showField sm    "organization" o
showBibTeX sm (Pages        p) = showField sm    "pages" (I.spec sm $ L.foldNums "--" p)
showBibTeX sm (Publisher    p) = showField sm    "publisher" p
showBibTeX sm (School       s) = showField sm    "school" s
showBibTeX sm (Series       s) = showField sm    "series" s
showBibTeX sm (Title        t) = showField sm    "title" t
showBibTeX sm (Type         t) = showField sm    "type" t
showBibTeX sm (Volume       v) = showField sm    "volume" (wrapS v)
showBibTeX sm (Year         y) = showField sm    "year" (wrapS y)
showBibTeX sm (HowPublished (URL  u)) = showFieldCom sm "url" "howpublished" u
showBibTeX sm (HowPublished (Verb v)) = showField sm "howpublished" v

--showBibTeX sm (Author p@(Person {_convention=Mono}:_)) = showField "author"
  -- (LS.spec sm (rendPeople p)) :+: S ",\n" :+:
  -- showField "sortkey" (LS.spec sm (rendPeople p))
-- showBibTeX sm (Author    p) = showField "author" $ LS.spec sm (rendPeople p)

-- | Citation fields may be wrapped with braces, nothing, or a command.
data FieldWrap = Braces | NoDelimiters | Command String

-- | Helper that renders citation fields with a wrapper.
wrapField :: PrintingInformation -> FieldWrap -> String -> Spec -> D
wrapField sm fw f s = pure (text (f ++ "=")) <> resolve fw (spec sm s)
  where
    resolve Braces       = br
    resolve NoDelimiters = id
    resolve (Command st) = br . commandD st

showField, showFieldRaw :: PrintingInformation -> String -> Spec -> D
-- | Helper that renders citation fields wrapped with braces.
showField sm    = wrapField sm Braces
-- | Helper that renders citation fields with no delimiters.
showFieldRaw sm = wrapField sm NoDelimiters

-- | Helper that renders citation fields with a command.
showFieldCom   :: PrintingInformation -> String -> String -> Spec -> D
showFieldCom sm s = wrapField sm (Command s)

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
