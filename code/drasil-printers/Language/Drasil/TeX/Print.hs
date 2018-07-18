module Language.Drasil.TeX.Print(genTeX) where

import Prelude hiding (print)
import Data.List (intersperse, transpose, partition)
import Text.PrettyPrint (text, (<+>))
import qualified Text.PrettyPrint as TP
import Numeric (showFFloat)
import Control.Applicative (pure)
import Control.Arrow (second)

import qualified Language.Drasil as L (USymb(US), colAwidth, colBwidth, bibStyleT, 
  bibFname, RenderSpecial(..), People, rendPersLFM, HasSymbolTable,
  CitationKind(..), Month(..), Symbol(..), Sentence(S), (+:+), MaxWidthPercent,
  Decoration(Prime, Hat, Vector), Document, special)

import Language.Drasil.Printing.AST (Spec, ItemType(Nested, Flat), 
  ListType(Ordered, Unordered, Desc, Definitions, Simple), 
  Spec(Quote, EmptyS, Ref, S, Sy, Sp, HARDNL, E, (:+:)), 
  Fence(Norm, Abs, Curly, Paren), Expr, 
  Ops(Inte, Prod, Summ, Mul, Add, Or, And, Subt, Iff, LEq, GEq, 
  NEq, Eq, Gt, Lt, Impl, Dot, Cross, Neg, Exp, Dim, Not, Cot,
  Csc, Sec, Tan, Cos, Sin, Log, Ln, Prime, Comma, Boolean, Real, Natural, 
  Rational, Integer, IsIn), Spacing(Thin), Fonts(Emph, Bold), 
  Expr(Spc, Sqrt, Font, Fenced, MO, Over, Sup, Sub, Ident, Spec, Row, 
  Mtx, Div, Case, Str, Int, Dbl), OverSymb(Hat), Label)
import Language.Drasil.Printing.Citation (HP(Verb, URL), CiteField(HowPublished, 
  Year, Volume, Type, Title, Series, School, Publisher, Organization, Pages,
  Month, Number, Note, Journal, Editor, Chapter, Institution, Edition, BookTitle,
  Author, Address), Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (LayoutObj(Graph, Bib, ALUR, Figure, Definition,
  List, Table, EqnBlock, Paragraph, Header, HDiv), Document(Document), 
  ALUR(LikelyChange, UnlikelyChange, Assumption, Requirement))
import qualified Language.Drasil.Printing.Import as I
import Language.Drasil.Printing.Helpers hiding (paren, sqbrac)
import Language.Drasil.TeX.Helpers (label, caption, centering, mkEnv, item', description,
  includegraphics, center, figure, item, symbDescription, enumerate, itemize, toEqn, empty,
  newline, superscript, parens, fraction, quote, ref, ucref, lcref, aref, mref, sref, rref,
  hyperref, cite, sec, newpage, maketoc, maketitle, document, author, title)
import Language.Drasil.TeX.Monad (D, MathContext(Curr, Math, Text), (<>), vcat, (%%),
  toMath, switch, unPL, lub, hpunctuate, toText, ($+$), runPrint)
import Language.Drasil.TeX.Preamble (genPreamble)
import qualified Language.Drasil.RefTypes as RT

{-
import qualified Language.Drasil.Spec as LS
import qualified Language.Drasil.RefTypes as RT
import Language.Drasil.Development.UnitLang (USymb(US))
import Language.Drasil.Config (colAwidth, colBwidth, bibStyleT,bibFname)
import           Language.Drasil.Symbol (Symbol(..))
import qualified Language.Drasil.Symbol as S
import qualified Language.Drasil.Document as L
import Language.Drasil.Unicode (RenderSpecial(..))
import Language.Drasil.People (People,rendPersLFM)
import Language.Drasil.ChunkDB (HasSymbolTable)
import Language.Drasil.Chunk.Citation (CitationKind(..), Month(..))
-}

genTeX :: L.HasSymbolTable ctx => L.Document -> ctx -> TP.Doc
genTeX doc sm = runPrint (buildStd sm $ I.makeDocument sm doc) Text

buildStd :: L.HasSymbolTable s => s -> Document -> D
buildStd sm (Document t a c) =
  genPreamble c %%
  title (spec t) %%
  author (spec a) %%
  document (maketitle %% maketoc %% newpage %% print sm c)

-- clean until here; lo needs its sub-functions fixed first though
lo :: L.HasSymbolTable s => LayoutObj -> s -> D
lo (Header d t l)       _  = sec d (spec t) %% label (spec l)
lo (HDiv _ con _)       sm = print sm con -- FIXME ignoring 2 arguments?
lo (Paragraph contents) _  = toText $ spec contents
lo (EqnBlock contents)  _  = makeEquation contents
lo (Table _ rows r bl t) _  = toText $ makeTable rows (spec r) bl (spec t)
lo (Definition _ ssPs l) sm  = toText $ makeDefn sm ssPs $ spec l
lo (List l)               _  = toText $ makeList l
lo (Figure r c f wp)      _  = toText $ makeFigure (spec r) (spec c) f wp
lo (ALUR Requirement n l _)    _  = toText $ makeReq (spec n) (spec l)
lo (ALUR Assumption n l _)     _  = toText $ makeAssump (spec n) (spec l)
lo (ALUR LikelyChange n l _)   _  = toText $ makeLC (spec n) (spec l)
lo (ALUR UnlikelyChange n l _) _  = toText $ makeUC (spec n) (spec l)
lo (Bib bib)            sm = toText $ makeBib sm bib
lo (Graph ps w h c l)   _  = toText $ makeGraph
  (map (\(a,b) -> (spec a, spec b)) ps)
  (pure $ text $ maybe "" (\x -> "text width = " ++ show x ++ "em ,") w)
  (pure $ text $ maybe "" (\x -> "minimum height = " ++ show x ++ "em, ") h)
  (spec c) (spec l)

print :: L.HasSymbolTable s => s -> [LayoutObj] -> D
print sm l = foldr ($+$) empty $ map (flip lo sm) l

------------------ Symbol ----------------------------
symbol :: L.Symbol -> String
symbol (L.Atomic s)  = s
symbol (L.Special s) = unPL $ L.special s
--symbol (Greek g)   = unPL $ greek g
symbol (L.Concat sl) = foldr (++) "" $ map symbol sl
--
-- handle the special cases first, then general case
symbol (L.Corners [] [] [x] [] s) = brace $ (symbol s) ++"^"++ brace (symbol x)
symbol (L.Corners [] [] [] [x] s) = brace $ (symbol s) ++"_"++ brace (symbol x)
symbol (L.Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (L.Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (L.Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (L.Atop f s) = sFormat f s
symbol (L.Empty)    = ""

sFormat :: L.Decoration -> L.Symbol -> String
sFormat L.Hat    s = "\\hat{" ++ symbol s ++ "}"
sFormat L.Vector s = "\\mathbf{" ++ symbol s ++ "}"
sFormat L.Prime  s = symbol s ++ "'"

data OpenClose = Open | Close

-----------------------------------------------------------------
------------------ EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- (Since this is all implicitly in Math, leave it as String for now)
p_expr :: Expr -> String
p_expr (Dbl d)    = showFFloat Nothing d ""
p_expr (Int i)    = show i
p_expr (Str s)    = s  -- FIXME this is probably the wrong way to print strings
p_expr (Div n d) = "\\frac{" ++ p_expr n ++ "}{" ++ p_expr d ++"}"
p_expr (Case ps)  = "\\begin{cases}\n" ++ cases ps ++ "\n\\end{cases}"
p_expr (Mtx a)    = "\\begin{bmatrix}\n" ++ p_matrix a ++ "\n\\end{bmatrix}"
p_expr (Row [x]) = brace $ p_expr x -- a bit of a hack...
p_expr (Row l) = concatMap p_expr l
p_expr (Ident s) = s
p_expr (Spec s) = unPL $ L.special s
--p_expr (Gr g) = unPL $ greek g
p_expr (Sub e) = "_" ++ brace (p_expr e)
p_expr (Sup e) = "^" ++ brace (p_expr e)
p_expr (Over Hat s)     = "\\hat{" ++ p_expr s ++ "}"
p_expr (MO o) = p_ops o
p_expr (Fenced l r m)    = fence Open l ++ p_expr m ++ fence Close r
p_expr (Font Bold e) = "\\mathbf{" ++ p_expr e ++ "}"
p_expr (Font Emph e) = p_expr e -- Emph is ignored here because we're in Math mode
p_expr (Spc Thin) = "\\,"
p_expr (Sqrt e)  = "\\sqrt{" ++ p_expr e ++ "}"

p_ops :: Ops -> String
p_ops IsIn     = "\\in{}"
p_ops Integer  = "\\mathbb{Z}"
p_ops Rational = "\\mathbb{Q}"
p_ops Real     = "\\mathbb{R}"
p_ops Natural  = "\\mathbb{N}"
p_ops Boolean  = "\\mathbb{B}"
p_ops Comma    = ","
p_ops Prime    = "'"
p_ops Log      = "\\log"
p_ops Ln       = "\\ln"
p_ops Sin      = "\\sin"
p_ops Cos      = "\\cos"
p_ops Tan      = "\\tan"
p_ops Sec      = "\\sec"
p_ops Csc      = "\\csc"
p_ops Cot      = "\\cot"
p_ops Not      = "\\neg{}"
p_ops Dim      = "\\mathsf{dim}"
p_ops Exp      = "e"
p_ops Neg      = "-"
p_ops Cross    = "\\times"
p_ops Dot      = "\\cdot{}"
p_ops Eq       = "="
p_ops NEq      = "\\neq{}"
p_ops Lt       = "<"
p_ops Gt       = ">"
p_ops GEq      = "\\geq{}"
p_ops LEq      = "\\leq{}"
p_ops Impl     = "\\implies{}"
p_ops Iff      = "\\iff{}"
p_ops Subt     = "-"
p_ops And      = "\\land{}"
p_ops Or       = "\\lor{}"
p_ops Add      = "+"
p_ops Mul      = " "
p_ops Summ     = "\\displaystyle\\sum"
p_ops Prod     = "\\displaystyle\\prod"
p_ops Inte     = "\\int"

fence :: OpenClose -> Fence -> String
fence Open Paren = "\\left("
fence Close Paren = "\\right)"
fence Open Curly = "\\{"
fence Close Curly = "\\}"
fence _ Abs = "|"
fence _ Norm = "||"

-- | For printing Matrix
p_matrix :: [[Expr]] -> String
p_matrix [] = ""
p_matrix [x] = p_in x
p_matrix (x:xs) = p_matrix [x] ++ "\\\\\n" ++ p_matrix xs

p_in :: [Expr] -> String
p_in [] = ""
p_in [x] = p_expr x
p_in (x:xs) = p_in [x] ++ " & " ++ p_in xs

cases :: [(Expr,Expr)] -> String
cases []     = error "Attempt to create case expression without cases"
cases (p:[]) = (p_expr $ fst p) ++ ", & " ++ p_expr (snd p)
cases (p:ps) = cases [p] ++ "\\\\\n" ++ cases ps

-----------------------------------------------------------------
------------------ TABLE PRINTING---------------------------
-----------------------------------------------------------------

makeTable :: [[Spec]] -> D -> Bool -> D -> D
makeTable lls r bool t =
  pure (text ("\\begin{" ++ ltab ++ "}" ++ (brace . unwords . anyBig) lls))
  %% (pure (text "\\toprule"))
  %% makeRows [head lls]
  %% (pure (text "\\midrule"))
  %% makeRows (tail lls)
  %% (pure (text "\\bottomrule"))
  %% (if bool then caption t else empty)
  %% label r
  %% (pure $ text ("\\end{" ++ ltab ++ "}"))
  where ltab = tabType $ anyLong lls
        tabType True  = ltabu
        tabType False = ltable
        ltabu  = "longtabu" --Only needed if "X[l]" is used
        ltable = "longtable" ++ (if not bool then "*" else "")
        descr True  = "X[l]"
        descr False = "l"
  --returns "X[l]" for columns with long fields
        anyLong = or . map longColumn . transpose
        anyBig = map (descr . longColumn) . transpose
        longColumn = any (\x -> specLength x > 50)

-- | determines the length of a Spec
specLength :: Spec -> Int
specLength (S x)     = length x
specLength (E x)     = length $ filter (\c -> c `notElem` dontCount) $ p_expr x
specLength (Sy _)    = 1
specLength (a :+: b) = specLength a + specLength b
specLength (EmptyS)  = 0
specLength _         = 0

dontCount :: String
dontCount = "\\/[]{}()_^$:"

makeRows :: [[Spec]] -> D
makeRows []     = empty
makeRows (c:cs) = makeColumns c %% pure dbs %% makeRows cs

makeColumns :: [Spec] -> D
makeColumns ls = hpunctuate (text " & ") $ map spec ls

------------------ Spec -----------------------------------

needs :: Spec -> MathContext
needs (a :+: b) = needs a `lub` needs b
needs (S _)         = Text
needs (E _)         = Math
needs (Sy _)        = Text
needs (Sp _)        = Math
needs HARDNL        = Text
needs (Ref _ _ _ _) = Text
needs (EmptyS)      = Text
needs (Quote _)     = Text

-- print all Spec through here
spec :: Spec -> D
spec a@(s :+: t) = s' <> t'
  where
    ctx = const $ needs a
    s' = switch ctx $ spec s
    t' = switch ctx $ spec t
spec (E ex)      = toMath $ pure $ text $ p_expr ex
spec (S s)       = pure $ text (concatMap escapeChars s)
spec (Sy s)      = p_unit s
spec (Sp s)      = pure $ text $ unPL $ L.special s
spec HARDNL      = pure $ text "\\newline"
spec (Ref t@RT.Sect r _ _) = sref (show t) (pure $ text r)
spec (Ref t@RT.Def r _ _)  = hyperref (show t) (pure $ text r)
spec (Ref RT.Mod r _ _)    = mref  (pure $ text r)
spec (Ref RT.Req r _ _)    = rref  (pure $ text r)
spec (Ref RT.Assump r _ _) = aref  (pure $ text r)
spec (Ref RT.LC r _ _)     = lcref (pure $ text r)
spec (Ref RT.UC r _ _)     = ucref (pure $ text r)
spec (Ref RT.Cite r _ _)   = cite  (pure $ text r)
spec (Ref t r _ _)         = ref (show t) (pure $ text r)
spec EmptyS              = empty
spec (Quote q)           = quote $ spec q

escapeChars :: Char -> String
escapeChars '_' = "\\_"
escapeChars c = c : []

symbol_needs :: L.Symbol -> MathContext
symbol_needs (L.Atomic _)          = Text
symbol_needs (L.Special _)         = Math
--symbol_needs (Greek _)           = Math
symbol_needs (L.Concat [])         = Math
symbol_needs (L.Concat (s:_))      = symbol_needs s
symbol_needs (L.Corners _ _ _ _ _) = Math
symbol_needs (L.Atop _ _)          = Math
symbol_needs L.Empty               = Curr

p_unit :: L.USymb -> D
p_unit (L.US ls) = formatu t b
  where
    (t,b) = partition ((> 0) . snd) ls
    formatu :: [(L.Symbol,Integer)] -> [(L.Symbol,Integer)] -> D
    formatu [] l = line l 
    formatu l [] = foldr (<>) empty $ map pow l
    formatu nu de = toMath $ fraction (line nu) (line $ map (second negate) de)
    line :: [(L.Symbol,Integer)] -> D
    line []  = empty
    line [n] = pow n
    line l   = parens $ foldr (<>) empty $ map pow l
    pow :: (L.Symbol,Integer) -> D
    pow (n,1) = p_symb n
    pow (n,p) = toMath $ superscript (p_symb n) (pure $ text $ show p)
    -- printing of unit symbols is done weirdly... FIXME?
    p_symb (L.Concat s) = foldl (<>) empty $ map p_symb s
    p_symb n = let cn = symbol_needs n in switch (const cn) (pure $ text $ symbol n)

{-
p_unit :: USymb -> D
p_unit (UName (Concat s)) = foldl (<>) empty $ map (p_unit . UName) s
p_unit (UName n) =
  let cn = symbol_needs n in
  switch (const cn) (pure $ text $ symbol n)
p_unit (UProd l) = foldr (<>) empty (map p_unit l)
p_unit (UPow n p) = toMath $ superscript (p_unit n) (pure $ text $ show p)
p_unit (UDiv n d) = toMath $
  case d of -- 4 possible cases, 2 need parentheses, 2 don't
    UProd _  -> fraction (p_unit n) (parens $ p_unit d)
    UDiv _ _ -> fraction (p_unit n) (parens $ p_unit d)
    _        -> fraction (p_unit n) (p_unit d)
-}

-----------------------------------------------------------------
------------------ DATA DEFINITION PRINTING-----------------
-----------------------------------------------------------------

makeDefn :: L.HasSymbolTable s => s -> [(String,[LayoutObj])] -> D -> D
makeDefn _  [] _ = error "Empty definition"
makeDefn sm ps l = beginDefn %% makeDefTable sm ps l %% endDefn

beginDefn :: D
beginDefn = (pure $ text "~") <> newline
  %% (pure $ text "\\noindent \\begin{minipage}{\\textwidth}")

endDefn :: D
endDefn = pure $ text "\\end{minipage}" TP.<> dbs

makeDefTable :: L.HasSymbolTable s => s -> [(String,[LayoutObj])] -> D -> D
makeDefTable _ [] _ = error "Trying to make empty Data Defn"
makeDefTable sm ps l = vcat [
  pure $ text 
  $ "\\begin{tabular}{p{"++show L.colAwidth++"\\textwidth} p{"++show L.colBwidth++"\\textwidth}}",
  (pure $ text "\\toprule \\textbf{Refname} & \\textbf{") <> l <> (pure $ text "}"), --shortname instead of refname?
  (pure $ text "\\phantomsection "), label l,
  makeDRows sm ps,
  pure $ dbs <+> text ("\\bottomrule \\end{tabular}")
  ]

makeDRows :: L.HasSymbolTable s => s -> [(String,[LayoutObj])] -> D
makeDRows _  []         = error "No fields to create Defn table"
makeDRows sm ((f,d):[]) = dBoilerplate %% (pure $ text (f ++ " & ")) <>
  (vcat $ map (flip lo sm) d)
makeDRows sm ((f,d):ps) = dBoilerplate %% (pure $ text (f ++ " & ")) <>
  (vcat $ map (flip lo sm) d)
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
makeList (Simple items)      = itemize     $ vcat $ sim_item items
makeList (Desc items)        = description $ vcat $ sim_item items
makeList (Unordered items)   = itemize     $ vcat $ map pl_item items
makeList (Ordered items)     = enumerate   $ vcat $ map pl_item items
makeList (Definitions items) = symbDescription $ vcat $ def_item items

pl_item :: (ItemType,Maybe Label) -> D
pl_item (i, l) = mlref l $ p_item i

mlref :: Maybe Label -> D -> D
mlref = maybe id $ (%%) . label . spec

p_item :: ItemType -> D
p_item (Flat s) = item $ spec s
p_item (Nested t s) = vcat [item $ spec t, makeList s]

sim_item :: [(Spec,ItemType,Maybe Label)] -> [D]
sim_item = map (\(x,y,l) -> item' (spec $ x :+: S ":") $ mlref l $ sp_item y)
  where sp_item (Flat s) = spec s
        sp_item (Nested t s) = vcat [spec t, makeList s]

def_item :: [(Spec, ItemType,Maybe Label)] -> [D]
def_item = map (\(x,y,l) -> item $ mlref l $ spec $ x :+: S " is the " :+: d_item y)
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
-- p_op f@(Summation bs) x = oper f ++ makeBound bs ++ brace (sqbrac (p_expr x))
-- p_op f@(Product bs) x = oper f ++ makeBound bs ++ brace (p_expr x)
-- p_op f@(Integral bs wrtc) x = oper f ++ makeIBound bs ++ 
--   brace (p_expr x ++ "d" ++ symbol wrtc) -- HACK alert.
-- 
-- makeBound :: Maybe ((Symbol, Expr),Expr) -> String
-- makeBound (Just ((s,v),hi)) = "_" ++ brace ((symbol s ++"="++ p_expr v)) ++
--                               "^" ++ brace (p_expr hi)
-- makeBound Nothing = ""
-- 
-- makeIBound :: (Maybe Expr, Maybe Expr) -> String
-- makeIBound (Just low, Just high) = "_" ++ brace (p_expr low) ++
--                                    "^" ++ brace (p_expr high)
-- makeIBound (Just low, Nothing)   = "_" ++ brace (p_expr low)
-- makeIBound (Nothing, Just high)  = "^" ++ brace (p_expr high)
-- makeIBound (Nothing, Nothing)    = ""

-----------------------------------------------------------------
------------------ MODULE PRINTING----------------------------
-----------------------------------------------------------------

makeReq :: D -> D -> D
makeReq n l = description $ item' ((pure $ text ("\\refstepcounter{reqnum}"
  ++ "\\rthereqnum")) <> label l <> (pure $ text ":")) n

makeAssump :: D -> D -> D
makeAssump n l = description $ item' ((pure $ text ("\\refstepcounter{assumpnum}"
  ++ "\\atheassumpnum")) <> label l <> (pure $ text ":")) n

makeLC :: D -> D -> D
makeLC n l = description $ item' ((pure $ text ("\\refstepcounter{lcnum}"
  ++ "\\lcthelcnum")) <> label l <> (pure $ text ":")) n

makeUC :: D -> D -> D
makeUC n l = description $ item' ((pure $ text ("\\refstepcounter{ucnum}"
  ++ "\\uctheucnum")) <> label l <> (pure $ text ":")) n

makeGraph :: [(D,D)] -> D -> D -> D -> D -> D
makeGraph ps w h c l =
  mkEnv "figure" $
  vcat $ [ centering,
           pure $ text "\\begin{adjustbox}{max width=\\textwidth}",
           pure $ text "\\begin{tikzpicture}[>=latex,line join=bevel]",
           (pure $ text "\\tikzstyle{n} = [draw, shape=rectangle, ") <>
             w <> h <> (pure $ text "font=\\Large, align=center]"),
           pure $ text "\\begin{dot2tex}[dot, codeonly, options=-t raw]",
           pure $ text "digraph G {",
           pure $ text "graph [sep = 0. esep = 0, nodesep = 0.1, ranksep = 2];",
           pure $ text "node [style = \"n\"];"
         ]
     ++  map (\(a,b) -> (q a) <> (pure $ text " -> ") <> (q b) <>
                (pure $ text ";")) ps
     ++  [ pure $ text "}",
           pure $ text "\\end{dot2tex}",
           pure $ text "\\end{tikzpicture}",
           pure $ text "\\end{adjustbox}",
           caption c,
           label l
         ]
  where q x = (pure $ text "\"") <> x <> (pure $ text "\"")

---------------------------
-- Bibliography Printing --
---------------------------
-- **THE MAIN FUNCTION** --
makeBib :: L.HasSymbolTable s => s -> BibRef -> D
makeBib sm bib = spec $
  S ("\\begin{filecontents*}{"++L.bibFname++".bib}\n") :+: --bibFname is in Config.hs
  mkBibRef sm bib :+:
  S "\n\\end{filecontents*}\n" :+:
  S bibLines

bibLines :: String
bibLines =
  "\\nocite{*}\n" ++
  "\\bibstyle{" ++ L.bibStyleT ++ "}\n" ++ --bibStyle is in Config.hs
  "\\printbibliography[heading=none]"

mkBibRef :: L.HasSymbolTable s => s -> BibRef -> Spec
mkBibRef sm = foldl1 (\x y -> x :+: S "\n\n" :+: y) . map (renderF sm)

renderF :: L.HasSymbolTable s => s -> Citation -> Spec
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

showBibTeX :: L.HasSymbolTable s => s -> CiteField -> Spec
showBibTeX  _ (Address      s) = showField "address" s
showBibTeX sm (Author       p) = showField "author" (rendPeople sm p)
showBibTeX  _ (BookTitle    b) = showField "booktitle" b
showBibTeX  _ (Chapter      c) = showField "chapter" (wrapS c)
showBibTeX  _ (Edition      e) = showField "edition" (wrapS e)
showBibTeX sm (Editor       e) = showField "editor" (rendPeople sm e)
showBibTeX  _ (Institution  i) = showField "institution" i
showBibTeX  _ (Journal      j) = showField "journal" j
showBibTeX  _ (Month        m) = showField "month" (bibTeXMonth m)
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

rendPeople :: L.HasSymbolTable ctx => ctx -> L.People -> Spec
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
pages (x:[]) = wrapS x
pages (x:x2:[]) = wrapS $ show x ++ "-" ++ show x2
pages xs = error $ "Too many pages given in reference. Received: " ++ show xs
