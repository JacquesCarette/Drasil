module Language.Drasil.TeX.Print where

import Prelude hiding (print)
import Data.List (intersperse, transpose)
import Text.PrettyPrint (text, (<+>))
import qualified Text.PrettyPrint as TP
import Numeric (showFFloat)

import Control.Applicative (pure)

import Language.Drasil.Printing.AST
import Language.Drasil.TeX.AST
import qualified Language.Drasil.TeX.Import as I
import qualified Language.Drasil.Output.Formats as A
import qualified Language.Drasil.Spec as LS
import Language.Drasil.Config (lpmTeXParams, colAwidth, colBwidth,
              LPMParams(..),bibStyleT,bibFname)
import Language.Drasil.Printing.Helpers hiding (paren, sqbrac)
import Language.Drasil.TeX.Helpers
import Language.Drasil.TeX.Monad
import Language.Drasil.TeX.Preamble
import Language.Drasil.Symbol (Symbol(..),Decoration(..))
import qualified Language.Drasil.Document as L
import Language.Drasil.Unicode (RenderGreek(..), RenderSpecial(..))
import Language.Drasil.People (People,rendPersLFM,lstName,Person(..),Conv(Mono))
import Language.Drasil.ChunkDB (HasSymbolTable)
import Language.Drasil.Space (Space(..))

genTeX :: HasSymbolTable ctx => A.DocType -> L.Document -> ctx -> TP.Doc
genTeX typ doc sm = runPrint (build sm typ $ I.makeDocument doc sm) Text

build :: HasSymbolTable s => s -> A.DocType -> Document -> D
build sm (A.SRS _) doc   = buildStd sm doc
build sm (A.MG _) doc    = buildStd sm doc
build sm (A.MIS _) doc   = buildStd sm doc
build sm (A.LPM _) doc   = buildLPM sm lpmTeXParams doc
build _ (A.Website _) _ = error "Cannot use TeX to typeset Website" --Can't happen

buildStd :: HasSymbolTable s => s -> Document -> D
buildStd sm (Document t a c) =
  genPreamble c %%
  title (spec t) %%
  author (spec a) %%
  document (maketitle %% maketoc %% newpage %% print sm c)

buildLPM :: HasSymbolTable s => s -> LPMParams -> Document -> D
buildLPM sm (LPMParams (A.DocClass sb b1) (A.UsePackages ps) (A.ExDoc f n))
          (Document t a c) =
  docclass sb b1 %%
  listpackages ps %%
  exdoc f n %%
  title (spec t) %%
  author (spec a) %%
  document (maketitle %% print sm c)

listpackages :: [String] -> D
listpackages lp = foldr (%%) empty $ map usepackage lp

-- clean until here; lo needs its sub-functions fixed first though
lo :: HasSymbolTable s => LayoutObj -> s -> D
lo (Section d t con l)  sm  = sec d (spec t) %% label (spec l) %% print sm con
lo (Paragraph contents) _  = toText $ spec contents
lo (EqnBlock contents)  _  = makeEquation contents
lo (Table rows r bl t)  _  = toText $ makeTable rows (spec r) bl (spec t)
lo (Definition ssPs l) sm  = toText $ makeDefn sm ssPs $ spec l
lo (Defnt _ ssPs l)    sm  = toText $ makeDefn sm ssPs $ spec l
lo (List l)             _  = toText $ makeList l
lo (Figure r c f wp)    _  = toText $ makeFigure (spec r) (spec c) f wp
lo (Requirement n l)    _  = toText $ makeReq (spec n) (spec l)
lo (Assumption n l)     _  = toText $ makeAssump (spec n) (spec l)
lo (LikelyChange n l)   _  = toText $ makeLC (spec n) (spec l)
lo (UnlikelyChange n l) _  = toText $ makeUC (spec n) (spec l)
lo (Bib bib)            sm = toText $ makeBib sm bib
lo (Graph ps w h c l)   _  = toText $ makeGraph
  (map (\(a,b) -> (spec a, spec b)) ps)
  (pure $ text $ maybe "" (\x -> "text width = " ++ show x ++ "em ,") w)
  (pure $ text $ maybe "" (\x -> "minimum height = " ++ show x ++ "em, ") h)
  (spec c) (spec l)


print :: HasSymbolTable s => s -> [LayoutObj] -> D
print sm l = foldr ($+$) empty $ map (flip lo sm) l

------------------ Symbol ----------------------------
symbol :: Symbol -> String
symbol (Atomic s)  = s
symbol (Special s) = unPL $ special s
symbol (Greek g)   = unPL $ greek g
symbol (Concat sl) = foldr (++) "" $ map symbol sl
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = brace $ (symbol s) ++"^"++ brace (symbol x)
symbol (Corners [] [] [] [x] s) = brace $ (symbol s) ++"_"++ brace (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (Atop f s) = sFormat f s

sFormat :: Decoration -> Symbol -> String
sFormat Hat    s = "\\hat{" ++ symbol s ++ "}"
sFormat Vector s = "\\mathbf{" ++ symbol s ++ "}"
sFormat Prime  s = symbol s ++ "'"

-----------------------------------------------------------------
------------------ EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- (Since this is all implicitly in Math, leave it as String for now)
p_expr :: Expr -> String
p_expr (Var v)    = symbol (Atomic v) --Ensures variables are rendered the same as other symbols
p_expr (Dbl d)    = showFFloat Nothing d ""
p_expr (Int i)    = show i
p_expr (Assoc Add l)  = concat $ intersperse "+" $ map p_expr l
p_expr (Assoc Mul l)  = mul l
p_expr (Sym s)    = symbol s
p_expr (BOp Frac n d) = "\\frac{" ++ needMultlined n ++ "}{" ++ needMultlined d ++"}"
p_expr (BOp Div n d)  = divide n d
p_expr (BOp Pow x y)  = pow x y
p_expr (BOp Index a i) = p_indx a i
p_expr (BOp o x y)    = p_expr x ++ p_bop o ++ p_expr y
p_expr (Neg x)    = neg x
p_expr (Call f x) = p_expr f ++ paren (concat $ intersperse "," $ map p_expr x)
p_expr (Case ps)  = "\\begin{cases}\n" ++ cases ps ++ "\n\\end{cases}"
p_expr (Op f es)  = p_op f es
p_expr (Grouping x) = paren (p_expr x)
p_expr (Mtx a)    = "\\begin{bmatrix}\n" ++ p_matrix a ++ "\n\\end{bmatrix}"
--Logic
p_expr (Not x)    = "\\neg{}" ++ p_expr x
p_expr (Assoc And l)  = concat $ intersperse "\\land{}" $ map p_expr l
p_expr (Assoc Or l)   = concat $ intersperse "\\lor{}" $ map p_expr l
p_expr (IsIn  a b) = p_expr a ++ "\\in{}" ++ p_space b

p_expr (Forall v e) = "\\forall{} " ++ symbol v ++ ":\\ " ++ p_expr e
p_expr (Exists v e) = "\\exists{} " ++ symbol v ++ ":\\ " ++ p_expr e

p_bop :: BinOp -> String
p_bop Sub = "-"
p_bop Eq = "="
p_bop NEq = "\\neq{}"
p_bop Lt = "<"
p_bop Gt = ">"
p_bop GEq = "\\geq{}"
p_bop LEq = "\\leq{}"
p_bop Impl = "\\implies{}"
p_bop Iff = "\\iff{}"
p_bop Frac = "/"
p_bop Div = "/"
p_bop Pow = "^"
p_bop Dot = "\\cdot{}"
p_bop Index = error "no printing of Index"

-- | For seeing if long numerators or denominators need to be on multiple lines
needMultlined :: Expr -> String
needMultlined x
  | lngth > 70 = multl $ mklines $ map p_expr $ groupEx $ splitTerms x
  | otherwise  = p_expr x
  where lngth     = specLength $ E x
        multl str = "\\begin{multlined}\n" ++ str ++ "\\end{multlined}\n"
        mklines   = unlines . intersperse "\\\\+"
        --FIXME: make multiple splits if needed; don't always split in 2
        groupEx lst = extrac $ splitAt (length lst `div` 2) lst
        extrac ([],[]) = []
        extrac ([],l)  = [Assoc Add l]
        extrac (f,[])  = [Assoc Add f]
        extrac (f,l)   = [Assoc Add f, Assoc Add l]

splitTerms :: Expr -> [Expr]
splitTerms (Neg e)   = map Neg $ splitTerms e
splitTerms (Assoc Add l) = concat $ map splitTerms l
splitTerms (BOp Sub a b) = splitTerms a ++ splitTerms (Neg b)
splitTerms e = [e]

-- | For printing indexes
p_indx :: Expr -> Expr -> String
p_indx (Sym (Corners [] [] [] [x] s)) i = p_expr $ Sym $ Corners [][][][Concat [x, Atomic (","++ p_sub i)]] s
p_indx a@(Sym (Atomic _)) i = p_expr a ++"_"++ brace (p_sub i)
p_indx a@(Sym (Greek  _)) i = p_expr a ++"_"++ brace (p_sub i)
p_indx a                  i = brace (p_expr a) ++"_"++ brace (p_sub i)
-- Ensures only simple Expr's get rendered as an index
p_sub :: Expr -> String
p_sub e@(Var _)    = p_expr e
p_sub e@(Dbl _)    = p_expr e
p_sub e@(Int _)    = p_expr e
p_sub e@(Sym _)    = p_expr e
p_sub e@(Assoc Add _)  = p_expr e
p_sub e@(BOp Sub _ _)  = p_expr e
p_sub e@(Assoc Mul _)  = p_expr e
p_sub   (BOp Frac a b) = divide a b --no block division in an index
p_sub e@(BOp Div _ _)  = p_expr e
p_sub _            = error "Tried to Index a non-simple expr in LaTeX, currently not supported."

-- | For printing Matrix
p_matrix :: [[Expr]] -> String
p_matrix [] = ""
p_matrix [x] = p_in x
p_matrix (x:xs) = p_matrix [x] ++ "\\\\\n" ++ p_matrix xs

p_in :: [Expr] -> String
p_in [] = ""
p_in [x] = p_expr x
p_in (x:xs) = p_in [x] ++ " & " ++ p_in xs


-- | Helper for properly rendering multiplication of expressions
mul :: [ Expr ] -> String
mul = concat . intersperse " " . map mulParen

mulParen :: Expr -> String
mulParen a@(Assoc Add _) = paren $ p_expr a
mulParen a@(BOp Sub _ _) = paren $ p_expr a
mulParen a@(BOp Div _ _) = paren $ p_expr a
mulParen a = p_expr a

divide :: Expr -> Expr -> String
divide n d@(Assoc Add _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n d@(BOp Sub _ _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n@(Assoc Add _) d = paren (p_expr n) ++ "/" ++ p_expr d
divide n@(BOp Sub _ _) d = paren (p_expr n) ++ "/" ++ p_expr d
divide n d = p_expr n ++ "/" ++ p_expr d

neg :: Expr -> String
neg x@(Var _) = "-" ++ p_expr x
neg x@(Dbl _) = "-" ++ p_expr x
neg x@(Int _) = "-" ++ p_expr x
neg x@(Sym _) = "-" ++ p_expr x
neg x@(Neg _) = "-" ++ p_expr x
neg x         = paren ("-" ++ p_expr x)

pow :: Expr -> Expr -> String
pow x@(Assoc Add _) y = sqbrac (p_expr x) ++ "^" ++ brace (p_expr y)
pow x@(BOp Sub _ _) y = sqbrac (p_expr x) ++ "^" ++ brace (p_expr y)
pow x@(BOp Frac _ _) y = sqbrac (p_expr x) ++ "^" ++ brace (p_expr y)
pow x@(BOp Div _ _) y = paren (p_expr x) ++ "^" ++ brace (p_expr y)
pow x@(Assoc Mul _) y = paren (p_expr x) ++ "^" ++ brace (p_expr y)
pow x@(BOp Pow _ _) y = paren (p_expr x) ++ "^" ++ brace (p_expr y)
pow x y = p_expr x ++ "^" ++ brace (p_expr y)

cases :: [(Expr,Expr)] -> String
cases []     = error "Attempt to create case expression without cases"
cases (p:[]) = (needMultlined $ fst p) ++ ", & " ++ p_expr (snd p)
cases (p:ps) = cases [p] ++ "\\\\\n" ++ cases ps

p_space :: Space -> String
p_space Integer  = "\\mathbb{Z}"
p_space Rational = "\\mathbb{Q}"
p_space Real     = "\\mathbb{R}"
p_space Natural  = "\\mathbb{N}"
p_space Boolean  = "\\mathbb{B}"
p_space Char     = "Char"
p_space String   = "String"
p_space Radians  = "rad"
p_space (Vect a) = "V" ++ p_space a
p_space (Obj a)  = a
p_space (DiscreteI a)  = "\\{" ++ (concat $ intersperse ", " (map show a)) ++ "\\}"
p_space (DiscreteD a)  = "\\{" ++ (concat $ intersperse ", " (map show a)) ++ "\\}"
p_space (DiscreteS a)  = "\\{" ++ (concat $ intersperse ", " a) ++ "\\}"

function :: Function -> String
function Log            = "\\log"
function (Summation _)  = "\\displaystyle\\sum"
function (Product _)    = "\\displaystyle\\prod"
function Abs            = ""
function Norm           = ""
function (Integral _ _) = "\\int"
function Sin            = "\\sin"
function Cos            = "\\cos"
function Tan            = "\\tan"
function Sec            = "\\sec"
function Csc            = "\\csc"
function Cot            = "\\cot"
function Cross          = "\\times"
function Exp            = "e"
function Sqrt           = "\\sqrt"

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
specLength (a :-: b) = specLength a + specLength b
specLength (a :^: b) = specLength a + specLength b
specLength (a :/: b) = specLength a + specLength b
specLength (G _)     = 1
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
needs (S _)     = Text
needs (E _)     = Math
needs (_ :-: _) = Math --Sub/superscripts must be in Math ctxt.
needs (_ :^: _) = Math
needs (_ :/: _) = Math -- Fractions are always equations.
needs (Sy _)    = Text
needs (N _)     = Math
needs (G _)     = Math
needs (Sp _)    = Math
needs HARDNL    = Text
needs (Ref _ _) = Text
needs (EmptyS)  = Text  

-- print all Spec through here
spec :: Spec -> D
spec a@(s :+: t) = s' <> t'
  where
    ctx = const $ needs a
    s' = switch ctx $ spec s
    t' = switch ctx $ spec t
spec (E ex)      = toMath $ pure $ text $ p_expr ex
spec (a :-: s)   = toMath $ subscript (spec a) (spec s)
spec (a :^: s)   = toMath $ superscript (spec a) (spec s)
spec (a :/: s)   = toMath $ fraction (spec a) (spec s)
spec (S s)       = pure $ text (concatMap escapeChars s)
spec (N s)       = toMath $ pure $ text $ symbol s
spec (Sy s)      = p_unit s
spec (G g)       = pure $ text $ unPL $ greek g
spec (Sp s)      = pure $ text $ unPL $ special s
spec HARDNL      = pure $ text $ "\\newline"
spec (Ref t@LS.Sect r) = sref (show t) (spec r)
spec (Ref t@(LS.Def _) r) = hyperref (show t) (spec r)
spec (Ref t@LS.Mod r) = mref (show t) (spec r)
spec (Ref t@(LS.Req _) r) = rref (show t) (spec r)
spec (Ref t@(LS.Assump _) r) = aref (show t) (spec r)
spec (Ref t@(LS.LC _) r) = lcref (show t) (spec r)
spec (Ref t@LS.UC r) = ucref (show t) (spec r)
spec (Ref t r)   = ref (show t) (spec r)
spec EmptyS      = empty

escapeChars :: Char -> String
escapeChars '_' = "\\_"
escapeChars c = c : []

symbol_needs :: Symbol -> MathContext
symbol_needs (Atomic _)          = Text
symbol_needs (Special _)         = Math
symbol_needs (Greek _)           = Math
symbol_needs (Concat [])         = Math
symbol_needs (Concat (s:_))      = symbol_needs s
symbol_needs (Corners _ _ _ _ _) = Math
symbol_needs (Atop _ _)          = Math

p_unit :: LS.USymb -> D
p_unit (LS.UName (Concat s)) = foldl (<>) empty $ map (p_unit . LS.UName) s
p_unit (LS.UName n) =
  let cn = symbol_needs n in
  switch (const cn) (pure $ text $ symbol n)
p_unit (LS.UProd l) = foldr (<>) empty (map p_unit l)
p_unit (LS.UPow n p) = toMath $ superscript (p_unit n) (pure $ text $ show p)
p_unit (LS.UDiv n d) = toMath $
  case d of -- 4 possible cases, 2 need parentheses, 2 don't
    LS.UProd _  -> fraction (p_unit n) (parens $ p_unit d)
    LS.UDiv _ _ -> fraction (p_unit n) (parens $ p_unit d)
    _        -> fraction (p_unit n) (p_unit d)

-----------------------------------------------------------------
------------------ DATA DEFINITION PRINTING-----------------
-----------------------------------------------------------------

makeDefn :: HasSymbolTable s => s -> [(String,[LayoutObj])] -> D -> D
makeDefn _  [] _ = error "Empty definition"
makeDefn sm ps l = beginDefn %% makeDefTable sm ps l %% endDefn

beginDefn :: D
beginDefn = (pure $ text "~") <> newline
  %% (pure $ text "\\noindent \\begin{minipage}{\\textwidth}")

endDefn :: D
endDefn = pure $ text "\\end{minipage}" TP.<> dbs

makeDefTable :: HasSymbolTable s => s -> [(String,[LayoutObj])] -> D -> D
makeDefTable _ [] _ = error "Trying to make empty Data Defn"
makeDefTable sm ps l = vcat [
  pure $ text $ "\\begin{tabular}{p{"++show colAwidth++"\\textwidth} p{"++show colBwidth++"\\textwidth}}",
  (pure $ text "\\toprule \\textbf{Refname} & \\textbf{") <> l <> (pure $ text "}"),
  (pure $ text "\\phantomsection "), label l,
  makeDRows sm ps,
  pure $ dbs <+> text ("\\bottomrule \\end{tabular}")
  ]

makeDRows :: HasSymbolTable s => s -> [(String,[LayoutObj])] -> D
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
makeList (Simple items) = itemize   $ vcat (sim_item items)
makeList (Desc items)   = description $ vcat (sim_item items)
makeList (Item items)   = itemize   $ vcat (map p_item items)
makeList (Enum items)   = enumerate $ vcat (map p_item items)
makeList (Definitions items) = description $ vcat (def_item items)

p_item :: ItemType -> D
p_item (Flat s) = item (spec s)
p_item (Nested t s) = vcat [item (spec t), makeList s]

sim_item :: [(Spec,ItemType)] -> [D]
sim_item [] = [empty]
sim_item ((x,y):zs) = item' (spec (x :+: S ":")) (sp_item y) : sim_item zs
    where sp_item (Flat s) = spec s
          sp_item (Nested t s) = vcat [spec t, makeList s]
          
def_item :: [(Spec, ItemType)] -> [D]
def_item [] = [empty]
def_item ((x,y):zs) = item (spec (x :+: S " is the " :+: d_item y)) : def_item zs
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
p_op :: Function -> [Expr] -> String
p_op f@(Cross) xs = binfix_op f xs
p_op f@(Summation bs) (x:[]) = function f ++ makeBound bs ++ brace (sqbrac (p_expr x))
p_op (Summation _) _ = error "Something went wrong with a summation"
p_op f@(Product bs) (x:[]) = function f ++ makeBound bs ++ brace (p_expr x)
p_op f@(Integral bs wrtc) (x:[]) = function f ++ makeIBound bs ++ 
  brace (p_expr x ++ "d" ++ symbol wrtc) -- HACK alert.
p_op (Integral _ _) _  = error "Something went wrong with an integral"
p_op Abs (x:[]) = "|" ++ p_expr x ++ "|"
p_op Abs _ = error "Abs should only take one expr."
p_op Norm (x:[]) = "||" ++ p_expr x ++ "||"
p_op Norm _ = error "Norm should only take on expression."
p_op f@(Exp) (x:[]) = function f ++ "^" ++ brace (p_expr x)
p_op f@(Sqrt) (x:[]) = function f ++ "{" ++ p_expr x ++ "}"
p_op f (x:[]) = function f ++ paren (p_expr x) --Unary ops, this will change once more complicated functions appear.
p_op _ _ = error "Something went wrong with an operation"

makeBound :: Maybe ((Symbol, Expr),Expr) -> String
makeBound (Just ((s,v),hi)) = "_" ++ brace ((symbol s ++"="++ p_expr v)) ++
                              "^" ++ brace (p_expr hi)
makeBound Nothing = ""

makeIBound :: (Maybe Expr, Maybe Expr) -> String
makeIBound (Just low, Just high) = "_" ++ brace (p_expr low) ++ 
                                   "^" ++ brace (p_expr high)
makeIBound (Just low, Nothing)   = "_" ++ brace (p_expr low)
makeIBound (Nothing, Just high)  = "^" ++ brace (p_expr high)
makeIBound (Nothing, Nothing)    = ""

binfix_op :: Function -> [Expr] -> String
binfix_op f (x:y:[]) = p_expr x ++ function f ++ p_expr y
binfix_op _ _ = error "Attempting to print binary operator with inappropriate" ++
                   "number of operands (should be 2)"

-----------------------------------------------------------------
------------------ MODULE PRINTING----------------------------
-----------------------------------------------------------------

makeModule :: String -> D -> D
makeModule n l = description $ item' ((pure $ text ("\\refstepcounter{modnum}"
  ++ "\\mthemodnum")) <> label l <> (pure $ text ":")) (pure $ text n)

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
makeBib :: HasSymbolTable s => s -> BibRef -> D
makeBib sm bib = spec $
  S ("\\begin{filecontents*}{"++bibFname++".bib}\n") :+: --bibFname is in Config.hs
  mkBibRef sm bib :+:
  S "\n\\end{filecontents*}\n" :+:
  S bibLines

bibLines :: String
bibLines =
  "\\nocite{*}\n" ++ 
  "\\bibstyle{" ++ bibStyleT ++ "}\n" ++ --bibStyle is in Config.hs
  "\\printbibliography[heading=none]"

mkBibRef :: HasSymbolTable s => s -> BibRef -> Spec
mkBibRef sm = foldl1 (\x y -> x :+: S "\n\n" :+: y) . map (renderCite sm)

--for when we add other things to reference like website, newspaper
renderCite :: HasSymbolTable s => s -> Citation -> Spec
renderCite sm c@(Book      fields) = renderF sm c fields
renderCite sm c@(Article   fields) = renderF sm c fields
renderCite sm c@(MThesis   fields) = renderF sm c fields
renderCite sm c@(PhDThesis fields) = renderF sm c fields
renderCite sm c@(Misc      fields) = renderF sm c fields
renderCite sm c@(Online    fields) = renderF sm c fields

--Rendering a book--
renderF :: HasSymbolTable s => s -> Citation -> [CiteField] -> Spec
renderF sm c fields = 
  S "@":+: S (show c) :+: S "{" :+: (cite sm fields) :+: S ",\n" :+: 
  (foldl1 (:+:) . intersperse (S ",\n") . map (showBibTeX sm)) fields :+: S "}"
--renderBook _ = error "Tried to render a non-book using renderBook." 

cite :: HasSymbolTable s => s -> [CiteField] -> Spec
cite sm fields = foldr1 (:+:) $
  map (flip I.spec sm . rmSpace . lstName) (getAuthors fields) ++ [S $ show $ getYear fields]

-- Remove spaces
rmSpace :: LS.Sentence -> LS.Sentence
rmSpace (a LS.:+: b) = rmSpace a LS.:+: rmSpace b
rmSpace (LS.S x) = LS.S $ rmSpaceChar x
rmSpace y = y

rmSpaceChar :: [Char] -> [Char]
rmSpaceChar [] = []
rmSpaceChar (' ':xs) = rmSpaceChar xs
rmSpaceChar (x:xs)   = x : rmSpaceChar xs


getAuthors :: [CiteField] -> People
getAuthors [] = error "No authors found" --FIXME: return a warning
getAuthors ((Author people):_) = people
getAuthors (_:xs) = getAuthors xs

getYear :: [CiteField] -> Integer
getYear [] = error "No year found" --FIXME: return a warning
getYear ((Year year):_) = year
getYear ((Date _ _ year):_) = year
getYear (_:xs) = getYear xs

showBibTeX :: HasSymbolTable s => s -> CiteField -> Spec
showBibTeX _ (Place (city, state)) = showField "place" (city :+: S ", " :+: state)
showBibTeX _ (Edition    s) = showField "edition" (S $ show s ++ sufxer s)
showBibTeX _ (Series     s) = showField "series" s
showBibTeX _ (Title      s) = showField "title" s
showBibTeX _ (Volume     s) = showField "volume" (S $ show s)
showBibTeX _ (Publisher  s) = showField "publisher" s
showBibTeX sm (Author p@(Person {_convention=Mono}:_)) = showField "author" 
  (I.spec (rendPeople p) sm) :+: S ",\n" :+: 
  showField "sortkey" (I.spec (rendPeople p) sm)
showBibTeX sm (Author    p) = showField "author" (I.spec (rendPeople p) sm)
showBibTeX _ (Year       y) = showField "year" (S $ show y)
showBibTeX _ (Date   d m y) = showField "year"    (S $ unwords [show d, show m, show y])
showBibTeX _ (URLdate d m y) = showField "urldate" (S $ unwords [show d, show m, show y])
showBibTeX _ (Collection s) = showField "collection" s
showBibTeX _ (Journal    s) = showField "journal" s
showBibTeX _ (Page       s) = showField "pages" (S $ show s)
showBibTeX _ (Pages (a, b)) = showField "pages" (S $ show a ++ "-" ++ show b)
showBibTeX _ (Note       s) = showField "note" s
showBibTeX _ (Issue      s) = showField "number" (S $ show s)
showBibTeX _ (School     s) = showField "school" s
showBibTeX _ (URL        s) = showField "url" s
showBibTeX _ (HowPub     s) = showField "howpublished" s
showBibTeX sm (Editor     p) = showField "editor" (I.spec (rendPeople p) sm)

showField :: String -> Spec -> Spec
showField f s = S f :+: S "={" :+: s :+: S "}"

rendPeople :: People -> LS.Sentence
rendPeople []  = LS.S "N.a." -- "No authors given"
rendPeople people =
  foldl1 (\x y -> x LS.+:+ LS.S "and" LS.+:+ y) $ map rendPersLFM people
