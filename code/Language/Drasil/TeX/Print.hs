module Language.Drasil.TeX.Print where

import Prelude hiding (print)
import Data.List (intersperse)
import Text.PrettyPrint (text, (<+>))
import qualified Text.PrettyPrint as TP
import Data.Maybe (isNothing, fromJust)

import Control.Applicative (pure)

import Language.Drasil.TeX.AST
import qualified Language.Drasil.TeX.Import as I
import qualified Language.Drasil.Output.Formats as A
import Language.Drasil.Spec (USymb(..), RefType(..))
import Language.Drasil.Config (lpmTeXParams, colAwidth, colBwidth,
              LPMParams(..))
import Language.Drasil.Printing.Helpers
import Language.Drasil.TeX.Helpers
import Language.Drasil.TeX.Monad
import Language.Drasil.TeX.Preamble
import Language.Drasil.Symbol (Symbol(..),Decoration(..))
import qualified Language.Drasil.Document as L
import Language.Drasil.Unicode (RenderGreek(..), RenderSpecial(..))

genTeX :: A.DocType -> L.Document -> TP.Doc
genTeX typ doc = runPrint (build typ $ I.makeDocument doc) Text

build :: A.DocType -> Document -> D
build (A.SRS _) doc   = buildStd doc
build (A.MG _) doc    = buildStd doc
build (A.MIS _) doc   = buildStd doc
build (A.LPM _) doc   = buildLPM lpmTeXParams doc
build (A.Website _) _ = error "Cannot use TeX to typeset Website" --Can't happen

buildStd :: Document -> D
buildStd (Document t a c) =
  genPreamble c %%
  title (spec t) %%
  author (spec a) %%
  document (maketitle %% maketoc %% newpage %% print c)

buildLPM :: LPMParams -> Document -> D
buildLPM  (LPMParams (A.DocClass sb b1) (A.UsePackages ps) (A.ExDoc f n))
          (Document t a c) =
  docclass sb b1 %%
  listpackages ps %%
  exdoc f n %%
  title (spec t) %%
  author (spec a) %%
  document (maketitle %% print c)

listpackages :: [String] -> D
listpackages lp = foldr (%%) empty $ map usepackage lp

-- clean until here; lo needs its sub-functions fixed first though
lo :: LayoutObj -> D
lo (Section d t con l)     = sec d (spec t) %% label (spec l) %% print con
lo (Paragraph contents)    = toText $ spec contents
lo (EqnBlock contents)     = makeEquation contents
lo (Table rows r bl t)     = toText $ makeTable rows (spec r) bl (spec t)
--lo (CodeBlock c)           = code $ pure $ printCode c
lo (Definition ssPs l)     = toText $ makeDefn ssPs $ spec l
lo (List l)                = toText $ makeList l
lo (Figure r c f)          = toText $ makeFigure (spec r) (spec c) f
lo (Module n l)            = toText $ makeModule n $ spec l
lo (Requirement n l)       = toText $ makeReq (spec n) (spec l)
lo (Assumption n l)        = toText $ makeAssump (spec n) (spec l)
lo (LikelyChange n l)      = toText $ makeLC (spec n) (spec l)
lo (UnlikelyChange n l)    = toText $ makeUC (spec n) (spec l)
lo (Graph ps w h c l)      = toText $ makeGraph
                               (map (\(a,b) -> (spec a, spec b)) ps)
                               (if isNothing w
                                  then (pure $ text "")
                                  else (pure $ text $ "text width = " ++
                                         (show $ fromJust w) ++ "em, ")
                               )
                               (if isNothing w
                                  then (pure $ text "")
                                  else (pure $ text $ "minimum height = " ++
                                         (show $ fromJust h) ++ "em, ")
                               )
                               (spec c) (spec l)


print :: [LayoutObj] -> D
print l = foldr ($+$) empty $ map lo l

------------------ Symbol ----------------------------
symbol :: Symbol -> String
symbol (Atomic s)  = s
symbol (Special s) = unPL $ special s
symbol (Greek g)   = unPL $ greek g
symbol (Concat sl) = foldr (++) "" $ map symbol sl
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = (symbol s) ++"^"++ brace (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) ++"_"++ brace (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (Atop f s) = sFormat f s

sFormat :: Decoration -> Symbol -> String
sFormat Hat    s = "\\hat{" ++ symbol s ++ "}"
sFormat Vector s = "\\mathbf{" ++ symbol s ++ "}"

-----------------------------------------------------------------
------------------ EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- (Since this is all implicitly in Math, leave it as String for now)
p_expr :: Expr -> String
p_expr (Var v)    = v
p_expr (Dbl d)    = show d
p_expr (Int i)    = show i
p_expr (Add x y)  = p_expr x ++ "+" ++ p_expr y
p_expr (Sub x y)  = p_expr x ++ "-" ++ p_expr y
p_expr (Mul x y)  = mul x y
p_expr (Frac n d) = "\\frac{" ++ (p_expr n) ++ "}{" ++ (p_expr d) ++"}"
p_expr (Div n d)  = divide n d
p_expr (Pow x y)  = p_expr x ++ "^" ++ brace (p_expr y)
p_expr (Sym s)    = symbol s
p_expr (Eq x y)   = p_expr x ++ "=" ++ p_expr y
p_expr (Lt x y)   = p_expr x ++ "<" ++ p_expr y
p_expr (Gt x y)   = p_expr x ++ ">" ++ p_expr y
p_expr (Dot x y)  = p_expr x ++ "\\cdot{}" ++ p_expr y
p_expr (Neg x)    = neg x
p_expr (Call f x) = p_expr f ++ paren (concat $ intersperse "," $ map p_expr x)
p_expr (Case ps)  = "\\begin{cases}\n" ++ cases ps ++ "\n\\end{cases}"
p_expr (Op f es)  = p_op f es
p_expr (Grouping x) = paren (p_expr x)

mul :: Expr -> Expr -> String
mul x@(Add _ _) y = paren (p_expr x) ++ p_expr y
mul x@(Sub _ _) y = paren (p_expr x) ++ p_expr y
mul x y@(Dbl _)   = p_expr x ++ "*" ++ p_expr y
mul x y@(Int _)   = p_expr x ++ "*" ++ p_expr y
mul x y@(Add _ _) = p_expr x ++ paren (p_expr y)
mul x y@(Sub _ _) = p_expr x ++ paren (p_expr y)
mul x@(Sym (Concat _)) y = p_expr x ++ "*" ++ p_expr y
mul x y@(Sym (Concat _)) = p_expr x ++ "*" ++ p_expr y
mul x@(Sym (Atomic s)) y = if length s > 1 then p_expr x ++ "*" ++ p_expr y else
                            p_expr x ++ p_expr y
mul x y@(Sym (Atomic s)) = if length s > 1 then p_expr x ++ "*" ++ p_expr y else
                            p_expr x ++ p_expr y
mul x y           = p_expr x ++ p_expr y

divide :: Expr -> Expr -> String
divide n d@(Add _ _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n d@(Sub _ _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n@(Add _ _) d = p_expr n ++ "/" ++ paren (p_expr d)
divide n@(Sub _ _) d = p_expr n ++ "/" ++ paren (p_expr d)
divide n d = p_expr n ++ "/" ++ p_expr d

neg :: Expr -> String
neg x@(Var _) = "-" ++ p_expr x
neg x@(Dbl _) = "-" ++ p_expr x
neg x@(Int _) = "-" ++ p_expr x
neg x@(Sym _) = "-" ++ p_expr x
neg   (Neg n) = p_expr n
neg x         = paren ("-" ++ p_expr x)

cases :: [(Expr,Expr)] -> String
cases []     = error "Attempt to create case expression without cases"
cases (p:[]) = p_expr (fst p) ++ ", & " ++ p_expr (snd p)
cases (p:ps) = p_expr (fst p) ++ ", & " ++ p_expr (snd p) ++ "\\\\\n" ++ cases ps
-----------------------------------------------------------------
------------------ TABLE PRINTING---------------------------
-----------------------------------------------------------------

makeTable :: [[Spec]] -> D -> Bool -> D -> D
makeTable lls r bool t =
  pure (text (("\\begin{" ++ ltab ++ "}") ++ brace (header lls)))
  %% (pure (text "\\toprule"))
  %% makeRows [head lls]
  %% (pure (text "\\midrule"))
  %% makeRows (tail lls)
  %% (pure (text "\\bottomrule"))
  %% (if bool then caption t else empty)
  %% label r
  %% (pure $ text ("\\end{" ++ ltab ++ "}"))
  where header l = concat (replicate ((length (head l))-1) "l ") ++ "l"
--                    ++ "p" ++ brace (show tableWidth ++ "cm")
        ltab = "longtable" ++ (if not bool then "*" else "")

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
spec (S s)       = pure $ text s
spec (N s)       = toMath $ pure $ text $ symbol s
spec (Sy s)      = p_unit s
spec (G g)       = pure $ text $ unPL $ greek g
spec (Sp s)      = pure $ text $ unPL $ special s
spec HARDNL      = pure $ text $ "\\newline"
spec (Ref t@Sect r) = sref (show t) (spec r)
spec (Ref t@Def r) = hyperref (show t) (spec r)
spec (Ref t@Mod r) = mref (show t) (spec r)
spec (Ref t@Req r) = rref (show t) (spec r)
spec (Ref t@Assump r) = aref (show t) (spec r)
spec (Ref t@LC r) = lcref (show t) (spec r)
spec (Ref t@UC r) = ucref (show t) (spec r)
spec (Ref t r)   = ref (show t) (spec r)
spec EmptyS      = empty

symbol_needs :: Symbol -> MathContext
symbol_needs (Atomic _)          = Text
symbol_needs (Special _)         = Math
symbol_needs (Greek _)           = Math
symbol_needs (Concat [])         = Math
symbol_needs (Concat (s:_))      = symbol_needs s
symbol_needs (Corners _ _ _ _ _) = Math
symbol_needs (Atop _ _)          = Math

p_unit :: USymb -> D
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

-----------------------------------------------------------------
------------------ DATA DEFINITION PRINTING-----------------
-----------------------------------------------------------------

makeDefn :: [(String,LayoutObj)] -> D -> D
makeDefn [] _ = error "Empty definition"
makeDefn ps l = beginDefn %% makeDefTable ps l %% endDefn

beginDefn :: D
beginDefn = (pure $ text "~") <> newline
  %% (pure $ text "\\noindent \\begin{minipage}{\\textwidth}")

endDefn :: D
endDefn = pure $ text "\\end{minipage}" TP.<> dbs

makeDefTable :: [(String,LayoutObj)] -> D -> D
makeDefTable [] _ = error "Trying to make empty Data Defn"
makeDefTable ps l = vcat [
  pure $ text $ "\\begin{tabular}{p{"++show colAwidth++"\\textwidth} p{"++show colBwidth++"\\textwidth}}",
  (pure $ text "\\toprule \\textbf{Refname} & \\textbf{") <> l <> (pure $ text "}"),
  (pure $ text "\\phantomsection "), label l,
  makeDRows ps,
  pure $ dbs <+> text ("\\bottomrule \\end{tabular}")
  ]

makeDRows :: [(String,LayoutObj)] -> D
makeDRows []         = error "No fields to create Defn table"
makeDRows ((f,d):[]) = dBoilerplate %% (pure $ text (f ++ " & ")) <> lo d
makeDRows ((f,d):ps) = dBoilerplate %% (pure $ text (f ++ " & ")) <> lo d
                       %% makeDRows ps
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

p_item :: ItemType -> D
p_item (Flat s) = item (spec s)
p_item (Nested t s) = vcat [item (spec t), makeList s]

sim_item :: [(Spec,ItemType)] -> [D]
sim_item [] = [empty]
sim_item ((x,y):zs) = item' (spec (x :+: S ":")) (sp_item y) : sim_item zs
    where sp_item (Flat s) = spec s
          sp_item (Nested t s) = vcat [spec t, makeList s]

-----------------------------------------------------------------
------------------ FIGURE PRINTING--------------------------
-----------------------------------------------------------------

makeFigure :: D -> D -> String -> D
makeFigure r c f =
  figure (center (
  vcat [
    includegraphics f,
    caption c,
    label r
  ] ) )

-----------------------------------------------------------------
------------------ EXPR OP PRINTING-------------------------
-----------------------------------------------------------------
p_op :: Function -> [Expr] -> String
p_op f@(Cross) xs = binfix_op f xs
p_op f@(Summation bs) (x:[]) = show f ++ makeBound bs ++ brace (p_expr x)
p_op (Summation _) _ = error "Something went wrong with a summation"
p_op f@(Product bs) (x:[]) = show f ++ makeBound bs ++ brace (p_expr x)
p_op f@(Integral bs wrtc) (x:[]) = show f ++ makeIBound bs ++ 
  brace (p_expr x ++ p_expr wrtc)
p_op (Integral _ _) _  = error "Something went wrong with an integral"
p_op Abs (x:[]) = "|" ++ p_expr x ++ "|"
p_op Abs _ = error "Abs should only take one expr."
p_op Norm (x:[]) = "||" ++ p_expr x ++ "||"
p_op Norm _ = error "Norm should only take on expression."
p_op f@(Exp) (x:[]) = show f ++ "^" ++ brace (p_expr x)
p_op f (x:[]) = show f ++ paren (p_expr x) --Unary ops, this will change once more complicated functions appear.
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
binfix_op f (x:y:[]) = p_expr x ++ show f ++ p_expr y
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
