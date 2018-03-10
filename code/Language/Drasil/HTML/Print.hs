module Language.Drasil.HTML.Print where

import Prelude hiding (print)
import Data.List (intersperse, sort, sortBy)
import Text.PrettyPrint hiding (render, quotes, Str)
import Numeric (showFFloat)

import Language.Drasil.Expr (Oper(..),UFunc(..), BinOp(..))
import Language.Drasil.HTML.Import (makeDocument, spec)
import Language.Drasil.Printing.AST
import Language.Drasil.HTML.AST
import qualified Language.Drasil.Output.Formats as F
import Language.Drasil.Spec (USymb(..), RefType(..), Sentence, sC, (+:+))

import Language.Drasil.HTML.Helpers
import Language.Drasil.Printing.Helpers
import Language.Drasil.Unicode
import Language.Drasil.Symbol (Symbol(..), Decoration(..))
import qualified Language.Drasil.Document as L
import Language.Drasil.HTML.Monad
import Language.Drasil.People (People,Person(..),rendPersLFM',rendPersLFM'',Conv(..),nameStr,rendPersLFM, isInitial)
import Language.Drasil.Config (StyleGuide(..), bibStyleH)
import Language.Drasil.ChunkDB (HasSymbolTable(..))
import Language.Drasil.Space (Space(..))

--FIXME? Use Doc in place of Strings for p_spec/title_spec

-- | Generate an HTML document from a Drasil 'Document'
genHTML :: HasSymbolTable s => F.Filename -> L.Document -> s -> Doc
genHTML fn doc sm = build fn (makeDocument doc sm) sm

-- | Build the HTML Document, called by genHTML
build :: HasSymbolTable s => String -> Document -> s -> Doc
build fn (Document t a c) sm = 
  text ( "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""++
          " \"http://www.w3.org/TR/html4/loose.dtd\">" ++ "\n" ++
          "<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/"++
          "2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>") $$ 
  html (head_tag ((linkCSS fn) $$ (title (text (title_spec t sm)))) $$
  body (article_title (text (p_spec t sm)) $$ author (text (p_spec a sm))
  $$ print c sm
  ))
  
-- | Helper for rendering LayoutObjects into HTML
printLO :: HasSymbolTable s => LayoutObj -> s -> Doc
printLO (HDiv ts layoutObs l)  sm = refwrap (p_spec l sm) $ 
                                   div_tag ts (vcat (map (flip printLO sm) layoutObs))
printLO (Paragraph contents)   sm = paragraph $ text (p_spec contents sm)
printLO (Tagless contents)     sm = text $ p_spec contents sm
printLO (Table ts rows r b t)  sm = makeTable ts rows (p_spec r sm) b (p_spec t sm) sm
printLO (Definition dt ssPs l) sm = makeDefn dt ssPs (p_spec l sm) sm
printLO (Header n contents)    sm = h n $ text (p_spec contents sm)
printLO (List t)               sm = makeList t sm
printLO (Figure r c f wp)      sm = makeFigure (p_spec r sm) (p_spec c sm) f wp
printLO (ALUR _ x l id')       sm = makeRefList (p_spec x sm) (p_spec l sm) (p_spec id' sm)
printLO (Bib bib)              sm = printLO (makeBib sm bib) sm


-- | Called by build, uses 'printLO' to render the layout 
-- objects in Doc format.
print :: HasSymbolTable s => [LayoutObj] -> s -> Doc
print l sm = foldr ($$) empty $ map (flip printLO sm) l

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders the title of the document. Different than body rendering
-- because certain things can't be rendered in an HTML title.
title_spec :: HasSymbolTable s => Spec -> s -> String
title_spec (N s)       _ = t_symbol s
title_spec (a :+: b)  sm = title_spec a sm ++ title_spec b sm
title_spec (a :^: b)  sm = title_spec a sm ++ "^" ++ brace (title_spec b sm)
title_spec (a :-: b)  sm = title_spec a sm ++ "_" ++ title_spec b sm
title_spec (a :/: b)  sm = brace (p_spec a sm) ++ "/" ++ brace (p_spec b sm)
title_spec HARDNL      _ = ""
title_spec s          sm = p_spec s sm

-- | Renders the Sentences in the HTML body (called by 'printLO')
p_spec :: HasSymbolTable s => Spec -> s -> String
p_spec (E e)       _ = p_expr e
p_spec (a :+: b)  sm = p_spec a sm ++ p_spec b sm
p_spec (a :-: b)  sm = p_spec a sm ++ sub (brace (p_spec b sm))
p_spec (a :^: b)  sm = p_spec a sm ++ sup (p_spec b sm)
p_spec (a :/: b)  sm = fraction (p_spec a sm) (p_spec b sm)
p_spec (S s)       _ = s
p_spec (N s)       _ = symbol s
p_spec (Sy s)      _ = uSymb s
p_spec (G g)       _ = unPH $ greek g
p_spec (Sp s)      _ = unPH $ special s
p_spec HARDNL      _ = "<br />"
p_spec (Ref (Def (Just r)) a)    sm = reflink (p_spec a sm) (p_spec (spec r sm) sm)
p_spec (Ref (Assump (Just r)) a) sm = reflink (p_spec a sm) (p_spec (spec r sm) sm)
p_spec (Ref (Req (Just r)) a)    sm = reflink (p_spec a sm) (p_spec (spec r sm) sm)
p_spec (Ref (LC (Just r)) a)     sm = reflink (p_spec a sm) (p_spec (spec r sm) sm)
p_spec (Ref _ a)  sm = reflink (p_spec a sm) (p_spec a sm)
p_spec EmptyS      _ = ""

-- | Renders symbols for HTML title
t_symbol :: Symbol -> String
t_symbol (Corners [] [] [] [x] s) = t_symbol s ++ "_" ++ t_symbol x
t_symbol (Corners [] [] [x] [] s) = t_symbol s ++ "^" ++ t_symbol x
t_symbol s                        = symbol s

-- | Adds emphasis to symbols by default. Use symbolNoEm for no <em>
--   Units do not need this, for example.
symbol :: Symbol -> String
symbol s = em $ symbolNoEm s

-- | Renders symbols for HTML document
symbolNoEm :: Symbol -> String
symbolNoEm (Atomic s)  = s
symbolNoEm (Special s) = unPH $ special s
symbolNoEm (Concat sl) = foldr (++) "" $ map symbolNoEm sl
symbolNoEm (Greek g)   = unPH $ greek g
-- handle the special cases first, then general case
symbolNoEm (Corners [] [] [x] [] s) = (symbolNoEm s) ++ sup (symbolNoEm x)
symbolNoEm (Corners [] [] [] [x] s) = (symbolNoEm s) ++ sub (symbolNoEm x)
symbolNoEm (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbolNoEm (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbolNoEm (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbolNoEm (Atop Vector s)       = "<b>" ++ symbolNoEm s ++ "</b>"
symbolNoEm (Atop Hat s)          = symbolNoEm s ++ "&#770;"
symbolNoEm (Atop Prime s)        = symbolNoEm s ++ "&prime;"
symbolNoEm Empty                 = ""

uSymb :: USymb -> String
uSymb (UName s)           = symbolNoEm s
uSymb (UProd l)           = foldr1 (\x -> ((x++"&sdot;")++) ) (map uSymb l)
uSymb (UPow s i)          = uSymb s ++ sup (show i)
uSymb (UDiv n (UName d))  = uSymb n ++ "/" ++ uSymb (UName d)
uSymb (UDiv n d)          = uSymb n ++ "/(" ++ (uSymb d) ++ ")"

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- | Renders expressions in the HTML (called by multiple functions)
p_expr :: Expr -> String
p_expr (Dbl d)    = showFFloat Nothing d ""
p_expr (Int i)    = show i
p_expr (Str s)    = s
p_expr (Sym s)    = symbol s
p_expr (Assoc Mul l) = mul l
p_expr (Assoc Add l)  = concat $ intersperse " &plus; " $ map p_expr l
p_expr (Assoc And l)  = concat $ intersperse " &and; " $ map p_expr l
p_expr (Assoc Or l)   = concat $ intersperse " &or; " $ map p_expr l
p_expr (BOp Subt a b)  = p_expr a ++ " &minus; " ++ p_expr b
p_expr (BOp Frac a b) = fraction (p_expr a) (p_expr b) --Found in HTMLHelpers
p_expr (BOp Div a b)  = divide a b
p_expr (BOp Pow a b)  = pow a b
p_expr (BOp Eq a b)   = p_expr a ++ " = " ++ p_expr b
p_expr (BOp NEq a b)  = p_expr a ++ "&ne;" ++ p_expr b
p_expr (BOp Lt a b)   = p_expr a ++ "&thinsp;&lt;&thinsp;" ++ p_expr b --thin spaces make these more readable
p_expr (BOp Gt a b)   = p_expr a ++ "&thinsp;&gt;&thinsp;" ++ p_expr b
p_expr (BOp LEq a b)  = p_expr a ++ "&thinsp;&le;&thinsp;" ++ p_expr b
p_expr (BOp GEq a b)  = p_expr a ++ "&thinsp;&ge;&thinsp;" ++ p_expr b
p_expr (BOp Dot a b)  = p_expr a ++ "&sdot;" ++ p_expr b
p_expr (BOp Cross a b) = p_expr a ++ "&#10799;" ++ p_expr b
p_expr (UOp Neg a)    = neg a
p_expr (Funct f e)    = p_op f e
p_expr (Call f x) = p_expr f ++ paren (concat $ intersperse "," $ map p_expr x)
p_expr (Case ps)  = cases ps (p_expr)
p_expr (UOp f es)  = p_uop f es
p_expr (Grouping e) = paren (p_expr e)
p_expr (Mtx a)    = "<table class=\"matrix\">\n" ++ p_matrix a ++ "</table>"
p_expr (BOp Index a i)= p_indx a i
--Logic
p_expr (BOp Impl a b) = p_expr a ++ " &rArr; " ++ p_expr b
p_expr (BOp Iff a b)  = p_expr a ++ " &hArr; " ++ p_expr b
p_expr (IsIn  a b) = p_expr a ++ "&thinsp;&isin;&thinsp;"  ++ p_space b


-- | For printing indexes
p_indx :: Expr -> Expr -> String
p_indx a@(Sym (Corners [] [] [] [_] _)) i = p_expr a ++ sub (","++ p_sub i)
p_indx a i = p_expr a ++ sub (p_sub i)
-- Ensures only simple Expr's get rendered as an index
p_sub :: Expr -> String
p_sub e@(Dbl _)        = p_expr e
p_sub e@(Int _)        = p_expr e
p_sub e@(Sym _)        = p_expr e
p_sub   (Assoc Add l)  = concat $ intersperse "&plus;" $ map p_expr l --removed spaces
p_sub   (BOp Subt a b) = p_expr a ++ "&minus;" ++ p_expr b
p_sub e@(Assoc _ _)    = p_expr e
p_sub   (BOp Frac a b) = divide a b --no block division 
p_sub e@(BOp Div _ _)  = p_expr e
p_sub _                = error "Tried to Index a non-simple expr in HTML, currently not supported."

-- | For printing Matrix
p_matrix :: [[Expr]] -> String
p_matrix [] = ""
p_matrix [x] = "<tr>" ++ p_in x ++ "</tr>\n"
p_matrix (x:xs) = p_matrix [x] ++ p_matrix xs

p_in :: [Expr] -> String
p_in [] = ""
p_in [x] = "<td>" ++ p_expr x ++ "</td>"
p_in (x:xs) = p_in [x] ++ p_in xs

-- | Helper for properly rendering multiplication of expressions
mul :: [ Expr ] -> String
mul = concat . intersperse "&#8239;" . map (add_paren (prec Mul))

-- | Helper for properly rendering parentheses around the multiplier
add_paren :: Int -> Expr -> String
add_paren p a@(Assoc o _)    = if prec o > p then paren $ p_expr a else p_expr a
add_paren _ a@(BOp Div _ _)  = paren $ p_expr a
add_paren _ a@(BOp Subt _ _) = paren $ p_expr a
add_paren _ a                = p_expr a

-- | Helper for properly rendering division of expressions
divide :: Expr -> Expr -> String
divide n d@(Assoc Add _)   = p_expr n ++ "/" ++ paren (p_expr d)
divide n d@(BOp Subt _ _)  = p_expr n ++ "/" ++ paren (p_expr d)
divide n@(Assoc Add _)  d  = paren (p_expr n) ++ "/" ++ p_expr d
divide n@(BOp Subt _ _) d  = paren (p_expr n) ++ "/" ++ p_expr d
divide n d                 = p_expr n ++ "/" ++ p_expr d

-- | Helper for properly rendering negation of expressions
neg :: Expr -> String
neg a@(Dbl     _)     = minus a
neg a@(Int     _)     = minus a
neg a@(Sym     _)     = minus a
neg a@(UOp   _ _)     = minus a
neg a@(Funct _ _)     = minus a
neg a@(Assoc Mul _)   = minus a
neg a@(BOp Index _ _) = minus a
neg a               = "&minus;" ++ paren (p_expr a)

minus :: Expr -> String
minus e = "&minus;" ++ p_expr e

-- | Helper for properly rendering exponents
pow :: Expr -> Expr -> String
pow a@(Assoc Add _)  b = sqbrac (p_expr a) ++ sup (p_expr b)
pow a@(BOp Subt _ _) b = sqbrac (p_expr a) ++ sup (p_expr b)
pow a@(BOp Frac _ _) b = sqbrac (p_expr a) ++ sup (p_expr b)
pow a@(BOp Div _ _)  b = paren (p_expr a) ++ sup (p_expr b)
pow a@(Assoc Mul _)  b = paren (p_expr a) ++ sup (p_expr b)
pow a@(BOp Pow _ _)  b = paren (p_expr a) ++ sup (p_expr b)
pow a                b = p_expr a ++ sup (p_expr b)

p_space :: Space -> String
p_space Integer  = "&#8484;"
p_space Rational = "&#8474;"
p_space Real     = "&#8477;"
p_space Natural  = "&#8469;"
p_space Boolean  = "&#120121;"
p_space Char     = "Char"
p_space String   = "String"
p_space Radians  = "rad"
p_space (Vect a) = "V" ++ p_space a
p_space (DiscreteI a)  = "{" ++ (concat $ intersperse ", " (map show a)) ++ "}"
p_space (DiscreteD a)  = "{" ++ (concat $ intersperse ", " (map show a)) ++ "}"
p_space (DiscreteS a)  = "{" ++ (concat $ intersperse ", " a) ++ "}"
  
-----------------------------------------------------------------
------------------BEGIN TABLE PRINTING---------------------------
-----------------------------------------------------------------

-- | Renders HTML table, called by 'printLO'
makeTable ::HasSymbolTable s => Tags -> [[Spec]] -> String -> Bool -> String -> s -> Doc
makeTable _ [] _ _ _        _ = error "No table to print (see PrintHTML)"
makeTable ts (l:lls) r b t sm = refwrap r (wrap "table" ts (
    tr (makeHeaderCols l sm) $$ makeRows lls sm) $$ if b then caption t else empty)

-- | Helper for creating table rows
makeRows :: HasSymbolTable s => [[Spec]] -> s -> Doc
makeRows []      _ = empty
makeRows (c:cs) sm = tr (makeColumns c sm) $$ makeRows cs sm

makeColumns, makeHeaderCols :: HasSymbolTable s => [Spec] -> s -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols ls sm = vcat $ map (th . text . flip p_spec sm) ls

-- | Helper for creating table columns
makeColumns ls sm = vcat $ map (td . text . flip p_spec sm) ls

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: HasSymbolTable s => L.DType -> [(String,[LayoutObj])] -> String -> s -> Doc
makeDefn _ [] _    _ = error "Empty definition"
makeDefn dt ps l sm = refwrap l $ wrap "table" [dtag dt] (makeDRows ps sm)
  where dtag (L.Data _)   = "ddefn"
        dtag (L.Theory _) = "tdefn"
        dtag (L.General)  = "gdefn"
        dtag (L.Instance) = "idefn"
        dtag (L.TM) = "tdefn"
        dtag (L.DD) = "ddefn"

-- | Helper for making the definition table rows
makeDRows :: HasSymbolTable s => [(String,[LayoutObj])] -> s -> Doc
makeDRows []          _ = error "No fields to create defn table"
makeDRows ((f,d):[]) sm = tr (th (text f) $$ td (vcat $ map (flip printLO sm) d))
makeDRows ((f,d):ps) sm = tr (th (text f) $$ td (vcat $ map (flip printLO sm) d)) $$ makeDRows ps sm

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists
makeList :: HasSymbolTable s => ListType -> s -> Doc
makeList (Simple items) sm = div_tag ["list"] 
  (vcat $ map (\(b,e) -> wrap "p" [] ((text (p_spec b sm ++ ": ") <> (p_item e sm)))) items)
makeList (Desc items)   sm = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((wrap "b" [] (text (p_spec b sm ++ ": "))
   <> (p_item e sm)))) items)
makeList (Ordered items) sm = wrap "ol" ["list"] (vcat $ map
  (wrap "li" [] . flip p_item sm) items)
makeList (Unordered items) sm = wrap "ul" ["list"] (vcat $ map
  (wrap "li" [] . flip p_item sm) items)
makeList (Definitions items) sm = div_tag ["list"] 
  (vcat $ map (\(b,e) -> wrap "p" [] ((text (p_spec b sm ++ " is the") <+> 
  (p_item e sm)))) items)

-- | Helper for rendering list items
p_item :: HasSymbolTable s => ItemType -> s -> Doc  
p_item (Flat s)     sm = text $ p_spec s sm
p_item (Nested s l) sm = vcat [text (p_spec s sm),makeList l sm]
  
-----------------------------------------------------------------
------------------BEGIN FIGURE PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders figures in HTML
makeFigure :: String -> String -> String -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refwrap r (image f c wp $$ caption c)

-----------------------------------------------------------------
------------------BEGIN EXPR OP PRINTING-------------------------
-----------------------------------------------------------------
-- | Renders expression operations/functions. 
p_op :: Functional -> Expr -> String
p_op (Summation bs) x = lrgOp "&sum;" bs ++ paren (p_expr x)
p_op (Product bs) x = lrgOp "&prod;" bs ++ paren (p_expr x)
p_op (Integral bs wrtc) x = intg bs
  ++ paren (p_expr x ++ (symbol (Atomic "d") ++ "&#8239;" ++ symbol wrtc))

p_uop :: UFunc -> Expr -> String
p_uop Abs x = "|" ++ p_expr x ++ "|"
p_uop Norm x = "||" ++ p_expr x ++ "||"
p_uop Not a    = "&not;" ++ p_expr a
p_uop f@(Exp) x = function f ++ sup (p_expr x)
p_uop f x = function f ++ paren (p_expr x) --Unary ops, this will change once more complicated functions appear.


-- | Helpers for summation/product, used by 'p_op'
makeBound :: String -> String
makeBound s = "<tr><td><span class=\"bound\">" ++ s ++ "</span></td></tr>\n"

lrgOp :: String -> Maybe ((Symbol, Expr),Expr) -> String
lrgOp f Nothing = "<span class=\"symb\">" ++ f ++ "</span>"
lrgOp f (Just ((s,v),hi)) = "<table class=\"operator\">\n" ++ makeBound (p_expr hi) ++
  "<tr><td><span class=\"symb\">" ++ f ++ "</span></td></tr>\n" ++
  makeBound (symbol s ++"="++ p_expr v) ++ "</table>"

intg :: (Maybe Expr, Maybe Expr) -> String
intg (Nothing, Nothing) = "<span class=\"symb\">&int;</span>"
intg (Just l, Nothing) = "<span class=\"symb\">&int;</span>" ++ sub (p_expr l ++ " ")
intg (low,high) = "<table class=\"operator\">\n" ++ pHigh high ++
  "<tr><td><span class=\"symb\">&int;</span></td></tr>\n" ++
  pLow low ++ "</table>"
  where pLow Nothing   = ""
        pLow (Just l)  = makeBound (p_expr l)
        pHigh Nothing  = ""
        pHigh (Just hi) = makeBound (p_expr hi)

-- | Helper for integration bound creation, used by 'p_op'
makeIBound :: (Maybe Expr, Maybe Expr) -> String
makeIBound (Just low, Just high) = sub (p_expr low) ++ sup (p_expr high)
makeIBound (Just low, Nothing)   = sub (p_expr low)
makeIBound (Nothing, Just high)  = sup (p_expr high)
makeIBound (Nothing, Nothing)    = ""

function :: UFunc -> String
function Log            = "log"
function Abs            = ""
function Norm           = ""
function Sin            = "sin"
function Cos            = "cos"
function Tan            = "tan"
function Sec            = "sec"
function Csc            = "csc"
function Cot            = "cot"
function Exp            = "e"
function Sqrt           = "&radic;"
function Not            = "&not;"
function Neg            = "-" -- but usually not reached...
function Dim            = "dim" -- hmmm
  
-- | Renders modules
makeModule :: String -> String -> Doc
makeModule m l = refwrap l (paragraph $ wrap "b" [] (text m))

-- | Renders assumptions, requirements, likely changes
makeRefList :: String -> String -> String -> Doc
makeRefList a l i = refwrap l (wrap "ul" [] (text $ i ++ ": " ++ a))

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**
makeBib :: HasSymbolTable s => s -> BibRef -> LayoutObj
makeBib sm = listRef . map (Flat . S) . sort . map (flip renderCite sm)
  where listRef = List . Simple . zip [S $ sqbrac $ show x | x <- [(1 :: Integer)..]]
  --some function to get a numbered list, idealy it wouldn't go from string to Spec
  
--for when we add other things to reference like website, newspaper
renderCite :: HasSymbolTable s => Citation -> s -> String
renderCite a@(Book      fields) sm = renderF a fields useStyleBk    sm
renderCite a@(Article   fields) sm = renderF a fields useStyleArtcl sm
renderCite a@(MThesis   fields) sm = renderF a fields useStyleBk    sm
renderCite a@(PhDThesis fields) sm = renderF a fields useStyleBk    sm
renderCite a@(Misc      fields) sm = renderF a fields useStyleBk    sm
renderCite a@(Online    fields) sm = renderF a fields useStyleArtcl sm --rendered similar to articles for some reason

compCiteField :: CiteField -> CiteField -> Ordering
compCiteField (Author     _) _ = LT
compCiteField _ (Author     _) = GT
compCiteField (Title      _) _ = LT
compCiteField _ (Title      _) = GT
compCiteField (Series     _) _ = LT
compCiteField _ (Series     _) = GT
compCiteField (Collection _) _ = LT
compCiteField _ (Collection _) = GT
compCiteField (Editor     _) _ = LT
compCiteField _ (Editor     _) = GT
compCiteField (Journal    _) _ = LT
compCiteField _ (Journal    _) = GT
compCiteField (Volume     _) _ = LT
compCiteField _ (Volume     _) = GT
compCiteField (Issue      _) _ = LT
compCiteField _ (Issue      _) = GT
compCiteField (Edition    _) _ = LT
compCiteField _ (Edition    _) = GT
compCiteField (HowPub     _) _ = LT
compCiteField _ (HowPub     _) = GT
compCiteField (School     _) _ = LT
compCiteField _ (School     _) = GT
compCiteField (Place      _) _ = LT
compCiteField _ (Place      _) = GT
compCiteField (Publisher  _) _ = LT
compCiteField _ (Publisher  _) = GT
compCiteField (Date   _ _ _) _ = LT
compCiteField _ (Date   _ _ _) = GT
compCiteField (Year       _) _ = LT
compCiteField _ (Year       _) = GT
compCiteField (URL       _) _  = LT
compCiteField _ (URL       _)  = GT
compCiteField (Page       _) _ = LT
compCiteField _ (Page       _) = GT
compCiteField (Pages      _) _ = LT
compCiteField _ (Pages      _) = GT
compCiteField (URLdate _ _ _) _ = LT
compCiteField _ (URLdate _ _ _) = GT
compCiteField (Note       _) _ = LT

renderF :: HasSymbolTable s => Citation -> [CiteField] -> (StyleGuide -> (CiteField -> s -> String)) -> s -> String
renderF c fields styl sm = unwords $
  map (flip (styl bibStyleH) sm) (sortBy compCiteField fields) ++ endingField c bibStyleH

endingField :: Citation -> StyleGuide -> [String]
endingField (Book      _) MLA = ["Print."]
endingField (Article   _) MLA = ["Print."]
endingField (MThesis   _) MLA = ["Print."]
endingField (PhDThesis _) MLA = ["Print."]
endingField (Misc      _) MLA = [""]
endingField (Online    _) MLA = [""]
endingField _             _   = []

-- Config helpers --
useStyleBk :: HasSymbolTable s => StyleGuide -> (CiteField -> s -> String)
useStyleBk MLA     = bookMLA
useStyleBk APA     = bookAPA
useStyleBk Chicago = bookChicago

useStyleArtcl :: HasSymbolTable s => StyleGuide -> (CiteField -> s -> String)
useStyleArtcl MLA     = artclMLA
useStyleArtcl APA     = artclAPA
useStyleArtcl Chicago = artclChicago

-- FIXME: move these show functions and use tags, combinators
bookMLA :: HasSymbolTable s => CiteField -> s -> String
bookMLA (Place (city, state)) sm = p_spec (city :+: S ", " :+: state) sm ++ ":"
bookMLA (Edition    s)  _  = comm $ show s ++ sufxer s ++ " ed."
bookMLA (Series     s) sm  = dot $ em $ p_spec s sm
bookMLA (Title      s) sm  = dot $ em $ p_spec s sm --If there is a series or collection, this should be in quotes, not italics
bookMLA (Volume     s)  _  = comm $ "vol. " ++ show s
bookMLA (Publisher  s) sm  = comm $ p_spec s sm
bookMLA (Author     p) sm  = dot $ p_spec (rendPeople' sm p) sm
bookMLA (Year       y)  _  = dot $ show y
bookMLA (Date    d m y)  _ = dot $ unwords [show d, show m, show y]
bookMLA (URLdate d m y) sm = "Web. " ++ bookMLA (Date d m y) sm
bookMLA (Collection s) sm  = dot $ em $ p_spec s sm
bookMLA (Journal    s) sm  = comm $ em $ p_spec s sm
bookMLA (Page       n)  _  = dot $ "p. " ++ show n
bookMLA (Pages  (a,b))  _  = dot $ "pp. " ++ show a ++ "&ndash;" ++ show b
bookMLA (Note       s) sm  = p_spec s sm
bookMLA (Issue      n)  _  = comm $ "no. " ++ show n
bookMLA (School     s) sm  = comm $ p_spec s sm
bookMLA (URL        s) sm  = dot $ p_spec s sm
bookMLA (HowPub     s) sm  = comm $ p_spec s sm
bookMLA (Editor     p) sm  = comm $ "Edited by " ++ p_spec (foldlList (map (flip spec sm . nameStr) p)) sm

bookAPA :: HasSymbolTable s => CiteField -> s -> String --FIXME: year needs to come after author in APA
bookAPA (Author   p) sm = needDot $ p_spec (rendPeople sm rendPersLFM' p) sm --APA uses initals rather than full name
bookAPA (Year     y)  _ = dot $ paren $ show y --APA puts "()" around the year
bookAPA (Date _ _ y) sm = bookAPA (Year y) sm --APA doesn't care about the day or month
bookAPA (URLdate d m y)  _ = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA (Page     n)   _ = dot $ show n
bookAPA (Pages (a,b))  _ = dot $ show a ++ "&ndash;" ++ show b
bookAPA (Editor   p)  sm = dot $ p_spec (foldlList $ map ( flip spec sm . nameStr) p) sm ++ " (Ed.)"
bookAPA i sm = bookMLA i sm --Most items are rendered the same as MLA

bookChicago :: HasSymbolTable s => CiteField -> s -> String
bookChicago (Author   p) sm = needDot $ p_spec (rendPeople sm rendPersLFM'' p) sm --APA uses middle initals rather than full name
bookChicago (Date _ _ y) sm = bookChicago (Year y) sm --APA doesn't care about the day or month
bookChicago (URLdate d m y)  _ = "accessed " ++ (comm $ unwords [show d, show m, show y])
bookChicago p@(Page   _) sm = bookAPA p sm
bookChicago p@(Pages  _) sm = bookAPA p sm
bookChicago (Editor   p) sm = dot $ p_spec (foldlList $ map (flip spec sm . nameStr) p) sm ++ toPlural p " ed"
bookChicago i sm = bookMLA i sm--Most items are rendered the same as MLA

-- for article renderings
artclMLA :: HasSymbolTable s => CiteField -> s -> String
artclMLA (Title s) = quotes . dot . p_spec s
artclMLA i = bookMLA i

artclAPA :: HasSymbolTable s => CiteField -> s -> String
artclAPA (Title  s) sm = dot $ p_spec s sm
artclAPA (Volume n)  _ = em $ show n
artclAPA (Issue  n)  _ = comm $ paren $ show n
artclAPA i sm = bookAPA i sm

artclChicago :: HasSymbolTable s => CiteField -> s -> String
artclChicago i@(Title    _) = artclMLA i
artclChicago (Volume     n) = \_ -> comm $ show n
artclChicago (Issue      n) = \_ -> "no. " ++ show n
artclChicago i@(Year     _) = bookAPA i
artclChicago i@(Date _ _ _) = bookAPA i
artclChicago i = bookChicago i

-- PEOPLE RENDERING --

rendPeople :: HasSymbolTable s => s -> (Person -> Sentence) -> People -> Spec
rendPeople _ _ []  = S "N.a." -- "No authors given"
rendPeople sm f people = foldlList $ map (flip spec sm . f) people --foldlList is in SentenceStructures.hs

rendPeople' :: HasSymbolTable s => s -> People -> Spec
rendPeople' _ []  = S "N.a." -- "No authors given"
rendPeople' sm people = foldlList $ map (flip spec sm . rendPers) (init people) ++  [spec (rendPersL $ last people) sm] 

foldlList :: [Spec] -> Spec
foldlList []    = EmptyS
foldlList [a,b] = a :+: S " and " :+: b
foldlList lst   = foldle1 (\a b -> a :+: S ", " :+: b) (\a b -> a :+: S ", and " :+: b) lst

foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g ((f x y):xs)

needDot :: String -> String
needDot str = dotIt $ last str
  where dotIt '.' = str
        dotIt _   = dot str
-- LFM is Last, First Middle
rendPers :: Person -> Sentence
rendPers = rendPersLFM

-- To render the last person's name
rendPersL :: Person -> Sentence
rendPersL (Person {_surname = n, _convention = Mono}) = n
rendPersL (Person {_given = f, _surname = l, _middle = []}) =
  isInitial l `sC` isInitial f
rendPersL (Person {_given = f, _surname = l, _middle = ms}) =
  isInitial l `sC` foldr1 (+:+) ([isInitial f] ++ map (isInitial) (init ms) ++ [last ms])

--adds an 's' if there is more than one person in a list
toPlural :: People -> String -> String
toPlural (_:_) str = str ++ "s"
toPlural _     str = str
