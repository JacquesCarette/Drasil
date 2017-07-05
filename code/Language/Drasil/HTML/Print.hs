module Language.Drasil.HTML.Print where

import Prelude hiding (print)
import Data.List (intersperse)
import Text.PrettyPrint hiding (render)
import Numeric (showFFloat)

import Language.Drasil.HTML.Import (makeDocument)
import Language.Drasil.HTML.AST
import Language.Drasil.Output.Formats (DocType(..))
import Language.Drasil.Spec (USymb(..))

import Language.Drasil.HTML.Helpers
import Language.Drasil.Printing.Helpers
import Language.Drasil.Unicode
import Language.Drasil.Symbol (Symbol(..), Decoration(..))
import qualified Language.Drasil.Document as L
import Language.Drasil.HTML.Monad

--FIXME? Use Doc in place of Strings for p_spec/title_spec

-- | Generate an HTML document from a Drasil 'Document'
genHTML :: DocType -> L.Document -> Doc
genHTML (Website fn) doc = build fn $ makeDocument doc
genHTML _ _ = error "Cannot generate HTML for non-Website doctype"

-- | Build the HTML Document, called by genHTML
build :: String -> Document -> Doc
build fn (Document t a c) = 
  text ( "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""++
          " \"http://www.w3.org/TR/html4/loose.dtd\">" ++ "\n" ++
          "<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/"++
          "2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>") $$ 
  html (head_tag ((linkCSS fn) $$ (title (text (title_spec t)))) $$
  body (article_title (text (p_spec t)) $$ author (text (p_spec a))
  $$ print c
  ))
  
-- | Helper for rendering LayoutObjects into HTML
printLO :: LayoutObj -> Doc
printLO (HDiv ts layoutObs l)   = refwrap (p_spec l) $ 
                                  div_tag ts (vcat (map printLO layoutObs))
printLO (Paragraph contents)    = paragraph $ text (p_spec contents)
printLO (Tagless contents)      = text $ p_spec contents
printLO (Table ts rows r b t)   = makeTable ts rows (p_spec r) b (p_spec t)
--printLO (CodeBlock c)           = code $ printCode c
printLO (Definition dt ssPs l)  = makeDefn dt ssPs (p_spec l)
printLO (Header n contents)     = h n $ text (p_spec contents)
printLO (List t)                = makeList t
printLO (Figure r c f)          = makeFigure (p_spec r) (p_spec c) f
printLO (Module m l)            = makeModule m (p_spec l)
printLO (Assumption a l)        = makeAssump (p_spec a) (p_spec l)


-- | Called by build, uses 'printLO' to render the layout 
-- objects in Doc format.
print :: [LayoutObj] -> Doc
print l = foldr ($$) empty $ map printLO l

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders the title of the document. Different than body rendering
-- because certain things can't be rendered in an HTML title.
title_spec :: Spec -> String
title_spec (N s)      = t_symbol s
title_spec (a :+: b)  = title_spec a ++ title_spec b
title_spec (a :^: b)  = title_spec a ++ "^" ++ brace (title_spec b)
title_spec (a :-: b)  = title_spec a ++ "_" ++ title_spec b
title_spec (a :/: b)  = brace (p_spec a) ++ "/" ++ brace (p_spec b)
title_spec HARDNL     = ""
title_spec s          = p_spec s

-- | Renders the Sentences in the HTML body (called by 'printLO')
p_spec :: Spec -> String
p_spec (E e)      = p_expr e
p_spec (a :+: b)  = p_spec a ++ p_spec b
p_spec (a :-: b)  = p_spec a ++ sub (brace (p_spec b))
p_spec (a :^: b)  = p_spec a ++ sup (p_spec b)
p_spec (a :/: b)  = fraction (p_spec a) (p_spec b)
p_spec (S s)      = s
p_spec (N s)      = symbol s
p_spec (Sy s)     = uSymb s
p_spec (G g)      = unPH $ greek g
p_spec (Sp s)     = unPH $ special s
p_spec HARDNL     = "<br />"
p_spec (Ref r a)  = reflink (p_spec a) ("this " ++ show r)
p_spec EmptyS     = ""

-- | Renders symbols for HTML title
t_symbol :: Symbol -> String
t_symbol (Corners [] [] [] [x] s) = t_symbol s ++ "_" ++ t_symbol x
t_symbol (Corners [] [] [x] [] s) = t_symbol s ++ "^" ++ t_symbol x
t_symbol s                        = symbol s

-- | Renders symbols for HTML body
symbol :: Symbol -> String
symbol (Atomic s)  = s
symbol (Special s) = unPH $ special s
symbol (Concat sl) = foldr (++) "" $ map symbol sl
symbol (Greek g)   = unPH $ greek g
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = (symbol s) ++ sup (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) ++ sub (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (Atop Vector s)       = "<b>" ++ symbol s ++ "</b>"
symbol (Atop Hat s)          = symbol s ++ "&#770;"
symbol (Atop Prime s)        = symbol s ++ "'"

uSymb :: USymb -> String
uSymb (UName s)           = symbol s
uSymb (UProd l)           = foldr1 (\x -> (x++)) (map uSymb l)
uSymb (UPow s i)          = uSymb s ++ sup (show i)
uSymb (UDiv n (UName d))  = uSymb n ++ "/" ++ uSymb (UName d)
uSymb (UDiv n d)          = uSymb n ++ "/(" ++ (uSymb d) ++ ")"

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- | Renders expressions in the HTML (called by multiple functions)
p_expr :: Expr -> String
p_expr (Var v)    = v
p_expr (Dbl d)    = showFFloat Nothing d ""
p_expr (Int i)    = show i
p_expr (Bln b)    = show b
p_expr (Mul a b)  = mul a b
p_expr (Add a b)  = p_expr a ++ "+" ++ p_expr b
p_expr (Sub a b)  = p_expr a ++ "-" ++ p_expr b
p_expr (Frac a b) = fraction (p_expr a) (p_expr b) --Found in HTMLHelpers
p_expr (Div a b)  = divide a b
p_expr (Pow a b)  = pow a b
p_expr (Sym s)    = symbol s
p_expr (Eq a b)   = p_expr a ++ "=" ++ p_expr b
p_expr (NEq a b)  = p_expr a ++ "&ne;" ++ p_expr b
p_expr (Lt a b)   = p_expr a ++ "&lt;" ++ p_expr b
p_expr (Gt a b)   = p_expr a ++ "&gt;" ++ p_expr b
p_expr (LEq a b)  = p_expr a ++ "&le;" ++ p_expr b
p_expr (GEq a b)  = p_expr a ++ "&ge;" ++ p_expr b
p_expr (Dot a b)  = p_expr a ++ "&sdot;" ++ p_expr b
p_expr (Neg a)    = neg a
p_expr (Call f x) = p_expr f ++ paren (concat $ intersperse "," $ map p_expr x)
p_expr (Case ps)  = cases ps (p_expr)
p_expr (Op f es)  = p_op f es
p_expr (Grouping e) = paren (p_expr e)
--Logic
p_expr (Not a)    = "&not;" ++ p_expr a
p_expr (And a b)  = p_expr a ++ "&and;" ++ p_expr b
p_expr (Or a b)   = p_expr a ++ "&or;" ++ p_expr b
p_expr (Impl a b) = p_expr a ++ " &rArr; " ++ p_expr b
p_expr (Iff a b)  = p_expr a ++ " &hArr; " ++ p_expr b
p_expr (IsIn  a b) = (concat $ intersperse "," $ map p_expr a) ++ "&isin;"  ++ show b
p_expr (NotIn a b) = (concat $ intersperse "," $ map p_expr a) ++ "&notin;" ++ show b
p_expr (State a b) = (concat $ intersperse ", " $ map p_quan a) ++ ": " ++ p_expr b

-- | Helper for rendering Quantifier statements
p_quan :: Quantifier -> String
p_quan (Forall e) = "&forall;" ++ p_expr e
p_quan (Exists e) = "&exist;"  ++ p_expr e

-- | Helper for properly rendering multiplication of expressions
mul :: Expr -> Expr -> String
mul a b@(Dbl _) = mulParen a ++ "*" ++ p_expr b
mul a b@(Int _) = mulParen a ++ "*" ++ p_expr b
mul x@(Sym (Concat _)) y = p_expr x ++ "*" ++ mulParen y
mul x y@(Sym (Concat _)) = mulParen x ++ "*" ++ p_expr y
mul x@(Sym (Atomic s)) y = if length s > 1 then p_expr x ++ "*" ++ mulParen y else
                            p_expr x ++ mulParen y
mul x y@(Sym (Atomic s)) = if length s > 1 then mulParen x ++ "*" ++ p_expr y else
                            mulParen x ++ p_expr y
mul a b         = mulParen a ++ mulParen b

-- | Helper for properly rendering parentheses around multiplication
mulParen :: Expr -> String
mulParen a@(Add _ _) = paren $ p_expr a
mulParen a@(Sub _ _) = paren $ p_expr a
--mulParen (Mul n m) = mulParen n ++ mulParen m
mulParen a = p_expr a

-- | Helper for properly rendering division of expressions
divide :: Expr -> Expr -> String
divide n d@(Add _ _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n d@(Sub _ _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n@(Add _ _) d = paren (p_expr n) ++ "/" ++ p_expr d
divide n@(Sub _ _) d = paren (p_expr n) ++ "/" ++ p_expr d
divide n d = p_expr n ++ "/" ++ p_expr d

-- | Helper for properly rendering negation of expressions
neg :: Expr -> String
neg a@(Var _) = "-" ++ p_expr a
neg a@(Dbl _) = "-" ++ p_expr a
neg a@(Int _) = "-" ++ p_expr a
neg a@(Sym _) = "-" ++ p_expr a
neg   (Neg n) = p_expr n
neg a         = paren ("-" ++ p_expr a)

-- | Helper for properly rendering exponents
pow :: Expr -> Expr -> String
pow a@(Add _ _) b = sqbrac (p_expr a) ++ sup (p_expr b)
pow a@(Sub _ _) b = sqbrac (p_expr a) ++ sup (p_expr b)
pow a@(Frac _ _) b = sqbrac (p_expr a) ++ sup (p_expr b) --Found in HTMLHelpers
pow a@(Div _ _) b = paren (p_expr a) ++ sup (p_expr b)
pow a@(Mul _ _) b = paren (p_expr a) ++ sup (p_expr b)
pow a@(Pow _ _) b = paren (p_expr a) ++ sup (p_expr b)
pow a b = p_expr a ++ sup (p_expr b)

-----------------------------------------------------------------
------------------BEGIN TABLE PRINTING---------------------------
-----------------------------------------------------------------

-- | Renders HTML table, called by 'printLO'
makeTable :: Tags -> [[Spec]] -> String -> Bool -> String -> Doc
makeTable _ [] _ _ _       = error "No table to print (see PrintHTML)"
makeTable ts (l:lls) r b t = refwrap r (wrap "table" ts (
    tr (makeHeaderCols l) $$ makeRows lls) $$ if b then caption t else empty)

-- | Helper for creating table rows
makeRows :: [[Spec]] -> Doc
makeRows []     = empty
makeRows (c:cs) = tr (makeColumns c) $$ makeRows cs

makeColumns, makeHeaderCols :: [Spec] -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols ls = vcat $ map (th . text . p_spec) ls

-- | Helper for creating table columns
makeColumns ls = vcat $ map (td . text . p_spec) ls

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: L.DType -> [(String,[LayoutObj])] -> String -> Doc
makeDefn _ [] _   = error "Empty definition"
makeDefn dt ps l = refwrap l $ wrap "table" [dtag dt] (makeDRows ps)
  where dtag (L.Data _)   = "ddefn"
        dtag (L.Theory _) = "tdefn"
        dtag (L.General)  = "gdefn"

-- | Helper for making the definition table rows
makeDRows :: [(String,[LayoutObj])] -> Doc
makeDRows []         = error "No fields to create defn table"
makeDRows ((f,d):[]) = tr (th (text f) $$ td (vcat $ map printLO d))
makeDRows ((f,d):ps) = tr (th (text f) $$ td (vcat $ map printLO d)) $$ makeDRows ps

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists
makeList :: ListType -> Doc
makeList (Simple items) = div_tag ["list"] 
  (vcat $ map (\(b,e) -> wrap "p" [] ((text (p_spec b ++ ": ") <> (p_item e)))) items)
makeList (Desc items)   = div_tag ["list"]
  (vcat $ map (\(b,e) -> wrap "p" [] ((wrap "b" [] (text (p_spec b ++ ": "))
   <> (p_item e)))) items)
makeList t@(Ordered items) = wrap (show t ++ "l") ["list"] (vcat $ map
  (wrap "li" [] . p_item) items)
makeList t@(Unordered items) = wrap (show t ++ "l") ["list"] (vcat $ map
  (wrap "li" [] . p_item) items)
makeList (Definitions items) = div_tag ["list"] 
  (vcat $ map (\(b,e) -> wrap "p" [] ((text (p_spec b ++ " is the") <+> 
  (p_item e)))) items)

-- | Helper for rendering list items
p_item :: ItemType -> Doc  
p_item (Flat s) = text $ p_spec s
p_item (Nested s l) = vcat [text (p_spec s),makeList l]
  
-----------------------------------------------------------------
------------------BEGIN FIGURE PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders figures in HTML
makeFigure :: String -> String -> String -> Doc
makeFigure r c f = refwrap r (image f c $$ caption c)

-----------------------------------------------------------------
------------------BEGIN EXPR OP PRINTING-------------------------
-----------------------------------------------------------------
-- | Renders expression operations/functions. 
p_op :: Function -> [Expr] -> String
p_op f@(Cross) xs = binfix_op f xs
p_op f@(Summation bs) (x:[]) = show f ++ makeBound bs ++ paren (p_expr x)
p_op (Summation _) _ = error "Something went wrong with a summation"
p_op f@(Product bs) (x:[]) = show f ++ makeBound bs ++ paren (p_expr x)
p_op f@(Integral bs wrtc) (x:[]) = 
  show f ++ makeIBound bs ++ paren (p_expr x ++ p_expr wrtc)
p_op (Integral _ _) _  = error "Something went wrong with an integral" 
p_op Abs (x:[]) = "|" ++ p_expr x ++ "|"
p_op Abs _ = error "Abs should only take one expr."
p_op Norm (x:[]) = "||" ++ p_expr x ++ "||"
p_op Norm _ = error "Norm should only take on expression."
p_op f@(Exp) (x:[]) = show f ++ sup (p_expr x)
p_op f (x:[]) = show f ++ paren (p_expr x) --Unary ops, this will change once more complicated functions appear.
p_op _ _ = error "Something went wrong with an operation"

-- | Helper for summation bound creation, used by 'p_op'
makeBound :: Maybe ((Symbol, Expr),Expr) -> String
makeBound (Just ((s,v),hi)) = sub (symbol s ++"="++ p_expr v) ++ sup (p_expr hi)
makeBound Nothing = ""

-- | Helper for integration bound creation, used by 'p_op'
makeIBound :: (Maybe Expr, Maybe Expr) -> String
makeIBound (Just low, Just high) = sub (p_expr low) ++ sup (p_expr high)
makeIBound (Just low, Nothing)   = sub (p_expr low)
makeIBound (Nothing, Just high)  = sup (p_expr high)
makeIBound (Nothing, Nothing)    = ""

-- | Helper for rendering binary infix operators, used by 'p_op'
binfix_op :: Function -> [Expr] -> String
binfix_op f (x:y:[]) = p_expr x ++ show f ++ p_expr y
binfix_op _ _ = error "Attempting to print binary operate with inappropriate" ++
                   "number of operands (should be 2)"

-- | Renders modules
makeModule :: String -> String -> Doc
makeModule m l = refwrap l (paragraph $ wrap "b" [] (text m))

-- | Renders assumptions
makeAssump :: String -> String -> Doc
makeAssump a l = refwrap l (wrap "ol" [] (text a))
