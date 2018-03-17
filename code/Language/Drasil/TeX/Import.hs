module Language.Drasil.TeX.Import where

import Control.Lens ((^.))
import Data.List (intersperse)

import Language.Drasil.Expr (Expr(..), Oper(..), BinOp(..), sy, UFunc(..),
    DerivType(..), EOperator(..), ($=), RealRange(..), DomainDesc(..))
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Expr.Extract
import Language.Drasil.Chunk.Change (chng, chngType, ChngType(..))
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Spec
import Language.Drasil.Space (Space(..))
import qualified Language.Drasil.TeX.AST as T
import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term)
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.SymbolForm (eqSymb)
import Language.Drasil.ChunkDB (getUnitLup, HasSymbolTable(..),symbLookup)
import Language.Drasil.Chunk.ReqChunk (requires)
import Language.Drasil.Chunk.Citation ( Citation, CiteField(..), HP(..)
                                      , citeID, externRefT, fields)
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Reference
import Language.Drasil.Symbol
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Unit (usymb)

-- | translating operations
oper :: Oper -> P.Oper
oper And = P.And
oper Or = P.Or
oper Add = P.Add
oper Mul = P.Mul

ufunc :: UFunc -> P.UFunc
ufunc Norm = P.Norm
ufunc Abs = P.Abs
ufunc Log = P.Log
ufunc Sin = P.Sin
ufunc Cos = P.Cos
ufunc Tan = P.Tan
ufunc Sec = P.Sec
ufunc Csc = P.Csc
ufunc Cot = P.Cot
ufunc Exp = P.Exp
ufunc Sqrt = P.Sqrt
ufunc Not = P.Not
ufunc Neg = P.Neg
ufunc Dim = P.Dim

binop :: BinOp -> P.BinOp
binop Frac = P.Frac
binop Div = P.Div
binop Pow = P.Pow
binop Subt = P.Subt
binop Eq = P.Eq
binop NEq = P.NEq
binop Lt = P.Lt
binop Gt = P.Gt
binop LEq = P.LEq
binop GEq = P.GEq
binop Impl = P.Impl
binop Iff = P.Iff
binop Index = P.Index
binop Dot = P.Dot
binop Cross = P.Cross

space :: Space -> P.Expr
space Integer = P.MO P.Integer
space Rational = P.MO P.Rational
space Real = P.MO P.Real
space Natural = P.MO P.Natural
space Boolean = P.MO P.Boolean
space Char = P.Ident "Char"
space String = P.Ident "String"
space Radians = error "Radians not translated"
space (Vect _) = error "Vector space not translated"
space (DiscreteI _) = error "DiscreteI" --ex. let A = {1, 2, 4, 7}
space (DiscreteD _) = error "DiscreteD" -- [Double]
space (DiscreteS l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map P.Ident l

expr :: HasSymbolTable ctx => Expr -> ctx -> P.Expr
expr (Dbl d)            _ = P.Dbl  d
expr (Int i)            _ = P.Int  i
expr (Str s)            _ = P.Str  s
expr (Assoc op l)      sm = P.Assoc (oper op) $ map (\x -> expr x sm) l
expr (C c)            sm = P.Sym $ eqSymb $ symbLookup c $ sm^.symbolTable -- FIXME Stage?
expr (Deriv Part a b)  sm = P.BOp P.Frac (P.Assoc P.Mul [P.Sym (Special Partial), expr a sm])
                            (P.Assoc P.Mul [P.Sym (Special Partial), P.Sym $ eqSymb $ symbLookup b $ sm^.symbolTable])
expr (Deriv Total a b) sm = P.BOp P.Frac (P.Assoc P.Mul [P.Sym lD, expr a sm])
                            (P.Assoc P.Mul [P.Sym lD, P.Sym $ eqSymb $ symbLookup b $ sm^.symbolTable])
expr (FCall f x)       sm = P.Call (expr f sm) (map (flip expr sm) x)
expr (Case ps)         sm = if length ps < 2 then
        error "Attempting to use multi-case expr incorrectly"
        else P.Case (zip (map (flip expr sm . fst) ps) (map (flip expr sm . snd) ps))
expr (Matrix a)        sm = P.Mtx $ map (map (flip expr sm)) a
expr (UnaryOp o u)     sm = P.UOp (ufunc o) $ expr u sm
expr (EOp o)           sm = eop o sm
expr (BinaryOp Div a b) sm = P.BOp P.Frac (replace_divs sm a) (replace_divs sm b)
expr (BinaryOp o a b)  sm = P.BOp (binop o) (expr a sm) (expr b sm)
expr (IsIn  a b)       sm = P.Row [expr a sm, P.MO P.IsIn, space b]

eop :: HasSymbolTable ctx => EOperator -> ctx -> P.Expr
eop (Summation (IntegerDD v (BoundedR l h)) e) sm =
  P.Funct (P.Summation (Just ((v, expr l sm), expr h sm))) (expr e sm)
eop (Summation (All _) e) sm = P.Funct (P.Summation Nothing) (expr e sm)
eop (Summation(RealDD _ _) _) _ = error "TeX/Import.hs Summation cannot be over Real"
eop (Product (IntegerDD v (BoundedR l h)) e) sm =
  P.Funct (P.Product (Just ((v, expr l sm), expr h sm))) (expr e sm)
eop (Product (All _) e) sm = P.Funct (P.Product Nothing) (expr e sm)
eop (Product (RealDD _ _) _) _ = error "TeX/Import.hs Product cannot be over Real"
eop (Integral (RealDD v (BoundedR l h)) e) sm =
  P.Funct (P.Integral (Just (expr l sm), Just (expr h sm)) v) (expr e sm)
eop (Integral (All v) e) sm =
  P.Funct (P.Integral (Just (P.Sym v), Nothing) v) (expr e sm)
eop (Integral (IntegerDD _ _) _) _ =
  error "TeX/Import.hs Integral cannot be over Integers"

int_wrt :: Symbol -> P.Expr
int_wrt wrtc = P.Assoc P.Mul [P.Sym lD, P.Sym wrtc]

replace_divs :: HasSymbolTable ctx => ctx -> Expr -> P.Expr
replace_divs sm (BinaryOp Div a b) = P.BOp P.Div (replace_divs sm a) (replace_divs sm b)
replace_divs sm (Assoc op l) = P.Assoc (oper op) $ map (replace_divs sm) l
replace_divs sm (BinaryOp Pow a b) = P.BOp P.Pow (replace_divs sm a) (replace_divs sm b)
replace_divs sm (BinaryOp Subt a b) = P.BOp P.Subt (replace_divs sm a) (replace_divs sm b)
replace_divs sm a            = expr a sm

spec :: HasSymbolTable ctx => ctx -> Sentence -> P.Spec
spec _  (S s)          = P.S s
spec _  (Sy s)         = P.Sy s
spec sm (EmptyS :+: b) = spec sm b
spec sm (a :+: EmptyS) = spec sm a
spec sm (a :+: b)      = spec sm a P.:+: spec sm b
spec _  (G g)          = P.G g
spec _  (Sp s)         = P.Sp s
spec sm (F f s)        = spec sm (accent f s)
spec _  (P s)          = P.N s
spec _  (Ref t r _)    = P.Ref t r (P.S r)
spec sm (Quote q)      = P.S "``" P.:+: spec sm q P.:+: P.S "\""
spec _  EmptyS         = P.EmptyS
spec sm (E e)          = P.E $ expr e sm

decorate :: Decoration -> Sentence -> Sentence
decorate Hat    s = S "\\hat{" :+: s :+: S "}"
decorate Vector s = S "\\bf{" :+: s :+: S "}"
decorate Prime  s = s :+: S "'"

accent :: Accent -> Char -> Sentence
accent Grave  s = S $ "\\`{" ++ (s : "}")
accent Acute  s = S $ "\\'{" ++ (s : "}")

makeDocument :: HasSymbolTable ctx => ctx -> Document -> T.Document
makeDocument sm (Document title author sections) =
  T.Document (spec sm title) (spec sm author) (createLayout sm sections)

layout :: HasSymbolTable ctx => ctx -> Int -> SecCons -> T.LayoutObj
layout sm currDepth (Sub s) = sec sm (currDepth+1) s
layout sm _         (Con c) = lay sm c

createLayout :: HasSymbolTable ctx => ctx -> Sections -> [T.LayoutObj]
createLayout sm = map (sec sm 0)

sec :: HasSymbolTable ctx => ctx -> Int -> Section -> T.LayoutObj
sec sm depth x@(Section title contents _) =
  T.Section depth (spec sm title) (map (layout sm depth) contents) (P.S (refAdd x))

lay :: HasSymbolTable ctx => ctx -> Contents -> T.LayoutObj
lay sm x@(Table hdr lls t b _)
  | null lls || length hdr == length (head lls) = T.Table ((map (spec sm) hdr) :
      (map (map (spec sm)) lls)) (P.S (refAdd x)) b (spec sm t)
  | otherwise = error $ "Attempting to make table with " ++ show (length hdr) ++
                        " headers, but data contains " ++
                        show (length (head lls)) ++ " columns."
lay sm (Paragraph c)         = T.Paragraph (spec sm c)
lay sm (EqnBlock c _)        = T.EqnBlock (P.E (expr c sm))
--lay (CodeBlock c)         = T.CodeBlock c
lay sm x@(Definition c)       = T.Definition (makePairs sm c) (P.S (refAdd x))
lay sm (Enumeration cs)       = T.List $ makeL sm cs
lay sm x@(Figure c f wp _)    = T.Figure (P.S (refAdd x)) (spec sm c) f wp
lay sm x@(Requirement r)      =
  T.Requirement (spec sm (requires r)) (P.S (refAdd x))
lay sm x@(Assumption a)       =
  T.Assumption (spec sm (assuming a)) (P.S (refAdd x))
lay sm x@(Change lc)    = (if (chngType lc) == Likely then 
  T.LikelyChange else T.UnlikelyChange) (spec sm (chng lc)) (P.S (refAdd x))
lay sm x@(Graph ps w h t _)   = T.Graph (map (\(y,z) -> (spec sm y, spec sm z)) ps)
                               w h (spec sm t) (P.S (refAdd x))
lay sm (Defnt dtyp pairs rn)  = T.Defnt dtyp (layPairs pairs) (P.S rn)
  where layPairs = map (\(x,y) -> (x, map (lay sm) y))
lay sm (Bib bib)          = T.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable ctx => ctx -> Citation -> P.Citation
layCite sm c = P.Cite (citeID c) (externRefT c) (map (layField sm) (fields c))

layField :: HasSymbolTable ctx => ctx -> CiteField -> P.CiteField
layField sm (Address      s) = P.Address      $ spec sm s
layField  _ (Author       p) = P.Author       p
layField sm (BookTitle    b) = P.BookTitle    $ spec sm b
layField  _ (Chapter      c) = P.Chapter      c
layField  _ (Edition      e) = P.Edition      e
layField  _ (Editor       e) = P.Editor       e
layField sm (Institution  i) = P.Institution  $ spec sm i
layField sm (Journal      j) = P.Journal      $ spec sm j
layField  _ (Month        m) = P.Month        m
layField sm (Note         n) = P.Note         $ spec sm n
layField  _ (Number       n) = P.Number       n
layField sm (Organization o) = P.Organization $ spec sm o
layField  _ (Pages        p) = P.Pages        p
layField sm (Publisher    p) = P.Publisher    $ spec sm p
layField sm (School       s) = P.School       $ spec sm s
layField sm (Series       s) = P.Series       $ spec sm s
layField sm (Title        t) = P.Title        $ spec sm t
layField sm (Type         t) = P.Type         $ spec sm t
layField  _ (Volume       v) = P.Volume       v
layField  _ (Year         y) = P.Year         y
layField sm (HowPublished (URL  u)) = P.HowPublished (P.URL  $ spec sm u)
layField sm (HowPublished (Verb v)) = P.HowPublished (P.Verb $ spec sm v)

makeL :: HasSymbolTable ctx => ctx -> ListType -> P.ListType
makeL sm (Bullet bs)      = P.Ordered     $ map (item sm) bs
makeL sm (Numeric ns)     = P.Unordered   $ map (item sm) ns
makeL sm (Simple ps)      = P.Simple      $ map (\(x,y) -> (spec sm x, item sm y)) ps
makeL sm (Desc ps)        = P.Desc        $ map (\(x,y) -> (spec sm x, item sm y)) ps
makeL sm (Definitions ps) = P.Definitions $ map (\(x,y) -> (spec sm x, item sm y)) ps

item :: HasSymbolTable ctx => ctx -> ItemType -> P.ItemType
item sm (Flat i)     = P.Flat   (spec sm i)
item sm (Nested t s) = P.Nested (spec sm t) (makeL sm s)

makePairs :: HasSymbolTable ctx => ctx -> DType -> [(String,[T.LayoutObj])]
makePairs m (Data c) = [
  ("Label",       [T.Paragraph $ spec m (titleize $ c ^. term)]),
  ("Units",       [T.Paragraph $ spec m (unit'2Contents c)]),
  ("Equation",    [eqnStyleDD  $ buildEqn m c]),
  ("Description", [T.Paragraph $ buildDDDescription m c])
  ]
makePairs m (Theory c) = [
  ("Label",       [T.Paragraph $ spec m (titleize $ c ^. term)]),
  ("Equation",    [eqnStyleTM $ P.E (expr (c ^. relat) m)]),
  ("Description", [T.Paragraph (spec m (c ^. defn))])
  ]
makePairs _ General  = error "Not yet implemented"
makePairs _ Instance = error "Not yet implemented"
makePairs _ TM       = error "Not yet implemented"
makePairs _ DD       = error "Not yet implemented"


-- Toggle equation style
eqnStyleDD :: T.Contents -> T.LayoutObj
eqnStyleDD = if numberedDDEquations then T.EqnBlock else T.Paragraph

eqnStyleTM :: T.Contents -> T.LayoutObj
eqnStyleTM = if numberedTMEquations then T.EqnBlock else T.Paragraph

buildEqn :: HasSymbolTable ctx => ctx -> QDefinition -> P.Spec
buildEqn sm c = P.N (eqSymb c) P.:+: P.S " = " P.:+:
  P.E (expr (c^.equat) sm)

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable ctx => ctx -> QDefinition -> P.Spec
buildDDDescription m c = descLines m
  (if verboseDDDescription then vars (sy c $= c^.equat) m else [])

descLines :: (HasSymbolTable ctx, Quantity q) => ctx -> [q] -> P.Spec
descLines _ []      = error "No chunks to describe"
descLines m (vc:[]) = (P.N (eqSymb vc) P.:+:
  (P.S " is the " P.:+: (spec m (phrase $ vc ^. term)) P.:+:
   maybe (P.S "") (\a -> P.S " (" P.:+: P.Sy (a ^. usymb) P.:+: P.S ")") (getUnitLup vc m)))
descLines m (vc:vcs) = descLines m (vc:[]) P.:+: P.HARDNL P.:+: descLines m vcs
