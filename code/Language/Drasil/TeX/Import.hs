module Language.Drasil.TeX.Import(makeDocument,spec) where

import Control.Lens ((^.))
import Data.List (intersperse)

import Language.Drasil.Expr (Expr(..), BinOp(..), sy, UFunc(..), Oper(Mul,Add),
    DerivType(..), EOperator(..), ($=), RealRange(..), DomainDesc(..))
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Expr.Extract
import Language.Drasil.Chunk.Change (chng, chngType, ChngType(..))
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Spec
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
import Language.Drasil.Unit (usymb)
import Language.Drasil.Printing.Import (oper,space)

expr :: HasSymbolTable ctx => Expr -> ctx -> P.Expr
expr (Dbl d)            _ = P.Dbl  d
expr (Int i)            _ = P.Int  i
expr (Str s)            _ = P.Str  s
expr (Assoc op l)      sm = P.Assoc (oper op) $ map (\x -> expr x sm) l
expr (C c)            sm = symbol $ eqSymb $ symbLookup c $ sm^.symbolTable -- FIXME Stage?
expr (Deriv Part a b)  sm = P.Div (P.Assoc P.Mul [P.Spec Partial, expr a sm])
                            (P.Assoc P.Mul [P.Spec Partial, symbol $ eqSymb $ symbLookup b $ sm^.symbolTable])
expr (Deriv Total a b) sm = P.Div (P.Assoc P.Mul [P.Ident "d", expr a sm])
                            (P.Assoc P.Mul [P.Ident "d", symbol $ eqSymb $ symbLookup b $ sm^.symbolTable])
expr (FCall f [x])     sm = P.Row [expr f sm, P.Fenced P.Paren P.Paren $ expr x sm]
expr (FCall f x)       sm = P.Row [expr f sm, 
  P.Fenced P.Paren P.Paren $ P.Row $ intersperse (P.MO P.Comma) $ map (flip expr sm) x]
expr (Case ps)         sm = if length ps < 2 then
        error "Attempting to use multi-case expr incorrectly"
        else P.Case (zip (map (flip expr sm . fst) ps) (map (flip expr sm . snd) ps))
expr (Matrix a)        sm = P.Mtx $ map (map (flip expr sm)) a
expr (UnaryOp Log u)   sm = mkCall sm P.Log u
expr (UnaryOp Sin u)    sm = mkCall sm P.Sin u
expr (UnaryOp Cos u)    sm = mkCall sm P.Cos u
expr (UnaryOp Tan u)    sm = mkCall sm P.Tan u
expr (UnaryOp Sec u)    sm = mkCall sm P.Sec u
expr (UnaryOp Csc u)    sm = mkCall sm P.Csc u
expr (UnaryOp Cot u)    sm = mkCall sm P.Cot u
expr (UnaryOp Dim u)    sm = mkCall sm P.Dim u
expr (UnaryOp Not u)    sm = P.Row [P.MO P.Not, expr u sm]
expr (UnaryOp Exp u)    sm = P.Row [P.MO P.Exp, P.Sup $ expr u sm]
expr (UnaryOp Abs u)    sm = P.Fenced P.Abs P.Abs $ expr u sm
expr (UnaryOp Norm u)   sm = P.Fenced P.Norm P.Norm $ expr u sm
expr (UnaryOp Sqrt u)   sm = P.Row [P.MO P.Sqrt, P.Row [expr u sm]]
expr (UnaryOp Neg u)    sm = neg sm u
expr (EOp o)           sm = eop o sm
expr (BinaryOp Frac a b) sm = P.Div (expr a sm) (expr b sm)
expr (BinaryOp Cross a b) sm = P.Row [expr a sm, P.MO P.Cross, expr b sm]
expr (BinaryOp Dot a b) sm = P.Row [expr a sm, P.MO P.Dot, expr b sm]
expr (BinaryOp Eq a b) sm = mkBOp sm P.Eq a b
expr (BinaryOp NEq a b) sm = mkBOp sm P.NEq a b
expr (BinaryOp Lt a b) sm = mkBOp sm P.Lt a b
expr (BinaryOp Gt a b) sm = mkBOp sm P.Gt a b
expr (BinaryOp LEq a b) sm = mkBOp sm P.LEq a b
expr (BinaryOp GEq a b) sm = mkBOp sm P.GEq a b
expr (BinaryOp Impl a b) sm = mkBOp sm P.Impl a b
expr (BinaryOp Iff a b) sm = mkBOp sm P.Iff a b
expr (BinaryOp Index a b) sm = indx sm a b
expr (BinaryOp Pow a b) sm = pow sm a b
expr (BinaryOp Subt a b)  sm = P.Row [expr a sm, P.MO P.Subt, expr b sm]
expr (IsIn  a b)       sm = P.Row [expr a sm, P.MO P.IsIn, space b]

mkCall :: HasSymbolTable ctx => ctx -> P.Ops -> Expr -> P.Expr
mkCall s o e = P.Row [P.MO o, P.Fenced P.Paren P.Paren $ expr e s]

mkBOp :: HasSymbolTable ctx => ctx -> P.Ops -> Expr -> Expr -> P.Expr
mkBOp sm o a b = P.Row [expr a sm, P.MO o, expr b sm]

neg :: HasSymbolTable ctx => ctx -> Expr -> P.Expr
neg s x@(Dbl _) = P.Row [P.MO P.Neg, expr x s]
neg s x@(Int _) = P.Row [P.MO P.Neg, expr x s]
neg s a@(C   _) = P.Row [P.MO P.Neg, expr a s]
neg s a@(UnaryOp _ _)   = P.Row [P.MO P.Neg, expr a s]
neg s a@(Assoc Mul _)   = P.Row [P.MO P.Neg, expr a s]
neg s x                 = P.Fenced P.Paren P.Paren $ P.Row $ [P.MO P.Neg, expr x s]

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
  P.Funct (P.Integral (Just (symbol v), Nothing) v) (expr e sm)
eop (Integral (IntegerDD _ _) _) _ =
  error "TeX/Import.hs Integral cannot be over Integers"

-- | For printing indexes
indx :: HasSymbolTable ctx => ctx -> Expr -> Expr -> P.Expr
indx sm (C c) i = f s
  where
    i' = expr i sm
    s = eqSymb $ symbLookup c $ sm^.symbolTable
    f (Corners [] [] [] [b] e) = 
      let e' = symbol e
          b' = symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(Atomic _) = P.Row [symbol a, P.Sub i']
    f a@(Greek _)  = P.Row [symbol a, P.Sub i']
    f   e          = let e' = symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [expr a sm], P.Sub $ expr i sm]

symbol :: Symbol -> P.Expr
symbol (Atomic s)  = P.Ident s
symbol (Special s) = P.Spec s
symbol (Greek g)   = P.Gr g
symbol (Concat sl) = P.Row $ map symbol sl
--
-- handle the special cases first, then general case
-- double Row is to reproduce what was there before.  FIXME ?
symbol (Corners [] [] [x] [] s) = P.Row [P.Row [symbol s, P.Sup $ symbol x]]
symbol (Corners [] [] [] [x] s) = P.Row [P.Row [symbol s, P.Sub $ symbol x]]
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (Atop f s) = sFormat f s
symbol (Empty)    = P.Row []

sFormat :: Decoration -> Symbol -> P.Expr
sFormat Hat    s = P.Over P.Hat $ symbol s
sFormat Vector s = P.Font P.Bold $ symbol s
sFormat Prime  s = P.Row [symbol s, P.MO P.Prime]

-- | Helper for properly rendering exponents
pow :: HasSymbolTable ctx => ctx -> Expr -> Expr -> P.Expr
pow sm a@(Assoc Add _)  b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Subt _ _) b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Frac _ _) b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(Assoc Mul _)  b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Pow _ _)  b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a                b = P.Row [expr a sm, P.Sup (expr b sm)]

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

-- decorate :: Decoration -> Sentence -> Sentence
-- decorate Hat    s = S "\\hat{" :+: s :+: S "}"
-- decorate Vector s = S "\\bf{" :+: s :+: S "}"
-- decorate Prime  s = s :+: S "'"

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
