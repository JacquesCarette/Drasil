module Language.Drasil.TeX.Import where

import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), UFunc(..), BiFunc(..), Oper(..),
    DerivType(..), EOperator(..), ($=), RealRange(..), DomainDesc(..))
import Language.Drasil.Expr.Extract
import Language.Drasil.Spec
import qualified Language.Drasil.TeX.AST as T
import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term)
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.Quantity (Quantity(..), eqSymb)
import Language.Drasil.ChunkDB (getUnitLup, symbLookup, HasSymbolTable(..))
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Symbol
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.SymbolAlphabet
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Unit (usymb)
import Language.Drasil.Citations (Citation(..),CiteField(..))

expr :: HasSymbolTable ctx => Expr -> ctx -> P.Expr
expr (V v)              _ = P.Var  v
expr (Dbl d)            _ = P.Dbl  d
expr (Int i)            _ = P.Int  i
expr (Assoc op l)      sm = P.Assoc op $ map (\x -> expr x sm) l
expr (a :/ b)          sm = P.BOp P.Frac (replace_divs a sm) (replace_divs b sm)
expr (a :- b)          sm = P.BOp P.Sub  (expr a sm) (expr b sm)
expr (a :. b)          sm = P.BOp P.Dot  (expr a sm) (expr b sm)
expr (Neg a)           sm = P.Neg  (expr a sm)
expr (C c)             sm = -- FIXME: Add Stage for Context
  P.Sym  (eqSymb (symbLookup c (sm ^. symbolTable)))
expr (Deriv Part a 1)  sm = P.Assoc Mul [P.Sym (Special Partial), expr a sm]
expr (Deriv Total a 1) sm = P.Assoc Mul [P.Sym lD, expr a sm]
expr (Deriv Part a b)  sm = P.BOp P.Frac (P.Assoc Mul [P.Sym (Special Partial), expr a sm])
                           (P.Assoc Mul [P.Sym (Special Partial), expr b sm])
expr (Deriv Total a b) sm = P.BOp P.Frac (P.Assoc Mul [P.Sym lD, expr a sm])
                           (P.Assoc Mul [P.Sym lD, expr b sm])
expr (FCall f x)       sm = P.Call (expr f sm) (map (flip expr sm) x)
expr (Case ps)         sm = if length ps < 2 then
        error "Attempting to use multi-case expr incorrectly"
        else P.Case (zip (map (flip expr sm . fst) ps) (map (flip expr sm . snd) ps))
expr (Matrix a)        sm = P.Mtx $ map (map (flip expr sm)) a
expr (Index a i)       sm = P.BOp P.Index (expr a sm) (expr i sm)
expr (UnaryOp u)       sm = (\(x,y) -> P.Op x [y]) (ufunc u sm)
expr (Grouping e)      sm = P.Grouping (expr e sm)
expr (BinaryOp b)      sm = bfunc b sm
expr (EOp o)           sm = (\(x,y) -> P.Op x [y]) (eop o sm)
expr (Not a)           sm = P.Not  (expr a sm)
expr (IsIn  a b)       sm = P.IsIn  (expr a sm) b
expr (ForAll a b)      sm = P.Forall a (expr b sm)
expr (Exists a b)      sm = P.Exists a (expr b sm)
expr _                 _  = error "Expression unimplemented in TeX"

ufunc :: HasSymbolTable ctx => UFunc -> ctx -> (P.Function, P.Expr)
ufunc (Log e) sm = (P.Log, expr e sm)
ufunc (Abs e) sm = (P.Abs, expr e sm)
ufunc (Norm e) sm = (P.Norm, expr e sm)
ufunc (Sin e) sm = (P.Sin, expr e sm)
ufunc (Cos e) sm = (P.Cos, expr e sm)
ufunc (Tan e) sm = (P.Tan, expr e sm)
ufunc (Sec e) sm = (P.Sec, expr e sm)
ufunc (Csc e) sm = (P.Csc, expr e sm)
ufunc (Cot e) sm = (P.Cot, expr e sm)
ufunc (Exp e) sm = (P.Exp, expr e sm)
ufunc (Sqrt e) sm = (P.Sqrt, expr e sm)

bfunc :: HasSymbolTable ctx => BiFunc -> ctx -> P.Expr
bfunc (Cross e1 e2)     sm = P.BOp P.Cross (expr e1 sm) (expr e2 sm)
bfunc (Power e1 e2)     sm = P.BOp P.Pow (expr e1 sm) (expr e2 sm)
bfunc (EEquals a b)     sm = P.BOp P.Eq  (expr a sm) (expr b sm)
bfunc (ENEquals a b)    sm = P.BOp P.NEq (expr a sm) (expr b sm)
bfunc (ELess a b)       sm = P.BOp P.Lt  (expr a sm) (expr b sm)
bfunc (EGreater a b)    sm = P.BOp P.Gt  (expr a sm) (expr b sm)
bfunc (ELessEq a b)     sm = P.BOp P.LEq (expr a sm) (expr b sm)
bfunc (EGreaterEq a b)  sm = P.BOp P.GEq (expr a sm) (expr b sm)
bfunc (Implies a b)     sm = P.BOp P.Impl  (expr a sm) (expr b sm)
bfunc (IFF a b)         sm = P.BOp P.Iff   (expr a sm) (expr b sm)


eop :: HasSymbolTable ctx => EOperator -> ctx -> (P.Function, P.Expr)
eop (Summation (IntegerDD v (BoundedR l h)) e) sm =
  (P.Summation (Just ((v, expr l sm), expr h sm)), (expr e sm))
eop (Summation (All _) e) sm = (P.Summation Nothing,(expr e sm))
eop (Summation(RealDD _ _) _) _ = error "TeX/Import.hs Summation cannot be over Real"
eop (Product (IntegerDD v (BoundedR l h)) e) sm =
  (P.Product (Just ((v, expr l sm), expr h sm)), expr e sm)
eop (Product (All _) e) sm = (P.Product Nothing, (expr e sm))
eop (Product (RealDD _ _) _) _ = error "TeX/Import.hs Product cannot be over Real"
eop (Integral (RealDD v (BoundedR l h)) e) sm =
  (P.Integral (Just (expr l sm), Just (expr h sm)) v, expr e sm)
eop (Integral (All v) e) sm =
  (P.Integral (Just (P.Sym v), Nothing) v, expr e sm)
eop (Integral (IntegerDD _ _) _) _ =
  error "TeX/Import.hs Integral cannot be over Integers"

int_wrt :: Symbol -> P.Expr
int_wrt wrtc = P.Assoc Mul [P.Sym lD, P.Sym wrtc]

replace_divs :: HasSymbolTable ctx => Expr -> ctx -> P.Expr
replace_divs (a :/ b) sm = P.BOp P.Div (replace_divs a sm) (replace_divs b sm)
replace_divs (Assoc op l) sm = P.Assoc op $ map (\x -> replace_divs x sm) l
replace_divs (BinaryOp (Power a b)) sm = P.BOp P.Pow (replace_divs a sm) (replace_divs b sm)
replace_divs (a :- b) sm = P.BOp P.Sub (replace_divs a sm) (replace_divs b sm)
replace_divs a        sm = expr a sm

spec :: HasSymbolTable ctx => Sentence -> ctx -> T.Spec
spec (S s)      _ = T.S s
spec (Sy s)     _ = T.Sy s
spec (EmptyS :+: b) sm = spec b sm
spec (a :+: EmptyS) sm = spec a sm
spec (a :+: b) sm = spec a sm T.:+: spec b sm
spec (G g)      _ = T.G g
spec (Sp s)     _ = T.Sp s
spec (F f s)   sm = spec (accent f s) sm
spec (P s)      _ = T.N s
spec (Ref t r) sm = T.Ref t (spec r sm)
spec (Quote q) sm = T.S "``" T.:+: spec q sm T.:+: T.S "\""
spec EmptyS     _ = T.EmptyS
spec (E e)     sm = T.E $ expr e sm

decorate :: Decoration -> Sentence -> Sentence
decorate Hat    s = S "\\hat{" :+: s :+: S "}"
decorate Vector s = S "\\bf{" :+: s :+: S "}"
decorate Prime  s = s :+: S "'"

accent :: Accent -> Char -> Sentence
accent Grave  s = S $ "\\`{" ++ (s : "}")
accent Acute  s = S $ "\\'{" ++ (s : "}")

makeDocument :: HasSymbolTable ctx => Document -> ctx -> T.Document
makeDocument (Document title author sections) sm =
  T.Document (spec title sm) (spec author sm) (createLayout sections sm)

layout :: HasSymbolTable ctx => Int -> SecCons -> ctx -> T.LayoutObj
layout currDepth (Sub s) sm = sec (currDepth+1) s sm
layout _         (Con c) sm = lay sm c

createLayout :: HasSymbolTable ctx => Sections -> ctx -> [T.LayoutObj]
createLayout secs sm = map (flip (sec 0) sm) secs

sec :: HasSymbolTable ctx => Int -> Section -> ctx -> T.LayoutObj
sec depth x@(Section title contents) sm =
  T.Section depth (spec title sm) (map (flip (layout depth) sm) contents) (spec (refName x) sm)

lay :: HasSymbolTable ctx => ctx -> Contents -> T.LayoutObj
lay sm x@(Table hdr lls t b)
  | null lls || length hdr == length (head lls) = T.Table ((map (flip spec sm) hdr) :
      (map (map (flip spec sm)) lls)) (spec (refName x) sm) b (spec t sm)
  | otherwise = error $ "Attempting to make table with " ++ show (length hdr) ++
                        " headers, but data contains " ++
                        show (length (head lls)) ++ " columns."
lay sm (Paragraph c)         = T.Paragraph (spec c sm)
lay sm (EqnBlock c)          = T.EqnBlock (T.E (expr c sm))
--lay (CodeBlock c)         = T.CodeBlock c
lay sm x@(Definition c)      = T.Definition (makePairs sm c) (spec (refName x) sm)
lay sm (Enumeration cs)      = T.List $ makeL sm cs
lay sm x@(Figure c f wp)     = T.Figure (spec (refName x) sm) (spec c sm) f wp
lay sm x@(Requirement r)     =
  T.Requirement (spec (phrase (r ^. term)) sm) (spec (refName x) sm)
lay sm x@(Assumption a)      =
  T.Assumption (spec (phrase $ a ^. term) sm) (spec (refName x) sm)
lay sm x@(LikelyChange lc)   =
  T.LikelyChange (spec (phrase $ lc ^. term) sm)
  (spec (refName x) sm)
lay sm x@(UnlikelyChange ucc) =
  T.UnlikelyChange (spec (phrase $ ucc ^. term) sm)
  (spec (refName x) sm)
lay sm x@(Graph ps w h t)    = T.Graph (map (\(y,z) -> (spec y sm, spec z sm)) ps)
                               w h (spec t sm) (spec (refName x) sm)
lay sm (TMod ps r _)         = T.Definition (map (\(x,y) ->
  (x, map (lay sm) y)) ps) (spec r sm)
lay sm (DDef ps r _)         = T.Definition (map (\(x,y) ->
  (x, map (lay sm) y)) ps) (spec r sm)
lay sm (Defnt dtyp pairs rn) = T.Defnt dtyp (layPairs pairs) (spec rn sm)
  where layPairs = map (\(x,y) -> (x, map (lay sm) y))
lay _  (GDef)             = T.Paragraph (T.EmptyS)  -- need to implement!
lay _  (IMod)             = T.Paragraph (T.EmptyS)  -- need to implement!
lay sm (Bib bib)          = T.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable ctx => ctx -> Citation -> T.Citation
layCite sm (Book      fields) = T.Book      $ map (layField sm) fields
layCite sm (Article   fields) = T.Article   $ map (layField sm) fields
layCite sm (MThesis   fields) = T.MThesis   $ map (layField sm) fields
layCite sm (PhDThesis fields) = T.PhDThesis $ map (layField sm) fields
layCite sm (Misc      fields) = T.Misc      $ map (layField sm) fields
layCite sm (Online    fields) = T.Online    $ map (layField sm) fields

layField :: HasSymbolTable ctx => ctx -> CiteField -> T.CiteField
layField _  (Author     p) = T.Author     p
layField sm (Title      s) = T.Title      $ spec s sm
layField sm (Series     s) = T.Series     $ spec s sm
layField sm (Collection s) = T.Collection $ spec s sm
layField _  (Volume     n) = T.Volume     n
layField _  (Edition    n) = T.Edition    n
layField sm (Place (c, s)) = T.Place (spec c sm, spec s sm)
layField sm (Publisher  s) = T.Publisher $ spec s sm
layField sm (Journal    s) = T.Journal   $ spec s sm
layField _  (Year       n) = T.Year       n
layField _  (Date    n m y)= T.Date    n m y
layField _  (URLdate n m y)= T.URLdate n m y
layField _  (Page       n) = T.Page       n
layField _  (Pages     ns) = T.Pages     ns
layField sm (Note       s) = T.Note       $ spec s sm
layField _  (Issue      n) = T.Issue      n
layField sm (School     s) = T.School     $ spec s sm
layField sm (URL        n) = T.URL        $ spec n sm
layField sm (HowPub     s) = T.HowPub     $ spec s sm
layField _  (Editor     p) = T.Editor     p

makeL :: HasSymbolTable ctx => ctx -> ListType -> T.ListType
makeL sm (Bullet bs)      = T.Enum        $ map (item sm) bs
makeL sm (Number ns)      = T.Item        $ map (item sm) ns
makeL sm (Simple ps)      = T.Simple      $ map (\(x,y) -> (spec x sm, item sm y)) ps
makeL sm (Desc ps)        = T.Desc        $ map (\(x,y) -> (spec x sm, item sm y)) ps
makeL sm (Definitions ps) = T.Definitions $ map (\(x,y) -> (spec x sm, item sm y)) ps

item :: HasSymbolTable ctx => ctx -> ItemType -> T.ItemType
item sm (Flat i)     = T.Flat   (spec i sm)
item sm (Nested t s) = T.Nested (spec t sm) (makeL sm s)

makePairs :: HasSymbolTable ctx => ctx -> DType -> [(String,[T.LayoutObj])]
makePairs m (Data c) = [
  ("Label",       [T.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Units",       [T.Paragraph $ spec (unit'2Contents c) m]),
  ("Equation",    [eqnStyleDD $ buildEqn c m]),
  ("Description", [T.Paragraph (buildDDDescription c m)])
  ]
makePairs m (Theory c) = [
  ("Label",       [T.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Equation",    [eqnStyleTM $ T.E (expr (c ^. relat) m)]),
  ("Description", [T.Paragraph (spec (c ^. defn) m)])
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

buildEqn :: HasSymbolTable ctx => QDefinition -> ctx -> T.Spec
buildEqn c sm = T.N (eqSymb c) T.:+: T.S " = " T.:+:
  T.E (expr (equat c) sm)

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable ctx => QDefinition -> ctx -> T.Spec
buildDDDescription c m = descLines m
  (if verboseDDDescription then (vars (getQ c $= equat c) m) else [])
  where getQ (EC a _ _) = C a

descLines :: (HasSymbolTable ctx, Quantity q) => ctx -> [q] -> T.Spec
descLines _ []      = error "No chunks to describe"
descLines m (vc:[]) = (T.N (eqSymb vc) T.:+:
  (T.S " is the " T.:+: (spec (phrase $ vc ^. term) m) T.:+:
   unWrp (getUnitLup vc m)))
  where unWrp (Just a) = T.S " (" T.:+: T.Sy (a ^. usymb) T.:+: T.S ")"
        unWrp Nothing  = T.S ""
descLines m (vc:vcs) = descLines m (vc:[]) T.:+: T.HARDNL T.:+: descLines m vcs
