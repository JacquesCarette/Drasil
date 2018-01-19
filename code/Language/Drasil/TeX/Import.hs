module Language.Drasil.TeX.Import where

import Control.Lens hiding ((:>),(:<),set)
import Prelude hiding (id)

import qualified Language.Drasil.TeX.AST as T

import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), BiFunc(..),
    DerivType(..), EOperator(..), ($=), RealRange(..), DomainDesc(..))
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term)
import Language.Drasil.Chunk.Quantity (Quantity(..), eqSymb)
import Language.Drasil.Chunk.ReqChunk (requires)
import Language.Drasil.ChunkDB (getUnitLup, symbLookup, HasSymbolTable(..))
import Language.Drasil.Citations (Citation(..),CiteField(..))
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Expr.Extract
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Reference
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec
import Language.Drasil.Symbol
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Unit (usymb)

expr :: HasSymbolTable ctx => Expr -> ctx -> T.Expr
expr (V v)              _ = T.Var  v
expr (Dbl d)            _ = T.Dbl  d
expr (Int i)            _ = T.Int  i
expr (a :* b)          sm = T.Mul  (expr a sm) (expr b sm)
expr (a :+ b)          sm = T.Add  (expr a sm) (expr b sm)
expr (a :/ b)          sm = T.Frac (replace_divs a sm) (replace_divs b sm)
expr (a :^ b)          sm = T.Pow  (expr a sm) (expr b sm)
expr (a :- b)          sm = T.Sub  (expr a sm) (expr b sm)
expr (a :. b)          sm = T.Dot  (expr a sm) (expr b sm)
expr (Neg a)           sm = T.Neg  (expr a sm)
expr (C c)             sm = -- FIXME: Add Stage for Context
  T.Sym  (eqSymb (symbLookup c (sm ^. symbolTable)))
expr (Deriv Part a 1)  sm = T.Mul (T.Sym (Special Partial)) (expr a sm)
expr (Deriv Total a 1) sm = T.Mul (T.Sym lD) (expr a sm)
expr (Deriv Part a b)  sm = T.Frac (T.Mul (T.Sym (Special Partial)) (expr a sm))
                           (T.Mul (T.Sym (Special Partial)) (expr b sm))
expr (Deriv Total a b) sm = T.Frac (T.Mul (T.Sym lD) (expr a sm))
                           (T.Mul (T.Sym lD) (expr b sm))
expr (FCall f x)       sm = T.Call (expr f sm) (map (flip expr sm) x)
expr (Case ps)         sm = if length ps < 2 then 
        error "Attempting to use multi-case expr incorrectly"
        else T.Case (zip (map (flip expr sm . fst) ps) (map (flip rel sm . snd) ps))
expr x@(EEquals _ _)    sm = rel x sm
expr x@(ENEquals _ _)   sm = rel x sm
expr x@(EGreater _ _)   sm = rel x sm
expr x@(ELess _ _)      sm = rel x sm
expr x@(ELessEq _ _)    sm = rel x sm
expr x@(EGreaterEq _ _) sm = rel x sm
expr (Matrix a)        sm = T.Mtx $ map (map (flip expr sm)) a
expr (Index a i)       sm = T.Index (expr a sm) (expr i sm)
expr (UnaryOp u)       sm = (\(x,y) -> T.Op x [y]) (ufunc u sm)
expr (Grouping e)      sm = T.Grouping (expr e sm)
expr (BinaryOp b)      sm = (\(x,y) -> T.Op x y) (bfunc b sm)
expr (EOp o)           sm = (\(x,y) -> T.Op x [y]) (eop o sm)
expr (Not a)           sm = T.Not  (expr a sm)
expr (a :&& b)         sm = T.And  (expr a sm) (expr b sm)
expr (a :|| b)         sm = T.Or   (expr a sm) (expr b sm)
expr (a  :=>  b)       sm = T.Impl  (expr a sm) (expr b sm)
expr (a  :<=> b)       sm = T.Iff   (expr a sm) (expr b sm)
expr (IsIn  a b)       sm = T.IsIn  (expr a sm) (set b)
expr (ForAll a b)      sm = T.Forall a (expr b sm)
expr (Exists a b)      sm = T.Exists a (expr b sm)
expr _                 _  = error "Expression unimplemented in TeX"

ufunc :: HasSymbolTable ctx => UFunc -> ctx -> (T.Function, T.Expr)
ufunc (Log e) sm = (T.Log, expr e sm)
ufunc (Abs e) sm = (T.Abs, expr e sm)
ufunc (Norm e) sm = (T.Norm, expr e sm)
ufunc (Sin e) sm = (T.Sin, expr e sm)
ufunc (Cos e) sm = (T.Cos, expr e sm)
ufunc (Tan e) sm = (T.Tan, expr e sm)
ufunc (Sec e) sm = (T.Sec, expr e sm)
ufunc (Csc e) sm = (T.Csc, expr e sm)
ufunc (Cot e) sm = (T.Cot, expr e sm)
ufunc (Exp e) sm = (T.Exp, expr e sm)
ufunc (Sqrt e) sm = (T.Sqrt, expr e sm)

bfunc :: HasSymbolTable ctx => BiFunc -> ctx -> (T.Function, [T.Expr])
bfunc (Cross e1 e2) sm = (T.Cross, map (flip expr sm) [e1,e2])

eop :: HasSymbolTable ctx => EOperator -> ctx -> (T.Function, T.Expr)
eop (Summation (IntegerDD v (BoundedR l h)) e) sm =
  (T.Summation (Just ((v, expr l sm), expr h sm)), (expr e sm))
eop (Summation (All _) e) sm = (T.Summation Nothing,(expr e sm))
eop (Summation(RealDD _ _) _) _ = error "TeX/Import.hs Summation cannot be over Real"
eop (Product (IntegerDD v (BoundedR l h)) e) sm = 
  (T.Product (Just ((v, expr l sm), expr h sm)), expr e sm)
eop (Product (All _) e) sm = (T.Product Nothing, (expr e sm))
eop (Product (RealDD _ _) _) _ = error "TeX/Import.hs Product cannot be over Real"
eop (Integral (RealDD v (BoundedR l h)) e) sm = 
  (T.Integral (Just (expr l sm), Just (expr h sm)) v, expr e sm)
eop (Integral (All v) e) sm = 
  (T.Integral (Just (T.Sym v), Nothing) v, expr e sm)
eop (Integral (IntegerDD _ _) _) _ = 
  error "TeX/Import.hs Integral cannot be over Integers"

rel :: HasSymbolTable ctx => Relation -> ctx -> T.Expr
rel (EEquals a b)    sm = T.Eq  (expr a sm) (expr b sm)
rel (ENEquals a b)   sm = T.NEq (expr a sm) (expr b sm)
rel (ELess a b)      sm = T.Lt  (expr a sm) (expr b sm)
rel (EGreater a b)   sm = T.Gt  (expr a sm) (expr b sm)
rel (ELessEq a b)    sm = T.LEq (expr a sm) (expr b sm)
rel (EGreaterEq a b) sm = T.GEq (expr a sm) (expr b sm)
rel _ _ = error "Attempting to use non-Relation Expr in relation context."

-- | Helper for translating Spaces
set :: Space -> T.Set
set Integer  = T.Integer
set Rational = T.Rational
set Real     = T.Real
set Natural  = T.Natural
set Boolean  = T.Boolean
set Char     = T.Char
set String   = T.String
set Radians  = T.Radians
set (Vect a) = T.Vect (set a)
set (Obj a)  = T.Obj a
set (DiscreteI a) = T.DiscreteI a
set (DiscreteD a) = T.DiscreteD a
set (DiscreteS a) = T.DiscreteS a

int_wrt :: Symbol -> T.Expr
int_wrt wrtc = T.Mul (T.Sym lD) (T.Sym wrtc)

replace_divs :: HasSymbolTable ctx => Expr -> ctx -> T.Expr
replace_divs (a :/ b) sm = T.Div (replace_divs a sm) (replace_divs b sm)
replace_divs (a :+ b) sm = T.Add (replace_divs a sm) (replace_divs b sm)
replace_divs (a :* b) sm = T.Mul (replace_divs a sm) (replace_divs b sm)
replace_divs (a :^ b) sm = T.Pow (replace_divs a sm) (replace_divs b sm)
replace_divs (a :- b) sm = T.Sub (replace_divs a sm) (replace_divs b sm)
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
spec (Ref t r n) sm = T.Ref t (T.S r) (spec n sm)
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
layout currDepth (Sub s) = sec (currDepth+1) s
layout _         (Con c) = lay c

createLayout :: HasSymbolTable ctx => Sections -> ctx -> [T.LayoutObj]
createLayout secs sm = map (flip (sec 0) sm) secs

sec :: HasSymbolTable ctx => Int -> Section -> ctx -> T.LayoutObj
sec depth x@(Section title contents _) sm = 
  T.Section depth (spec title sm) (map (flip (layout depth) sm) contents) (T.S (refAdd x))

lay :: HasSymbolTable ctx => Contents -> ctx -> T.LayoutObj
lay x@(Table hdr lls t b _) sm
  | null lls || length hdr == length (head lls) = T.Table ((map (flip spec sm) hdr) :
      (map (map (flip spec sm)) lls)) (T.S (refAdd x)) b (spec t sm)
  | otherwise = error $ "Attempting to make table with " ++ show (length hdr) ++
                        " headers, but data contains " ++ 
                        show (length (head lls)) ++ " columns."
lay (Paragraph c)         sm = T.Paragraph (spec c sm)
lay (EqnBlock c _)        sm = T.EqnBlock (T.E (expr c sm)) --FIXME: Make eqn referable
--lay (CodeBlock c)         = T.CodeBlock c
lay x@(Definition c)      sm = T.Definition (makePairs c sm) (T.S (refAdd x))
lay (Enumeration cs)      sm = T.List $ makeL cs sm
lay x@(Figure c f wp _)   sm = T.Figure (T.S (refAdd x)) (spec c sm) f wp
lay x@(Requirement r)     sm = 
  T.Requirement (spec (requires r) sm) (T.S (refAdd x))
lay x@(Assumption a)      sm = 
  T.Assumption (spec (assuming a) sm) (T.S (refAdd x))
lay x@(LikelyChange lc)   sm = 
  T.LikelyChange (spec (phrase $ lc ^. term) sm)
  (T.S (refAdd x))
lay x@(UnlikelyChange ucc) sm = 
  T.UnlikelyChange (spec (phrase $ ucc ^. term) sm)
  (T.S (refAdd x))
lay x@(Graph ps w h t _)  sm = T.Graph (map (\(y,z) -> (spec y sm, spec z sm)) ps)
                               w h (spec t sm) (T.S (refAdd x))
lay (Defnt dtyp pairs rn) sm = T.Defnt dtyp (layPairs pairs) (T.S rn)
  where layPairs = map (\(x,y) -> (x, (map (\z -> lay z sm) y))) 
lay (Bib bib)         sm = T.Bib $ map (flip layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable ctx => Citation -> ctx -> T.Citation
layCite (Book      fields) sm = T.Book      $ map (flip layField sm) fields
layCite (Article   fields) sm = T.Article   $ map (flip layField sm) fields
layCite (MThesis   fields) sm = T.MThesis   $ map (flip layField sm) fields
layCite (PhDThesis fields) sm = T.PhDThesis $ map (flip layField sm) fields
layCite (Misc      fields) sm = T.Misc      $ map (flip layField sm) fields
layCite (Online    fields) sm = T.Online    $ map (flip layField sm) fields

layField :: HasSymbolTable ctx => CiteField -> ctx -> T.CiteField
layField (Author     p)  _ = T.Author     p
layField (Title      s) sm = T.Title      $ spec s sm
layField (Series     s) sm = T.Series     $ spec s sm
layField (Collection s) sm = T.Collection $ spec s sm
layField (Volume     n)  _ = T.Volume     n
layField (Edition    n)  _ = T.Edition    n
layField (Place (c, s)) sm = T.Place (spec c sm, spec s sm)
layField (Publisher  s) sm = T.Publisher $ spec s sm
layField (Journal    s) sm = T.Journal   $ spec s sm
layField (Year       n)  _ = T.Year       n
layField (Date    n m y) _ = T.Date    n m y
layField (URLdate n m y) _ = T.URLdate n m y
layField (Page       n)  _ = T.Page       n
layField (Pages     ns)  _ = T.Pages     ns
layField (Note       s) sm = T.Note       $ spec s sm
layField (Issue      n)  _ = T.Issue      n
layField (School     s) sm = T.School     $ spec s sm
layField (URL        n) sm = T.URL        $ spec n sm
layField (HowPub     s) sm = T.HowPub     $ spec s sm
layField (Editor     p)  _ = T.Editor     p

makeL :: HasSymbolTable ctx => ListType -> ctx -> T.ListType  
makeL (Bullet bs)      sm = T.Enum        $ (map (flip item sm) bs)
makeL (Number ns)      sm = T.Item        $ (map (flip item sm) ns)
makeL (Simple ps)      sm = T.Simple      $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Desc ps)        sm = T.Desc        $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Definitions ps) sm = T.Definitions $ map (\(x,y) -> (spec x sm, item y sm)) ps

item :: HasSymbolTable ctx => ItemType -> ctx -> T.ItemType
item (Flat i)     sm = T.Flat   (spec i sm)
item (Nested t s) sm = T.Nested (spec t sm) (makeL s sm) 
  
makePairs :: HasSymbolTable ctx => DType -> ctx -> [(String,[T.LayoutObj])]
makePairs (Data c) m = [
  ("Label",       [T.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Units",       [T.Paragraph $ spec (unit'2Contents c) m]),
  ("Equation",    [eqnStyleDD $ buildEqn c m]),
  ("Description", [T.Paragraph (buildDDDescription c m)])
  ]
makePairs (Theory c) m = [
  ("Label",       [T.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Equation",    [eqnStyleTM $ T.E (rel (c ^. relat) m)]),
  ("Description", [T.Paragraph (spec (c ^. defn) m)])
  ]
makePairs General  _ = error "Not yet implemented"
makePairs Instance _ = error "Not yet implemented"
makePairs TM _       = error "Not yet implemented"
makePairs DD _       = error "Not yet implemented"


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
buildDDDescription c m = descLines 
  (if verboseDDDescription then (vars (getQ c $= equat c) m) else []) m
  where getQ (EC a _ _) = C a

descLines :: (HasSymbolTable ctx, Quantity q) => [q] -> ctx -> T.Spec  
descLines []    _   = error "No chunks to describe"
descLines (vc:[]) m = (T.N (eqSymb vc) T.:+: 
  (T.S " is the " T.:+: (spec (phrase $ vc ^. term) m) T.:+:
   unWrp (getUnitLup vc m)))
  where unWrp (Just a) = T.S " (" T.:+: T.Sy (a ^. usymb) T.:+: T.S ")"
        unWrp Nothing  = T.S ""
descLines (vc:vcs) m = descLines (vc:[]) m T.:+: T.HARDNL T.:+: descLines vcs m


