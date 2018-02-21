module Language.Drasil.HTML.Import where

import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), UFunc(..), BiFunc(..), Oper(..),
    DerivType(..), EOperator(..), ($=), DomainDesc(..), RealRange(..))
import Language.Drasil.Spec
import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.HTML.AST as H

import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Change (chng)
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term, getA)
import Language.Drasil.Chunk.Quantity (Quantity(..), eqSymb)
import Language.Drasil.Chunk.ReqChunk (requires)
import Language.Drasil.ChunkDB (HasSymbolTable(..), getUnitLup, symbLookup)
import Language.Drasil.Chunk.Citation ( CiteField(..), HP(..), Citation 
                                      , externRefT, citeID, fields)
import Language.Drasil.Config (verboseDDDescription)
import Language.Drasil.Document
import Language.Drasil.Expr.Extract
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Reference
import Language.Drasil.Symbol
import Language.Drasil.SymbolAlphabet (lD)
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Unit (usymb)

import Control.Lens ((^.))
import Data.Maybe (fromJust)

-- | expr translation function from Drasil to HTML 'AST'
expr :: HasSymbolTable s => Expr -> s -> P.Expr
expr (V v)            _ = P.Var   v
expr (Dbl d)          _ = P.Dbl   d
expr (Int i)          _ = P.Int   i
expr (Assoc op l)     sm = P.Assoc op $ map (\x -> expr x sm) l
expr (Deriv Part a b) sm = P.BOp P.Frac (P.Assoc Mul [P.Sym (Special Partial), expr a sm])
                          (P.Assoc Mul [P.Sym (Special Partial), expr (C b) sm])
expr (Deriv Total a b)sm = P.BOp P.Frac (P.Assoc Mul [P.Sym lD, expr a sm])
                          (P.Assoc Mul [P.Sym lD, expr (C b) sm])
expr (C c)            sm = -- FIXME: Add Stage for Context
  P.Sym $ (eqSymb (symbLookup c (sm ^. symbolTable)))
expr (FCall f x)      sm = P.Call (expr f sm) (map (flip expr sm) x)
expr (Case ps)        sm = if length ps < 2 then
                    error "Attempting to use multi-case expr incorrectly"
                    else P.Case (zip (map (flip expr sm . fst) ps) (map (flip expr sm . snd) ps))
expr (Matrix a)         sm = P.Mtx $ map (map (flip expr sm)) a
expr (UnaryOp u)        sm = (\(x,y) -> P.Op x [y]) $ ufunc u sm
expr (Grouping e)       sm = P.Grouping (expr e sm)
expr (BinaryOp b)       sm = bfunc b sm
expr (EOp o)            sm = (\(x,y) -> P.Op x [y]) $ eop o sm
expr (IsIn  a b)        sm = P.IsIn  (expr a sm) b

-- | Helper function for translating 'UFunc's
ufunc :: HasSymbolTable s => UFunc -> s -> (P.Function, P.Expr)
ufunc (Abs e) sm = (P.Abs, expr e sm)
ufunc (Norm e) sm = (P.Norm, expr e sm)
ufunc (Log e) sm = (P.Log, expr e sm)
ufunc (Sin e)    sm = (P.Sin,  expr e sm)
ufunc (Cos e)    sm = (P.Cos,  expr e sm)
ufunc (Tan e)    sm = (P.Tan,  expr e sm)
ufunc (Sec e)    sm = (P.Sec,  expr e sm)
ufunc (Csc e)    sm = (P.Csc,  expr e sm)
ufunc (Cot e)    sm = (P.Cot,  expr e sm)
ufunc (Exp e)    sm = (P.Exp,  expr e sm)
ufunc (Sqrt e)   sm = (P.Sqrt, expr e sm)
ufunc (Not a)    sm = (P.Not,  expr a sm)
ufunc (Neg a)    sm = (P.Neg,  expr a sm)
ufunc (Dim a)    sm = (P.Dim,  expr a sm)

-- | Helper function for translating 'BiFunc's
bfunc :: HasSymbolTable s => BiFunc -> s -> P.Expr
bfunc (Cross e1 e2)      sm = P.BOp P.Cross (expr e1 sm) (expr e2 sm)
bfunc (Power a b)        sm = P.BOp P.Pow (expr a sm) (expr b sm)
bfunc (EEquals a b)      sm = P.BOp P.Eq  (expr a sm) (expr b sm)
bfunc (ENEquals a b)     sm = P.BOp P.NEq (expr a sm) (expr b sm)
bfunc (ELess a b)        sm = P.BOp P.Lt  (expr a sm) (expr b sm)
bfunc (EGreater a b)     sm = P.BOp P.Gt  (expr a sm) (expr b sm)
bfunc (ELessEq a b)      sm = P.BOp P.LEq (expr a sm) (expr b sm)
bfunc (EGreaterEq a b)   sm = P.BOp P.GEq (expr a sm) (expr b sm)
bfunc (Implies a b)      sm = P.BOp P.Impl (expr a sm) (expr b sm)
bfunc (IFF a b)          sm = P.BOp P.Iff  (expr a sm) (expr b sm)
bfunc (DotProduct a b)   sm = P.BOp P.Dot  (expr a sm) (expr b sm)
bfunc (Subtract a b)     sm = P.BOp P.Sub  (expr a sm) (expr b sm)
bfunc (Divide a b)       sm = P.BOp P.Frac (replace_divs a sm) (replace_divs b sm)
bfunc (Index a i)        sm = P.BOp P.Index (expr a sm) (expr i sm)

-- | Helper function for translating 'EOperator's
eop :: HasSymbolTable s => EOperator -> s -> (P.Function, P.Expr)
eop (Summation (IntegerDD v (BoundedR l h)) e) sm =
  (P.Summation (Just ((v, expr l sm), expr h sm)), (expr e sm))
eop (Summation (All _) e) sm = (P.Summation Nothing,(expr e sm))
eop (Summation(RealDD _ _) _) _ = error "HTML/Import.hs Summation cannot be over Real"
eop (Product (IntegerDD v (BoundedR l h)) e) sm =
  (P.Product (Just ((v, expr l sm), expr h sm)), expr e sm)
eop (Product (All _) e) sm = (P.Product Nothing, (expr e sm))
eop (Product (RealDD _ _) _) _ = error "HTML/Import.hs Product cannot be over Real"
eop (Integral (RealDD v (BoundedR l h)) e) sm =
  (P.Integral (Just (expr l sm), Just (expr h sm)) v, expr e sm)
eop (Integral (All v) e) sm =
  (P.Integral (Just (P.Sym v), Nothing) v, expr e sm)
eop (Integral (IntegerDD _ _) _) _ =
  error "HTML/Import.hs Integral cannot be over Integers"


-- | Helper function for translating the differential
int_wrt :: Symbol -> P.Expr
int_wrt wrtc = P.Assoc Mul [P.Sym lD, P.Sym wrtc]

-- | Helper function for translating operations in expressions
replace_divs :: HasSymbolTable s => Expr -> s -> P.Expr
replace_divs (BinaryOp (Divide a b)) sm = P.BOp P.Div (replace_divs a sm) (replace_divs b sm)
replace_divs (Assoc op l)           sm = P.Assoc op $ map (\x -> replace_divs x sm) l
replace_divs (BinaryOp (Power a b)) sm = P.BOp P.Pow (replace_divs a sm) (replace_divs b sm)
replace_divs (BinaryOp (Subtract a b)) sm = P.BOp P.Sub (replace_divs a sm) (replace_divs b sm)
replace_divs a                      sm = expr a sm

-- | Translates Sentence to the HTML representation of Sentence ('Spec')
spec :: HasSymbolTable s => Sentence -> s -> H.Spec
spec (S s)      _ = H.S s
spec (Sy s)     _ = H.Sy s
spec (EmptyS :+: b) sm = spec b sm
spec (a :+: EmptyS) sm = spec a sm
spec (a :+: b) sm = spec a sm H.:+: spec b sm
spec (G g)      _ = H.G g
spec (Sp s)     _ = H.Sp s
spec (P s)      _ = H.N s
spec (F f s)    sm = spec (accent f s) sm
spec (Ref _ r n) sm = H.Ref r (spec n sm)
spec (Quote q) sm = H.S "&quot;" H.:+: spec q sm H.:+: H.S "&quot;"
spec EmptyS     _ = H.EmptyS
spec (E e)     sm = H.E $ expr e sm

-- | Helper function for translating accented characters to
-- an HTML renderable form.
accent :: Accent -> Char -> Sentence
accent Grave  s = S $ '&' : s : "grave;" --Only works on vowels.
accent Acute  s = S $ '&' : s : "acute;" --Only works on vowels.

-- | Helper function for translating decorated characters to
-- an HTML renderable form.
decorate :: Decoration -> Sentence -> Sentence
decorate Hat    s = s :+: S "&#770;"
decorate Vector s = S "<b>" :+: s :+: S "</b>"
decorate Prime  s = s :+: S "&prime;"

-- | Translates from Document to the HTML representation of Document
makeDocument :: HasSymbolTable s => Document -> s -> H.Document
makeDocument (Document title author sections) sm =
  H.Document (spec title sm) (spec author sm) (createLayout sections sm)

-- | Translates from LayoutObj to the HTML representation of LayoutObj
layout :: HasSymbolTable s => Int -> SecCons -> s -> H.LayoutObj
layout currDepth (Sub s) sm = sec (currDepth+1) s sm
layout _         (Con c) sm = lay c sm

-- | Helper function for creating sections as layout objects
createLayout :: HasSymbolTable s => [Section] -> s -> [H.LayoutObj]
createLayout secs sm = map (flip (sec 0) sm) secs

-- | Helper function for creating sections at the appropriate depth
sec :: HasSymbolTable s => Int -> Section -> s -> H.LayoutObj
sec depth x@(Section title contents _) sm =
  H.HDiv [(concat $ replicate depth "sub") ++ "section"]
  ((H.Header (depth+2) (spec title sm)):(map (flip (layout depth) sm) contents))
  (H.S (refAdd x))

-- | Translates from Contents to the HTML Representation of LayoutObj.
-- Called internally by layout.
lay :: HasSymbolTable s => Contents -> s -> H.LayoutObj
lay x@(Table hdr lls t b _) sm = H.Table ["table"]
  ((map (flip spec sm) hdr) : (map (map (flip spec sm)) lls)) (H.S (refAdd x)) b (spec t sm)
lay (Paragraph c)       sm = H.Paragraph (spec c sm)
lay (EqnBlock c _)      sm = H.HDiv ["equation"] [H.Tagless (H.E (expr c sm))] (H.EmptyS)
                              -- FIXME: Make equations referable
--lay (CodeBlock c)        = H.CodeBlock c
lay x@(Definition c)    sm = H.Definition c (makePairs c sm) (H.S (refAdd x))
lay (Enumeration cs)    sm = H.List $ makeL cs sm
lay x@(Figure c f wp _) sm = H.Figure (H.S (refAdd x)) (spec c sm) f wp
lay (Graph _ _ _ _ _)    _ = H.Paragraph (H.EmptyS)  -- FIXME: need to implement!
lay x@(Requirement r)   sm = H.ALUR H.Requirement
  (spec (requires r) sm) (H.S (refAdd x)) (spec (fromJust $ getShortName r) sm)
lay x@(Assumption a)    sm = H.ALUR H.Assumption
  (spec (assuming a) sm) (H.S (refAdd x)) (spec (fromJust $ getShortName a) sm)
lay x@(LikelyChange lc) sm =
  H.ALUR H.LikelyChange (spec (chng lc) sm) (H.S (refAdd x)) (spec (fromJust $ getShortName lc) sm)
lay x@(UnlikelyChange uc) sm =
  H.ALUR H.UnlikelyChange (spec (chng uc) sm) (H.S (refAdd x)) (spec (fromJust $ getShortName uc) sm)
lay (Defnt dtyp pairs rn) sm = H.Definition dtyp (layPairs pairs) (H.S rn)
  where layPairs = map (\(x,y) -> (x, (map (\z -> lay z sm) y)))
lay (Bib bib)           sm = H.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable s => s -> Citation -> H.Citation
layCite sm c = H.Cite (citeID c) (externRefT c) (map (layField sm) (fields c))

layField :: HasSymbolTable s => s -> CiteField -> H.CiteField
layField sm (Address      s) = H.Address      $ spec s sm
layField  _ (Author       p) = H.Author       p
layField sm (BookTitle    s) = H.BookTitle    $ spec s sm
layField  _ (Chapter      i) = H.Chapter      i
layField  _ (Edition      n) = H.Edition      n
layField  _ (Editor       p) = H.Editor       p
layField sm (Institution  i) = H.Institution  $ spec i sm
layField sm (Journal      s) = H.Journal      $ spec s sm
layField  _ (Month        m) = H.Month        m
layField sm (Note         s) = H.Note         $ spec s sm
layField  _ (Number       n) = H.Number       n
layField sm (Organization i) = H.Organization $ spec i sm
layField  _ (Pages        n) = H.Pages        n
layField sm (Publisher    s) = H.Publisher    $ spec s sm
layField sm (School       s) = H.School       $ spec s sm
layField sm (Series       s) = H.Series       $ spec s sm
layField sm (Title        s) = H.Title        $ spec s sm
layField sm (Type         t) = H.Type         $ spec t sm
layField  _ (Volume       n) = H.Volume       n
layField  _ (Year         n) = H.Year         n
layField sm (HowPublished (URL  s)) = H.HowPublished (H.URL  $ spec s sm)
layField sm (HowPublished (Verb s)) = H.HowPublished (H.Verb $ spec s sm)

-- | Translates lists
makeL :: HasSymbolTable s => ListType -> s -> H.ListType
makeL (Bullet bs)      sm = H.Unordered   $ map (flip item sm) bs
makeL (Numeric ns)      sm = H.Ordered     $ map (flip item sm) ns
makeL (Simple ps)      sm = H.Simple      $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Desc ps)        sm = H.Desc        $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Definitions ps) sm = H.Definitions $ map (\(x,y) -> (spec x sm, item y sm)) ps

-- | Helper for translating list items
item :: HasSymbolTable s => ItemType -> s -> H.ItemType
item (Flat i)     sm = H.Flat (spec i sm)
item (Nested t s) sm = H.Nested (spec t sm) (makeL s sm)

-- | Translates definitions
-- (Data defs, General defs, Theoretical models, etc.)
makePairs :: HasSymbolTable s => DType -> s -> [(String,[H.LayoutObj])]
makePairs (Data c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "DD") $ fmap S $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Units",       [H.Paragraph $ spec (unit'2Contents c) m]),
  ("Equation",    [H.HDiv ["equation"] [H.Tagless (buildEqn c m)] (H.EmptyS)]),
  ("Description", [H.Paragraph (buildDDDescription c m)])
  ]
makePairs (Theory c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "T") $ fmap S $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Equation",    [H.HDiv ["equation"] [H.Tagless (H.E (expr (c ^. relat) m))]
                  (H.EmptyS)]),
  ("Description", [H.Paragraph (spec (c ^. defn) m)])
  ]
makePairs General  _ = error "Not yet implemented"
makePairs Instance _ = error "Not yet implemented"
makePairs TM _       = error "Not yet implemented"
makePairs DD _       = error "Not yet implemented"

missingAcro :: Sentence -> Maybe Sentence -> Sentence
missingAcro dflt Nothing = S "<b>":+: dflt :+: S "</b>"
missingAcro _ (Just a) = S "<b>":+: a :+: S "</b>"

-- | Translates the defining equation from a QDefinition to
-- HTML's version of Sentence
buildEqn :: HasSymbolTable s => QDefinition -> s -> H.Spec
buildEqn c sm = H.N (eqSymb c) H.:+: H.S " = " H.:+:
  H.E (expr (equat c) sm)

-- | Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable s => QDefinition -> s -> H.Spec
buildDDDescription c m = descLines
  (if verboseDDDescription then (vars (getQ c $= equat c) m) else []) m
  where getQ (EC a _ _) = C a

-- | Helper for building each line of the description of a data def
descLines :: (HasSymbolTable s, Quantity q) => [q] -> s -> H.Spec
descLines []    _   = error "No chunks to describe"
descLines (vc:[]) m = (H.N (eqSymb vc) H.:+:
  (H.S " is the " H.:+: (spec (phrase $ vc ^. term) m) H.:+:
   unWrp (getUnitLup vc m)))
  where unWrp (Just a) = H.S " (" H.:+: H.Sy (a ^. usymb) H.:+: H.S ")"
        unWrp Nothing  = H.S ""
descLines (vc:vcs) m = descLines (vc:[]) m H.:+: H.HARDNL H.:+: descLines vcs m
