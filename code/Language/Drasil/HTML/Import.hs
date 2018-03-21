module Language.Drasil.HTML.Import(makeDocument,spec) where

import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), BinOp(..), sy, UFunc(..), Oper(..),
    DerivType(..), EOperator(..), ($=), DomainDesc(..), RealRange(..))
import Language.Drasil.Spec
import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.HTML.AST as H

import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Change (chng, chngType, ChngType(..))
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term, getA)
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.SymbolForm (eqSymb)
import Language.Drasil.ChunkDB (HasSymbolTable(..), getUnitLup, symbLookup)
import Language.Drasil.Chunk.ReqChunk (requires)
import Language.Drasil.Chunk.Citation ( CiteField(..), HP(..), Citation 
                                      , externRefT, citeID, fields)
import Language.Drasil.Config (verboseDDDescription)
import Language.Drasil.Document
import Language.Drasil.Expr.Extract
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Reference
import Language.Drasil.Symbol
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Unit (usymb)
import Language.Drasil.Printing.Import (oper,binop,space)

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.List (intersperse)

-- | expr translation function from Drasil to HTML 'AST'
expr :: HasSymbolTable s => Expr -> s -> P.Expr
expr (Dbl d)          _ = P.Dbl   d
expr (Int i)          _ = P.Int   i
expr (Str s)          _ = P.Str   s
expr (Assoc op l)     sm = P.Assoc (oper op) $ map (\x -> expr x sm) l
expr (Deriv Part a b) sm = P.BOp P.Div (P.Assoc P.Mul [P.Font P.Emph $ P.Spec Partial, expr a sm])
                          (P.Assoc P.Mul [P.Font P.Emph $ P.Spec Partial, P.Font P.Emph $ symbol $ eqSymb $ symbLookup b $ sm^.symbolTable])
expr (Deriv Total a b)sm = P.BOp P.Div (P.Assoc P.Mul [P.Font P.Emph $ P.Ident "d", expr a sm])
                          (P.Assoc P.Mul [P.Font P.Emph $ P.Ident "d", P.Font P.Emph $ symbol $ eqSymb $ symbLookup b $ sm^.symbolTable])
expr (C c)            sm = P.Font P.Emph $ symbol $ eqSymb $ symbLookup c $ sm^.symbolTable
expr (FCall f x)      sm = P.Row [expr f sm, 
  P.Fenced P.Paren P.Paren $ P.Row $ intersperse (P.MO P.Comma) $ map (flip expr sm) x]
expr (Case ps)        sm = if length ps < 2 then
                    error "Attempting to use multi-case expr incorrectly"
                    else P.Case (zip (map (flip expr sm . fst) ps) (map (flip expr sm . snd) ps))
expr (Matrix a)         sm = P.Mtx $ map (map (flip expr sm)) a
expr (UnaryOp Log u)    sm = mkCall sm P.Log u
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
expr (UnaryOp Sqrt u)   sm = mkCall sm P.Sqrt u
expr (UnaryOp Neg u)    sm = neg sm u
expr (BinaryOp Frac a b) sm = P.BOp P.Div (expr a sm) (expr b sm)
expr (BinaryOp Cross a b) sm = mkBOp sm P.Cross a b
expr (BinaryOp Dot a b) sm = mkBOp sm P.Dot a b
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
expr (BinaryOp o a b)   sm = P.BOp (binop o) (expr a sm) (expr b sm)
expr (EOp o)            sm = eop o sm
expr (IsIn  a b)        sm = P.Row  [expr a sm, P.MO P.IsIn, space b]

mkCall :: HasSymbolTable ctx => ctx -> P.Ops -> Expr -> P.Expr
mkCall s o e = P.Row [P.MO o, P.Fenced P.Paren P.Paren $ expr e s]

mkBOp :: HasSymbolTable ctx => ctx -> P.Ops -> Expr -> Expr -> P.Expr
mkBOp sm o a b = P.Row [expr a sm, P.MO o, expr b sm]

-- | Helper for properly rendering negation of expressions
neg' :: Expr -> Bool
neg' (Dbl     _)     = True
neg' (Int     _)     = True
neg' (EOp _)         = True
neg' (Assoc Mul _)   = True
neg' (BinaryOp Index _ _) = True
neg' (UnaryOp _ _)   = True
neg' (C _)           = True
neg' _               = False

neg :: HasSymbolTable s => s -> Expr -> P.Expr
neg sm a = if (neg' a) then 
             P.Row [P.MO P.Neg, expr a sm]
           else 
             P.Row [P.MO P.Neg, P.Fenced P.Paren P.Paren $ expr a sm]

-- | For printing indexes
indx :: HasSymbolTable ctx => ctx -> Expr -> Expr -> P.Expr
indx sm (C c) i = f s
  where
    i' = expr i sm
    s = eqSymb $ symbLookup c $ sm^.symbolTable
    f (Corners [] [] [] [b] e) = 
      let e' = P.Font P.Emph $ symbol e
          b' = P.Font P.Emph $ symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(Atomic _) = P.Row [P.Font P.Emph $ symbol a, P.Sub i']
    f a@(Greek _)  = P.Row [P.Font P.Emph $ symbol a, P.Sub i']
    f   e          = let e' = P.Font P.Emph $ symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [expr a sm], P.Sub $ expr i sm]

-- | Helper function for translating 'EOperator's
eop :: HasSymbolTable s => EOperator -> s -> P.Expr
eop (Summation (IntegerDD v (BoundedR l h)) e) sm =
  P.Funct (P.Summation (Just ((v, expr l sm), expr h sm))) (expr e sm)
eop (Summation (All _) e) sm = P.Funct (P.Summation Nothing) (expr e sm)
eop (Summation(RealDD _ _) _) _ = error "HTML/Import.hs Summation cannot be over Real"
eop (Product (IntegerDD v (BoundedR l h)) e) sm =
  P.Funct (P.Product (Just ((v, expr l sm), expr h sm))) (expr e sm)
eop (Product (All _) e) sm = P.Funct (P.Product Nothing) (expr e sm)
eop (Product (RealDD _ _) _) _ = error "HTML/Import.hs Product cannot be over Real"
eop (Integral (RealDD v (BoundedR l h)) e) sm =
  P.Funct (P.Integral (Just (expr l sm), Just (expr h sm)) v) (expr e sm)
eop (Integral (All v) e) sm =
  P.Funct (P.Integral (Just $ P.Font P.Emph $ symbol v, Nothing) v) (expr e sm)
eop (Integral (IntegerDD _ _) _) _ =
  error "HTML/Import.hs Integral cannot be over Integers"

symbol :: Symbol -> P.Expr
symbol (Atomic s)  = P.Ident s
symbol (Special s) = P.Spec s
symbol (Greek g)   = P.Gr g
symbol (Concat sl) = P.Row $ map symbol sl
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = P.Row [P.Row [symbol s], P.Sup $ symbol x]
symbol (Corners [] [] [] [x] s) = P.Row [P.Row [symbol s], P.Sub $ symbol x]
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

-- | Translates Sentence to the HTML representation of Sentence ('Spec')
spec :: HasSymbolTable s => Sentence -> s -> P.Spec
spec (S s)      _ = P.S s
spec (Sy s)     _ = P.Sy s
spec (EmptyS :+: b) sm = spec b sm
spec (a :+: EmptyS) sm = spec a sm
spec (a :+: b) sm = spec a sm P.:+: spec b sm
spec (G g)      _ = P.G g
spec (Sp s)     _ = P.Sp s
spec (P s)      _ = P.N s
spec (F f s)    sm = spec (accent f s) sm
spec (Ref t r n) sm = P.Ref t r (spec n sm)
spec (Quote q) sm = P.S "&quot;" P.:+: spec q sm P.:+: P.S "&quot;"
spec EmptyS     _ = P.EmptyS
spec (E e)     sm = P.E $ expr e sm

-- | Helper function for translating accented characters to
-- an HTML renderable form.
accent :: Accent -> Char -> Sentence
accent Grave  s = S $ '&' : s : "grave;" --Only works on vowels.
accent Acute  s = S $ '&' : s : "acute;" --Only works on vowels.

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
  (P.S (refAdd x))

-- | Translates from Contents to the HTML Representation of LayoutObj.
-- Called internally by layout.
lay :: HasSymbolTable s => Contents -> s -> H.LayoutObj
lay x@(Table hdr lls t b _) sm = H.Table ["table"]
  ((map (flip spec sm) hdr) : (map (map (flip spec sm)) lls)) (P.S (refAdd x)) b (spec t sm)
lay (Paragraph c)       sm = H.Paragraph (spec c sm)
lay (EqnBlock c _)      sm = H.HDiv ["equation"] [H.Tagless (P.E (expr c sm))] (P.EmptyS)
                              -- FIXME: Make equations referable
--lay (CodeBlock c)        = H.CodeBlock c
lay x@(Definition c)    sm = H.Definition c (makePairs c sm) (P.S (refAdd x))
lay (Enumeration cs)    sm = H.List $ makeL cs sm
lay x@(Figure c f wp _) sm = H.Figure (P.S (refAdd x)) (spec c sm) f wp
lay (Graph _ _ _ _ _)    _ = H.Paragraph (P.EmptyS)  -- FIXME: need to implement!
lay x@(Requirement r)   sm = H.ALUR H.Requirement
  (spec (requires r) sm) (P.S (refAdd x)) (spec (fromJust $ getShortName r) sm)
lay x@(Assumption a)    sm = H.ALUR H.Assumption
  (spec (assuming a) sm) (P.S (refAdd x)) (spec (fromJust $ getShortName a) sm)
lay x@(Change lc) sm = H.ALUR 
  (if (chngType lc) == Likely then H.LikelyChange else H.UnlikelyChange)
  (spec (chng lc) sm) (P.S (refAdd x)) (spec (fromJust $ getShortName lc) sm)
lay (Defnt dtyp pairs rn) sm = H.Definition dtyp (layPairs pairs) (P.S rn)
  where layPairs = map (\(x,y) -> (x, (map (\z -> lay z sm) y)))
lay (Bib bib)           sm = H.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable s => s -> Citation -> P.Citation
layCite sm c = P.Cite (citeID c) (externRefT c) (map (layField sm) (fields c))

layField :: HasSymbolTable s => s -> CiteField -> P.CiteField
layField sm (Address      s) = P.Address      $ spec s sm
layField  _ (Author       p) = P.Author       p
layField sm (BookTitle    s) = P.BookTitle    $ spec s sm
layField  _ (Chapter      i) = P.Chapter      i
layField  _ (Edition      n) = P.Edition      n
layField  _ (Editor       p) = P.Editor       p
layField sm (Institution  i) = P.Institution  $ spec i sm
layField sm (Journal      s) = P.Journal      $ spec s sm
layField  _ (Month        m) = P.Month        m
layField sm (Note         s) = P.Note         $ spec s sm
layField  _ (Number       n) = P.Number       n
layField sm (Organization i) = P.Organization $ spec i sm
layField  _ (Pages        n) = P.Pages        n
layField sm (Publisher    s) = P.Publisher    $ spec s sm
layField sm (School       s) = P.School       $ spec s sm
layField sm (Series       s) = P.Series       $ spec s sm
layField sm (Title        s) = P.Title        $ spec s sm
layField sm (Type         t) = P.Type         $ spec t sm
layField  _ (Volume       n) = P.Volume       n
layField  _ (Year         n) = P.Year         n
layField sm (HowPublished (URL  s)) = P.HowPublished (P.URL  $ spec s sm)
layField sm (HowPublished (Verb s)) = P.HowPublished (P.Verb $ spec s sm)

-- | Translates lists
makeL :: HasSymbolTable s => ListType -> s -> P.ListType
makeL (Bullet bs)      sm = P.Unordered   $ map (flip item sm) bs
makeL (Numeric ns)     sm = P.Ordered     $ map (flip item sm) ns
makeL (Simple ps)      sm = P.Simple      $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Desc ps)        sm = P.Desc        $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Definitions ps) sm = P.Definitions $ map (\(x,y) -> (spec x sm, item y sm)) ps

-- | Helper for translating list items
item :: HasSymbolTable s => ItemType -> s -> P.ItemType
item (Flat i)     sm = P.Flat (spec i sm)
item (Nested t s) sm = P.Nested (spec t sm) (makeL s sm)

-- | Translates definitions
-- (Data defs, General defs, Theoretical models, etc.)
makePairs :: HasSymbolTable s => DType -> s -> [(String,[H.LayoutObj])]
makePairs (Data c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "DD") $ fmap S $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Units",       [H.Paragraph $ spec (unit'2Contents c) m]),
  ("Equation",    [H.HDiv ["equation"] [H.Tagless (buildEqn c m)] (P.EmptyS)]),
  ("Description", [H.Paragraph (buildDDDescription c m)])
  ]
makePairs (Theory c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "T") $ fmap S $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Equation",    [H.HDiv ["equation"] [H.Tagless (P.E (expr (c ^. relat) m))]
                  (P.EmptyS)]),
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
buildEqn :: HasSymbolTable s => QDefinition -> s -> P.Spec  
buildEqn c sm = P.N (eqSymb c) P.:+: P.S " = " P.:+: 
  P.E (expr (c^.equat) sm)

-- | Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable s => QDefinition -> s -> P.Spec
buildDDDescription c m = descLines
  (if verboseDDDescription then (vars (getQ c $= c^.equat) m) else []) m
  where getQ (EC a _ _) = sy a

-- | Helper for building each line of the description of a data def
descLines :: (HasSymbolTable s, Quantity q) => [q] -> s -> P.Spec  
descLines []    _   = error "No chunks to describe"
descLines (vc:[]) m = (P.N (eqSymb vc) P.:+: 
  (P.S " is the " P.:+: (spec (phrase $ vc ^. term) m) P.:+:
   unWrp (getUnitLup vc m)))
  where unWrp (Just a) = P.S " (" P.:+: P.Sy (a ^. usymb) P.:+: P.S ")"
        unWrp Nothing  = P.S ""
descLines (vc:vcs) m = descLines (vc:[]) m P.:+: P.HARDNL P.:+: descLines vcs m
