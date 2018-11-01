module Language.Drasil.Printing.Import(space,expr,symbol,spec,makeDocument) where

import Data.List (intersperse)

import Language.Drasil hiding (sec, symbol)

import Control.Lens ((^.))
import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..),
  Notation(Scientific, Engineering))

import Numeric (floatToDigits)
import Data.Tuple(fst, snd)
-- | Render a Space
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
space (DiscreteS l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map P.Ident l --ex. let Meal = {"breakfast", "lunch", "dinner"}

{-
p_space :: Space -> String
p_space Radians  = "rad"
p_space (Vect a) = "V" ++ p_space a
p_space (DiscreteI a)  = "{" ++ (concat $ intersperse ", " (map show a)) ++ "}"
p_space (DiscreteD a)  = "{" ++ (concat $ intersperse ", " (map show a)) ++ "}"
p_space (DiscreteS a)  = "{" ++ (concat $ intersperse ", " a) ++ "}"
-}

-- | Helper for inserting parentheses.
parens :: P.Expr -> P.Expr
parens = P.Fenced P.Paren P.Paren

mulExpr ::  (HasSymbolTable s, HasPrintingOptions s) => [Expr] -> s -> [P.Expr]
mulExpr (hd1:hd2:tl) sm = case (hd1, hd2) of
  (a, Int _) ->  [expr' sm (precA Mul) a , P.MO P.Dot] ++ (mulExpr (hd2:tl) sm)
  (a, Dbl _) ->  [expr' sm (precA Mul) a , P.MO P.Dot] ++ (mulExpr (hd2:tl) sm)
  (a, _)     ->  [expr' sm (precA Mul) a , P.MO P.Mul] ++ (mulExpr (hd2:tl) sm)
mulExpr (hd:[])      sm = [expr' sm (precA Mul) hd]
mulExpr []       sm     = [expr' sm (precA Mul) (Int 1)]

--This function takes the digits form `floatToDigits` function
-- and decimal point position and a counter and exponent
digitsProcess :: [Integer] -> Int -> Int -> Integer -> [P.Expr]
digitsProcess [0] _ _ _ = [P.Int 0, P.MO P.Point, P.Int 0]
digitsProcess (hd:tl) pos coun ex = if pos /= coun
  then [P.Int hd] ++ (digitsProcess tl pos (coun+1) ex)
  else if ex /= 0
    then [P.MO P.Point, P.Int hd] ++ (map P.Int tl) ++ [P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
    else [P.MO P.Point, P.Int hd] ++ (map P.Int tl)
digitsProcess [] pos coun ex = if pos > coun
  then [P.Int 0] ++ (digitsProcess [] pos (coun+1) ex)
  else if ex /= 0
    then [P.MO P.Point, P.Int 0, P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
    else [P.MO P.Point, P.Int 0]

-- THis function takes the exponent and the [Int] of base and give out
-- the decimal point position and processed exponent
-- This function supports transferring scientific notation to
-- engineering notation.
-- References for standard of Engineering Notation:
-- https://www.khanacademy.org/science/electrical-engineering/introduction-to-ee/
--    intro-to-ee/a/ee-numbers-in-electrical-engineering 
-- https://www.calculatorsoup.com/calculators/math/scientific-notation-converter.php
-- https://en.wikipedia.org/wiki/Scientific_notation
processExpo :: Int -> (Int, Int)
processExpo a 
  | a == 0 = (3, -3)
  | a == 1 = (1, 0)
  | a == 2 = (2, 0)
  | a == -1 = (3, -3) 
  | a == -2 = (1, -3)
  | a > 0 && mod (a-1)  3 == 0 = (1, a-1)
  | a > 0 && mod (a-1)  3 == 1 = (2, a-2)
  | a > 0 && mod (a-1)  3 == 2 = (3, a-3)
  | a < 0 && mod (-a) 3 == 0 = (3, a-3)
  | a < 0 && mod (-a) 3 == 1 = (2, a-2)
  | a < 0 && mod (-a) 3 == 2 = (1, a-1)
  | otherwise = error "The cases of processExpo should be exhaustive!"

-- | expr translation function from Drasil to layout AST
expr :: (HasSymbolTable s, HasPrintingOptions s) => Expr -> s -> P.Expr
expr (Dbl d)           sm = case sm ^. getSetting of
  Engineering -> P.Row $ digitsProcess (map toInteger $ fst $ floatToDigits 10 d)
     (fst $ processExpo $ snd $ floatToDigits 10 d) 0
     (toInteger $ snd $ processExpo $ snd $ floatToDigits 10 d)
  Scientific           ->  P.Dbl d
expr (Int i)            _ = P.Int i
expr (Str s)            _ = P.Str   s
expr (AssocB And l)    sm = P.Row $ intersperse (P.MO P.And) $ map (expr' sm (precB And)) l
expr (AssocB Or l)     sm = P.Row $ intersperse (P.MO P.Or ) $ map (expr' sm (precB Or)) l
expr (AssocA Add l)    sm = P.Row $ intersperse (P.MO P.Add) $ map (expr' sm (precA Add)) l
expr (AssocA Mul l)    sm = P.Row $ mulExpr l sm
expr (Deriv Part a b) sm =
  P.Div (P.Row [P.Spec Partial, P.Spc P.Thin, expr a sm])
        (P.Row [P.Spec Partial, P.Spc P.Thin,
                symbol $ eqSymb $ symbLookup b $ sm ^.symbolTable])
expr (Deriv Total a b)sm =
  P.Div (P.Row [P.Ident "d", P.Spc P.Thin, expr a sm])
        (P.Row [P.Ident "d", P.Spc P.Thin, symbol $ eqSymb $ symbLookup b $ sm^.symbolTable])
expr (C c)            sm = symbol $ lookupC sm c
expr (FCall f [x])    sm = P.Row [expr f sm, parens $ expr x sm]
expr (FCall f l)      sm = P.Row [expr f sm,
  parens $ P.Row $ intersperse (P.MO P.Comma) $ map (flip expr sm) l]
expr (Case ps)        sm = if length ps < 2 then
                    error "Attempting to use multi-case expr incorrectly"
                    else P.Case (zip (map (flip expr sm . fst) ps) (map (flip expr sm . snd) ps))
expr (Matrix a)         sm = P.Mtx $ map (map (flip expr sm)) a
expr (UnaryOp Log u)    sm = mkCall sm P.Log u
expr (UnaryOp Ln u)     sm = mkCall sm P.Ln u
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
expr (UnaryOp Sqrt u)   sm = P.Sqrt $ expr u sm
expr (UnaryOp Neg u)    sm = neg sm u
expr (BinaryOp Frac a b)  sm = P.Div (expr a sm) (expr b sm)
expr (BinaryOp Cross a b) sm = mkBOp sm P.Cross a b
expr (BinaryOp Dot a b)   sm = mkBOp sm P.Dot a b
expr (BinaryOp Eq a b)    sm = mkBOp sm P.Eq a b
expr (BinaryOp NEq a b)   sm = mkBOp sm P.NEq a b
expr (BinaryOp Lt a b)    sm = mkBOp sm P.Lt a b
expr (BinaryOp Gt a b)    sm = mkBOp sm P.Gt a b
expr (BinaryOp LEq a b)   sm = mkBOp sm P.LEq a b
expr (BinaryOp GEq a b)   sm = mkBOp sm P.GEq a b
expr (BinaryOp Impl a b)  sm = mkBOp sm P.Impl a b
expr (BinaryOp Iff a b)   sm = mkBOp sm P.Iff a b
expr (BinaryOp Index a b) sm = indx sm a b
expr (BinaryOp Pow a b)   sm = pow sm a b
expr (BinaryOp Subt a b)  sm = P.Row [expr a sm, P.MO P.Subt, expr b sm]
expr (Operator o dd e)    sm = eop sm o dd e
expr (IsIn  a b)          sm = P.Row  [expr a sm, P.MO P.IsIn, space b]
expr (RealI c ri)         sm = renderRealInt sm (lookupC sm c) ri

lookupC :: HasSymbolTable s => s -> UID -> Symbol
lookupC sm c = eqSymb $ symbLookup c $ sm^.symbolTable

mkCall :: (HasSymbolTable ctx, HasPrintingOptions ctx) => ctx -> P.Ops -> Expr -> P.Expr
mkCall s o e = P.Row [P.MO o, parens $ expr e s]

mkBOp :: (HasSymbolTable ctx, HasPrintingOptions ctx) => ctx -> P.Ops -> Expr -> Expr -> P.Expr
mkBOp sm o a b = P.Row [expr a sm, P.MO o, expr b sm]

expr' :: (HasSymbolTable ctx, HasPrintingOptions ctx) => ctx -> Int -> Expr -> P.Expr
expr' s p e = fence $ expr e s
  where
  fence = if eprec e < p then parens else id

-- | Helper for properly rendering negation of expressions
neg' :: Expr -> Bool
neg' (Dbl     _)          = True
neg' (Int     _)          = True
neg' (Operator _ _ _)     = True
neg' (AssocA Mul _)       = True
neg' (BinaryOp Index _ _) = True
neg' (UnaryOp _ _)        = True
neg' (C _)                = True
neg' _                    = False

-- | Render negated expressions
neg :: (HasSymbolTable s, HasPrintingOptions s) => s -> Expr -> P.Expr
neg sm a = P.Row [P.MO P.Neg, (if neg' a then id else parens) $ expr a sm]

-- | For printing indexes
indx :: (HasSymbolTable ctx, HasPrintingOptions ctx) => ctx -> Expr -> Expr -> P.Expr
indx sm (C c) i = f s
  where
    i' = expr i sm
    s = eqSymb $ symbLookup c $ sm^.symbolTable
    f (Corners [] [] [] [b] e) =
      let e' = symbol e
          b' = symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(Atomic _) = P.Row [symbol a, P.Sub i']
--    f a@(Greek _)  = P.Row [symbol a, P.Sub i']
    f   e          = let e' = symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [expr a sm], P.Sub $ expr i sm]

-- | Helper function for translating 'EOperator's
eop :: (HasSymbolTable s, HasPrintingOptions s) => s -> ArithOper -> DomainDesc Expr Expr -> Expr -> P.Expr
eop sm Mul (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Prod, P.Sub (P.Row [symbol v, P.MO P.Eq, expr l sm]), P.Sup (expr h sm),
         P.Row [expr e sm]]
eop sm Mul (AllDD _ Discrete) e = P.Row [P.MO P.Prod, P.Row[expr e sm]]
eop _  Mul (AllDD _ Continuous) _ = error "Printing/Import.hs Product-Integral not implemented."
eop _  Mul (BoundedDD _ Continuous _ _) _ = error "Printing/Import.hs Product-Integral not implemented."
eop sm Add (BoundedDD v Continuous l h) e =
  P.Row [P.MO P.Inte, P.Sub (expr l sm), P.Sup (expr h sm),
         P.Row [expr e sm], P.Spc P.Thin, P.Ident "d", symbol v]
eop sm Add (AllDD v Continuous) e =
  P.Row [P.MO P.Inte, P.Sub (symbol v), P.Row [expr e sm], P.Spc P.Thin, 
         P.Ident "d", symbol v]
eop sm Add (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Summ, P.Sub (P.Row [symbol v, P.MO P.Eq, expr l sm]), P.Sup (expr h sm),
         P.Row [expr e sm]]
eop sm Add (AllDD _ Discrete) e = P.Row [P.MO P.Summ, P.Row [expr e sm]]

symbol :: Symbol -> P.Expr
symbol (Atomic s)  = P.Ident s
symbol (Special s) = P.Spec s
--symbol (Greek g)   = P.Gr g
symbol (Concat sl) = P.Row $ map symbol sl
--
-- handle the special cases first, then general case
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
pow :: (HasSymbolTable ctx, HasPrintingOptions ctx) => ctx -> Expr -> Expr -> P.Expr
pow sm a@(AssocA Add _)  b = P.Row [parens (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Subt _ _) b = P.Row [parens (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Frac _ _) b = P.Row [parens (expr a sm), P.Sup (expr b sm)]
pow sm a@(AssocA Mul _)  b = P.Row [parens (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Pow _ _)  b = P.Row [parens (expr a sm), P.Sup (expr b sm)]
pow sm a                b = P.Row [expr a sm, P.Sup (expr b sm)]

-- | Print a RealInterval
renderRealInt :: (HasSymbolTable st, HasPrintingOptions st) => st -> Symbol -> RealInterval Expr Expr -> P.Expr
renderRealInt st s (Bounded (Inc,a) (Inc,b)) = 
  P.Row [ expr a st, P.MO P.LEq, symbol s, P.MO P.LEq, expr b st]
renderRealInt st s (Bounded (Inc,a) (Exc,b)) =
  P.Row [ expr a st, P.MO P.LEq, symbol s, P.MO P.Lt, expr b st]
renderRealInt st s (Bounded (Exc,a) (Inc,b)) =
  P.Row [ expr a st, P.MO P.Lt, symbol s, P.MO P.LEq, expr b st]
renderRealInt st s (Bounded (Exc,a) (Exc,b)) =
  P.Row [ expr a st, P.MO P.Lt, symbol s, P.MO P.Lt, expr b st]
renderRealInt st s (UpTo (Inc,a))    = P.Row [ symbol s, P.MO P.LEq, expr a st]
renderRealInt st s (UpTo (Exc,a))    = P.Row [ symbol s, P.MO P.Lt, expr a st]
renderRealInt st s (UpFrom (Inc,a))  = P.Row [ symbol s, P.MO P.GEq, expr a st]
renderRealInt st s (UpFrom (Exc,a))  = P.Row [ symbol s, P.MO P.Gt, expr a st]


-- | Translates Sentence to the Printing representation of Sentence ('Spec')
spec :: (HasSymbolTable s, HasDefinitionTable s, HasPrintingOptions s) =>
  s -> Sentence -> P.Spec
  -- make sure these optimizations are clear
spec sm (EmptyS :+: b) = spec sm b
spec sm (a :+: EmptyS) = spec sm a
spec sm (a :+: b)      = spec sm a P.:+: spec sm b
spec _ (S s)           = P.S s
spec _ (Sy s)          = P.Sy s
spec _ (Sp s)          = P.Sp s
spec _ (P s)           = P.E $ symbol s
spec sm (Ch s)         = P.E $ symbol $ lookupC sm s 
spec sm (Ref (Reference t r sn))   = P.Ref t r (spec sm (S . getStringSN $ resolveSN sn $
  lookupDeferredSN sm)) sn --FIXME: sn passed in twice?
spec sm (Quote q)      = P.Quote $ spec sm q
spec _  EmptyS         = P.EmptyS
spec sm (E e)          = P.E $ expr e sm

lookupDeferredSN :: (HasDefinitionTable ctx) => ctx -> UID -> String
lookupDeferredSN ctx u = maybe "" (\x -> x ++ ": ") $
  getA $ defLookup u $ ctx ^. defTable

-- | Translates from Document to the Printing representation of Document
makeDocument :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> Document -> T.Document
makeDocument sm (Document titleLb authorName sections) =
  T.Document (spec sm titleLb) (spec sm authorName) (createLayout sm sections)

-- | Translates from LayoutObj to the Printing representation of LayoutObj
layout :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> Int -> SecCons -> T.LayoutObj
layout sm currDepth (Sub s) = sec sm (currDepth+1) s
layout sm _         (Con c) = lay sm c

-- | Helper function for creating sections as layout objects
createLayout :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> [Section] -> [T.LayoutObj]
createLayout sm = map (sec sm 0)

-- | Helper function for creating sections at the appropriate depth
sec :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> Int -> Section -> T.LayoutObj
sec sm depth x@(Section titleLb contents _) = --FIXME: should ShortName be used somewhere?
  let ref = P.S (refAdd x) in
  T.HDiv [(concat $ replicate depth "sub") ++ "section"]
  (T.Header depth (spec sm titleLb) ref :
   map (layout sm depth) contents) ref

-- | Translates from Contents to the Printing Representation of LayoutObj.
-- Called internally by layout.
lay :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> Contents -> T.LayoutObj
lay sm (LlC x) = layLabelled sm x
lay sm (UlC x) = layUnlabelled sm (x ^. accessContents) 

layLabelled :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> LabelledContent -> T.LayoutObj
layLabelled sm x@(LblC _ (Table hdr lls t b)) = T.Table ["table"]
  ((map (spec sm) hdr) : (map (map (spec sm)) lls)) 
  (P.S $ getAdd (x ^. getRefAdd))
  b (spec sm t)
layLabelled sm x@(LblC _ (EqnBlock c))          = T.HDiv ["equation"] 
  [T.EqnBlock (P.E (expr c sm))] 
  (P.S $ getAdd (x ^. getRefAdd))
layLabelled sm x@(LblC _ (Figure c f wp))     = T.Figure 
  (P.S $ getAdd (x ^. getRefAdd))
  (spec sm c) f wp
{-
layLabelled sm x@(LblC _ (Requirement r))       = T.ALUR T.Requirement
  (spec sm $ requires r) 
  (P.S $ getAdd (x ^. getRefAdd)) 
  (spec sm $ getShortName r)
-}
layLabelled sm x@(LblC _ (Assumption a))        = T.ALUR T.Assumption
  (spec sm (assuming a))
  (P.S $ getAdd (x ^. getRefAdd))
  (spec sm $ getShortName a)
layLabelled sm x@(LblC _ (Graph ps w h t))    = T.Graph 
  (map (\(y,z) -> (spec sm y, spec sm z)) ps) w h (spec sm t)
  (P.S $ getAdd (x ^. getRefAdd))
layLabelled sm x@(LblC _ (Definition dtyp pairs)) = T.Definition 
  dtyp (layPairs pairs) 
  (P.S $ getAdd (x ^. getRefAdd))
  where layPairs = map (\(x',y) -> (x', map (lay sm) y))
layLabelled sm (LblC _ (Paragraph c))           = T.Paragraph (spec sm c)
layLabelled sm (LblC _ (Enumeration cs))        = T.List $ makeL sm cs
layLabelled sm (LblC _ (Bib bib))               = T.Bib $ map (layCite sm) bib

-- | Translates from Contents to the Printing Representation of LayoutObj.
-- Called internally by layout.
layUnlabelled :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> RawContent -> T.LayoutObj
layUnlabelled sm (Table hdr lls t b) = T.Table ["table"]
  ((map (spec sm) hdr) : (map (map (spec sm)) lls)) (P.S "nolabel0") b (spec sm t)
layUnlabelled sm (Paragraph c)          = T.Paragraph (spec sm c)
layUnlabelled sm (EqnBlock c)         = T.HDiv ["equation"] [T.EqnBlock (P.E (expr c sm))] P.EmptyS
layUnlabelled sm (Enumeration cs)       = T.List $ makeL sm cs
layUnlabelled sm (Figure c f wp)    = T.Figure (P.S "nolabel2") (spec sm c) f wp
-- layUnlabelled sm (Requirement r)      = T.ALUR T.Requirement
--   (spec sm $ requires r) (P.S "nolabel3") (spec sm $ getShortName r)
layUnlabelled sm (Assumption a)       = T.ALUR T.Assumption
  (spec sm (assuming a)) (P.S "nolabel4") (spec sm $ getShortName a)
layUnlabelled sm (Graph ps w h t)   = T.Graph (map (\(y,z) -> (spec sm y, spec sm z)) ps)
                               w h (spec sm t) (P.S "nolabel6")
layUnlabelled sm (Definition dtyp pairs)  = T.Definition dtyp (layPairs pairs) (P.S "nolabel7")
  where layPairs = map (\(x,y) -> (x, map temp y ))
        temp  y   = layUnlabelled sm (y ^. accessContents)
layUnlabelled sm (Bib bib)              = T.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite ::(HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> Citation -> P.Citation
layCite sm c = P.Cite (citeID c) (externRefT c) (map (layField sm) (c ^. getFields))

layField :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> CiteField -> P.CiteField
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

-- | Translates lists
makeL :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> ListType -> P.ListType
makeL sm (Bullet bs)      = P.Unordered   $ map (\(x,y) -> (item sm x, labref y)) bs
makeL sm (Numeric ns)     = P.Ordered     $ map (\(x,y) -> (item sm x, labref y)) ns
makeL sm (Simple ps)      = P.Simple      $ map (\(x,y,z) -> (spec sm x, item sm y, labref z)) ps
makeL sm (Desc ps)        = P.Desc        $ map (\(x,y,z) -> (spec sm x, item sm y, labref z)) ps
makeL sm (Definitions ps) = P.Definitions $ map (\(x,y,z) -> (spec sm x, item sm y, labref z)) ps

-- | Helper for translating list items
item :: (HasSymbolTable ctx, HasDefinitionTable ctx, HasPrintingOptions ctx) =>
  ctx -> ItemType -> P.ItemType
item sm (Flat i)     = P.Flat $ spec sm i
item sm (Nested t s) = P.Nested (spec sm t) (makeL sm s)

labref :: Maybe RefAdd -> Maybe P.Spec
labref l = maybe Nothing (\z -> Just $ P.S z) l

