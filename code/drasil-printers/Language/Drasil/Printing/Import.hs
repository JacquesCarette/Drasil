module Language.Drasil.Printing.Import (space, expr, symbol, spec,
  makeDocument) where

import Language.Drasil hiding (neg, sec, symbol, isIn)
import Language.Drasil.Display
import Language.Drasil.Development (UFuncB(..), UFuncVec(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..), LABinOp(..)
  , OrdBinOp(..), VVNBinOp(..), VVVBinOp(..)
  , precA, precB, eprec, dePrec, dePrecAssoc, DisplayExpr(..)
  , DisplayBinOp(..), DisplayAssocBinOp(Equivalence))
import Database.Drasil
import Utils.Drasil

import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..),
  PrintingInformation, Notation(Scientific, Engineering), ckdb, stg)

import Control.Lens ((^.))
import Data.Bifunctor (bimap, second)
import Data.List (intersperse, partition)
import Numeric (floatToDigits)
import Data.Maybe (fromMaybe)

-- | Render a 'Space'.
space :: PrintingInformation -> Space -> P.Expr
space _ Integer = P.MO P.Integer
space _ Rational = P.MO P.Rational
space _ Real = P.MO P.Real
space _ Natural = P.MO P.Natural
space _ Boolean = P.MO P.Boolean
space _ Char = P.Ident "Char"
space _ String = P.Ident "String"
space _ Radians = error "Radians not translated"
space _ (Vect _) = error "Vector space not translated"
space _ (Array _) = error "Array space not translated"
space _ (Actor s) = P.Ident s
space sm (DiscreteD l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map (flip expr sm . dbl) l -- [Double]
space _ (DiscreteS l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map P.Str l --ex. let Meal = {"breakfast", "lunch", "dinner"}
space _ Void = error "Void not translated"

{-
p_space :: Space -> String
p_space Radians  = "rad"
p_space (Vect a) = "V" ++ p_space a
p_space (DiscreteD a)  = "{" ++ (concat $ intersperse ", " (map show a)) ++ "}"
p_space (DiscreteS a)  = "{" ++ (concat $ intersperse ", " a) ++ "}"
-}

-- | Helper for inserting parentheses.
parens :: P.Expr -> P.Expr
parens = P.Fenced P.Paren P.Paren

-- | Helper for rendering printable expressions.
mulExpr ::  [Expr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
mulExpr (hd1:hd2:tl) o sm = case (hd1, hd2) of
  (a, Int _)      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, ExactDbl _) ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Dbl _)      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, _)          ->  [expr' sm (precA o) a, P.MO P.Mul] ++ mulExpr (hd2 : tl) o sm
mulExpr [hd]         o sm = [expr' sm (precA o) hd]
mulExpr []           o sm = [expr' sm (precA o) (Int 1)]

-- | Processes the digits from the 'floatToDigits' function,
-- decimal point position, a counter, and exponent.
digitsProcess :: [Integer] -> Int -> Int -> Integer -> [P.Expr]
digitsProcess [0] _ _ _ = [P.Int 0, P.MO P.Point, P.Int 0]
digitsProcess ds pos _ (-3) = [P.Int 0, P.MO P.Point] ++ replicate (3 - pos) (P.Int 0) ++ map P.Int ds
digitsProcess (hd:tl) pos coun ex
  | pos /= coun = P.Int hd : digitsProcess tl pos (coun + 1) ex
  | ex /= 0 = [P.MO P.Point, P.Int hd] ++ map P.Int tl ++ [P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
  | otherwise = [P.MO P.Point, P.Int hd] ++ map P.Int tl
digitsProcess [] pos coun ex
  | pos > coun = P.Int 0 : digitsProcess [] pos (coun+1) ex
  | ex /= 0 = [P.MO P.Point, P.Int 0, P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
  | otherwise = [P.MO P.Point, P.Int 0]

-- | Takes the exponent and the 'Int' of the base and gives
-- the decimal point position and processed exponent.
-- This function supports transferring scientific notation to
-- engineering notation.
-- References for standard of Engineering Notation:
--
-- https://www.khanacademy.org/science/electrical-engineering/introduction-to-ee/
--    intro-to-ee/a/ee-numbers-in-electrical-engineering 
--
-- https://www.calculatorsoup.com/calculators/math/scientific-notation-converter.php
--
-- https://en.wikipedia.org/wiki/Scientific_notation
processExpo :: Int -> (Int, Int)
processExpo a
  | mod (a-1) 3 == 0 = (1, a-1)
  | mod (a-1) 3 == 1 = (2, a-2)
  | mod (a-1) 3 == 2 = (3, a-3)
  | otherwise = error "The cases of processExpo should be exhaustive!"

-- | Common method of converting associative operations into printable layout AST.
assocExpr :: P.Ops -> Int -> [Expr] -> PrintingInformation -> P.Expr
assocExpr op prec exprs sm = P.Row $ intersperse (P.MO op) $ map (expr' sm prec) exprs

-- | Convert 'DisplayBinOps' into the operators of the AST language.
deBinOp :: DisplayBinOp -> P.Ops
deBinOp IsIn    = P.IsIn
deBinOp Defines = P.Eq

-- | Convert 'DisplayAssocBinOp's into the operators of the AST language.
deAssocBinOp :: DisplayAssocBinOp -> P.Ops
deAssocBinOp Equivalence = P.Eq 
deAssocBinOp _           = P.And

-- | Translate DisplayExprs to printable layout AST.
dispExpr :: DisplayExpr -> PrintingInformation -> P.Expr
dispExpr (AlgebraicExpr e)  sm = expr e sm
dispExpr (SpaceExpr s)      sm = space sm s
dispExpr (BinOp b l r)      sm = P.Row [dispExpr l sm, P.MO $ deBinOp b, dispExpr r sm]
dispExpr (AssocBinOp b des) sm = P.Row $ intersperse (P.MO op) $ map (dispExpr' sm prec) des
  where prec = dePrecAssoc b
        op   = deAssocBinOp b

-- | Translate Exprs to printable layout AST.
expr :: Expr -> PrintingInformation -> P.Expr
expr (Dbl d)                  sm = case sm ^. getSetting of
  Engineering -> P.Row $ digitsProcess (map toInteger $ fst $ floatToDigits 10 d)
     (fst $ processExpo $ snd $ floatToDigits 10 d) 0
     (toInteger $ snd $ processExpo $ snd $ floatToDigits 10 d)
  Scientific  ->  P.Dbl d
expr (Int i)                   _ = P.Int i
expr (ExactDbl d)             _  = P.Int d
expr (Str s)                   _ = P.Str s
expr (Perc a b)               sm = P.Row [expr (dbl val) sm, P.MO P.Perc]
  where
    val = fromIntegral a / (10 ** fromIntegral (b - 2))
expr (AssocB And l)           sm = assocExpr P.And (precB And) l sm
expr (AssocB Or l)            sm = assocExpr P.Or (precB Or) l sm
expr (AssocA AddI l)          sm = assocExpr P.Add (precA AddI) l sm
expr (AssocA AddRe l)         sm = assocExpr P.Add (precA AddRe) l sm
expr (AssocA MulI l)          sm = P.Row $ mulExpr l MulI sm
expr (AssocA MulRe l)         sm = P.Row $ mulExpr l MulRe sm
expr (Deriv Part a b)         sm =
  P.Div (P.Row [P.Spc P.Thin, P.Spec Partial, expr a sm])
        (P.Row [P.Spc P.Thin, P.Spec Partial,
                symbol $ lookupC (sm ^. stg) (sm ^. ckdb) b])
expr (Deriv Total a b)        sm =
  P.Div (P.Row [P.Spc P.Thin, P.Ident "d", expr a sm])
        (P.Row [P.Spc P.Thin, P.Ident "d",
                symbol $ lookupC (sm ^. stg) (sm ^. ckdb) b])
expr (C c)                    sm = symbol $ lookupC (sm ^. stg) (sm ^. ckdb) c
expr (FCall f [x] [])         sm =
  P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f, parens $ expr x sm]
expr (FCall f l ns)           sm = call sm f l ns
expr (New c l ns)             sm = call sm c l ns
expr (Message a m l ns)       sm =
  P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) a, P.MO P.Point, call sm m l ns]
expr (Field o f)              sm = P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) o,
  P.MO P.Point, symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f]
expr (Case _ ps)              sm =
  if length ps < 2
    then error "Attempting to use multi-case expr incorrectly"
    else P.Case (zip (map (flip expr sm . fst) ps) (map (flip expr sm . snd) ps))
expr (Matrix a)               sm = P.Mtx $ map (map (`expr` sm)) a
expr (UnaryOp Log u)          sm = mkCall sm P.Log u
expr (UnaryOp Ln u)           sm = mkCall sm P.Ln u
expr (UnaryOp Sin u)          sm = mkCall sm P.Sin u
expr (UnaryOp Cos u)          sm = mkCall sm P.Cos u
expr (UnaryOp Tan u)          sm = mkCall sm P.Tan u
expr (UnaryOp Sec u)          sm = mkCall sm P.Sec u
expr (UnaryOp Csc u)          sm = mkCall sm P.Csc u
expr (UnaryOp Cot u)          sm = mkCall sm P.Cot u
expr (UnaryOp Arcsin u)       sm = mkCall sm P.Arcsin u
expr (UnaryOp Arccos u)       sm = mkCall sm P.Arccos u
expr (UnaryOp Arctan u)       sm = mkCall sm P.Arctan u
expr (UnaryOp Exp u)          sm = P.Row [P.MO P.Exp, P.Sup $ expr u sm]
expr (UnaryOp Abs u)          sm = P.Fenced P.Abs P.Abs $ expr u sm
expr (UnaryOpB Not u)         sm = P.Row [P.MO P.Not, expr u sm]
expr (UnaryOpVec Norm u)      sm = P.Fenced P.Norm P.Norm $ expr u sm
expr (UnaryOpVec Dim u)       sm = mkCall sm P.Dim u
expr (UnaryOp Sqrt u)         sm = P.Sqrt $ expr u sm
expr (UnaryOp Neg u)          sm = neg sm u
expr (ArithBinaryOp Frac a b) sm = P.Div (expr a sm) (expr b sm)
expr (ArithBinaryOp Pow a b)  sm = pow sm a b
expr (ArithBinaryOp Subt a b) sm = P.Row [expr a sm, P.MO P.Subt, expr b sm]
expr (BoolBinaryOp Impl a b)  sm = mkBOp sm P.Impl a b
expr (BoolBinaryOp Iff a b)   sm = mkBOp sm P.Iff a b
expr (EqBinaryOp Eq a b)      sm = mkBOp sm P.Eq a b
expr (EqBinaryOp NEq a b)     sm = mkBOp sm P.NEq a b
expr (LABinaryOp Index a b)   sm = indx sm a b
expr (OrdBinaryOp Lt a b)     sm = mkBOp sm P.Lt a b
expr (OrdBinaryOp Gt a b)     sm = mkBOp sm P.Gt a b
expr (OrdBinaryOp LEq a b)    sm = mkBOp sm P.LEq a b
expr (OrdBinaryOp GEq a b)    sm = mkBOp sm P.GEq a b
expr (VVNBinaryOp Dot a b)    sm = mkBOp sm P.Dot a b
expr (VVVBinaryOp Cross a b)  sm = mkBOp sm P.Cross a b
expr (Operator o d e)         sm = eop sm o d e
expr (RealI c ri)             sm = renderRealInt sm (lookupC (sm ^. stg)
  (sm ^. ckdb) c) ri

-- | Given the stage of the symbol, looks up a character/symbol
-- inside a chunk database that matches the given 'UID'. 
lookupC :: Stage -> ChunkDB -> UID -> Symbol
lookupC Equational     sm c = eqSymb   $ symbResolve sm c
lookupC Implementation sm c = codeSymb $ symbResolve sm c

-- | Look up a term given a chunk database and a 'UID' associated with the term.
lookupT :: ChunkDB -> UID -> Sentence
lookupT sm c = phraseNP $ termResolve sm c ^. term

-- | Look up the acronym/abbreviation of a term. Otherwise returns the singular form of a term. Takes a chunk database and a 'UID' associated with the term.
lookupS :: ChunkDB -> UID -> Sentence
lookupS sm c = maybe (phraseNP $ l ^. term) S $ getA l
  where l = termResolve sm c

-- | Look up the plural form of a term given a chunk database and a 'UID' associated with the term.
lookupP :: ChunkDB -> UID -> Sentence
lookupP sm c = pluralNP $ termResolve sm c ^. term
-- plural n = NP.plural (n ^. term)

-- | Helper that creates an expression row given printing information, an operator, and an expression.
mkCall :: PrintingInformation -> P.Ops -> Expr -> P.Expr
mkCall s o e = P.Row [P.MO o, parens $ expr e s]

-- | Helper that creates a binary expression row given printing information, an operator, and two expressions.
mkBOp :: PrintingInformation -> P.Ops -> Expr -> Expr -> P.Expr
mkBOp sm o a b = P.Row [expr a sm, P.MO o, expr b sm]

-- | Helper that adds parenthesis to an expression where appropriate.
expr' :: PrintingInformation -> Int -> Expr -> P.Expr
expr' s p e = fence $ expr e s
  where fence = if eprec e < p then parens else id

-- | Helper that adds parenthesis to a display expression where appropriate.
dispExpr' :: PrintingInformation -> Int -> DisplayExpr -> P.Expr
dispExpr' s p e = fence $ dispExpr e s
  where fence = if dePrec e < p then parens else id

-- | Helper for properly rendering negation of expressions.
neg' :: Expr -> Bool
neg' (Dbl     _)            = True
neg' (Int     _)            = True
neg' Operator{}             = True
neg' (AssocA MulI _)        = True
neg' (AssocA MulRe _)       = True
neg' (LABinaryOp Index _ _) = True
neg' (UnaryOp _ _)          = True
neg' (UnaryOpB _ _)         = True
neg' (UnaryOpVec _ _)       = True
neg' (C _)                  = True
neg' _                      = False

-- | Render negated expressions.
neg :: PrintingInformation -> Expr -> P.Expr
neg sm a = P.Row [P.MO P.Neg, (if neg' a then id else parens) $ expr a sm]

-- | For printing indexes.
indx :: PrintingInformation -> Expr -> Expr -> P.Expr
indx sm (C c) i = f s
  where
    i' = expr i sm
    s = lookupC (sm ^. stg) (sm ^. ckdb) c
    f (Corners [] [] [] [b] e) =
      let e' = symbol e
          b' = symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(Variable _) = P.Row [symbol a, P.Sub i']
    f a@(Label _)    = P.Row [symbol a, P.Sub i']
--    f a@(Greek _)  = P.Row [symbol a, P.Sub i']
    f   e          = let e' = symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [expr a sm], P.Sub $ expr i sm]

-- | For printing expressions that call something.
call :: PrintingInformation -> UID -> [Expr] -> [(UID,Expr)] -> P.Expr
call sm f ps ns = P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f,
  parens $ P.Row $ intersperse (P.MO P.Comma) $ map (`expr` sm) ps ++
  zipWith (\n a -> P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) n,
  P.MO P.Eq, expr a sm]) (map fst ns) (map snd ns)]

-- | Helper function for addition 'EOperator's.
eopAdds :: PrintingInformation -> DomainDesc Expr Expr -> Expr -> P.Expr
eopAdds sm (BoundedDD v Continuous l h) e =
  P.Row [P.MO P.Inte, P.Sub (expr l sm), P.Sup (expr h sm),
         P.Row [expr e sm], P.Spc P.Thin, P.Ident "d", symbol v]
eopAdds sm (AllDD v Continuous) e =
  P.Row [P.MO P.Inte, P.Sub (symbol v), P.Row [expr e sm], P.Spc P.Thin,
         P.Ident "d", symbol v]
eopAdds sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Summ, P.Sub (P.Row [symbol v, P.MO P.Eq, expr l sm]), P.Sup (expr h sm),
         P.Row [expr e sm]]
eopAdds sm (AllDD _ Discrete) e = P.Row [P.MO P.Summ, P.Row [expr e sm]]

-- | Helper function for multiplicative 'EOperator's.
eopMuls :: PrintingInformation -> DomainDesc Expr Expr -> Expr -> P.Expr
eopMuls sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Prod, P.Sub (P.Row [symbol v, P.MO P.Eq, expr l sm]), P.Sup (expr h sm),
         P.Row [expr e sm]]
eopMuls sm (AllDD _ Discrete) e = P.Row [P.MO P.Prod, P.Row [expr e sm]]
eopMuls _ (AllDD _ Continuous) _ = error "Printing/Import.hs Product-Integral not implemented."
eopMuls _ (BoundedDD _ Continuous _ _) _ = error "Printing/Import.hs Product-Integral not implemented."


-- | Helper function for translating 'EOperator's.
eop :: PrintingInformation -> AssocArithOper -> DomainDesc Expr Expr -> Expr -> P.Expr
eop sm AddI = eopAdds sm
eop sm AddRe = eopAdds sm
eop sm MulI = eopMuls sm
eop sm MulRe = eopMuls sm

-- | Helper tha converts a symbol into an expression.
symbol :: Symbol -> P.Expr
symbol (Variable s) = P.Ident s
symbol (Label    s) = P.Label s
symbol (Integ    n) = P.Int (toInteger n)
symbol (Special  s) = P.Spec s
--symbol (Greek g)    = P.Gr g
symbol (Concat  sl) = P.Row $ map symbol sl
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = P.Row [P.Row [symbol s, P.Sup $ symbol x]]
symbol (Corners [] [] [] [x] s) = P.Row [P.Row [symbol s, P.Sub $ symbol x]]
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol Corners{}                = error "rendering of Corners (general)"
symbol (Atop f s)               = sFormat f s
symbol Empty                    = P.Row []

-- | Helper that adds decoration to symbols (for vectors, derivatives, etc.).
sFormat :: Decoration -> Symbol -> P.Expr
sFormat Hat    s = P.Over P.Hat $ symbol s
sFormat Vector s = P.Font P.Bold $ symbol s
sFormat Prime  s = P.Row [symbol s, P.MO P.Prime]

-- | Helper that adds parenthesis to the first expression. The second expression
-- is written as a superscript attached to the first.
withParens :: PrintingInformation -> Expr -> Expr -> P.Expr
withParens prI a b = P.Row [parens (expr a prI), P.Sup (expr b prI)]

-- | Helper for properly rendering exponents.
pow :: PrintingInformation -> Expr -> Expr -> P.Expr
pow prI a@(AssocA AddI _)          b = withParens prI a b
pow prI a@(AssocA AddRe _)         b = withParens prI a b
pow prI a@(AssocA MulI _)          b = withParens prI a b
pow prI a@(AssocA MulRe _)         b = withParens prI a b
pow prI a@(ArithBinaryOp Subt _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Frac _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Pow _ _)  b = withParens prI a b
pow prI a                          b = P.Row [expr a prI, P.Sup (expr b prI)]

-- | Print a 'RealInterval'.
renderRealInt :: PrintingInformation -> Symbol -> RealInterval Expr Expr -> P.Expr
renderRealInt st s (Bounded (Inc,a) (Inc,b)) =
  P.Row [expr a st, P.MO P.LEq, symbol s, P.MO P.LEq, expr b st]
renderRealInt st s (Bounded (Inc,a) (Exc,b)) =
  P.Row [expr a st, P.MO P.LEq, symbol s, P.MO P.Lt, expr b st]
renderRealInt st s (Bounded (Exc,a) (Inc,b)) =
  P.Row [expr a st, P.MO P.Lt, symbol s, P.MO P.LEq, expr b st]
renderRealInt st s (Bounded (Exc,a) (Exc,b)) =
  P.Row [expr a st, P.MO P.Lt, symbol s, P.MO P.Lt, expr b st]
renderRealInt st s (UpTo (Inc,a))   = P.Row [symbol s, P.MO P.LEq, expr a st]
renderRealInt st s (UpTo (Exc,a))   = P.Row [symbol s, P.MO P.Lt,  expr a st]
renderRealInt st s (UpFrom (Inc,a)) = P.Row [symbol s, P.MO P.GEq, expr a st]
renderRealInt st s (UpFrom (Exc,a)) = P.Row [symbol s, P.MO P.Gt,  expr a st]


-- | Translates 'Sentence' to the printable representation of a 'Sentence' ('Spec').
spec :: PrintingInformation -> Sentence -> P.Spec
  -- make sure these optimizations are clear
spec sm (EmptyS :+: b)     = spec sm b
spec sm (a :+: EmptyS)     = spec sm a
spec sm (a :+: b)          = spec sm a P.:+: spec sm b
spec _ (S s)               = either error P.S $ checkValidStr s invalidChars
  where invalidChars = ['<', '>', '\"', '&', '#', '$', '%', '&', '~', '^', '\\', '{', '}']
spec _ (Sy s)              = P.E $ pUnit s
spec _ Percent             = P.E $ P.MO P.Perc
spec _ (P s)               = P.E $ symbol s
spec sm (Ch SymbolStyle s) = P.E $ symbol $ lookupC (sm ^. stg) (sm ^. ckdb) s
spec sm (Ch TermStyle s)   = spec sm $ lookupT (sm ^. ckdb) s
spec sm (Ch ShortStyle s)  = spec sm $ lookupS (sm ^. ckdb) s
spec sm (Ch PluralTerm s)  = spec sm $ lookupP (sm ^. ckdb) s
spec sm (Ref u notes)      =
  let reff = refResolve u (sm ^. ckdb . refTable) in
  case reff of 
  (Reference _ (RP rp ra) sn _) ->
    P.Ref P.Internal ra $ spec sm $ renderShortName (sm ^. ckdb) rp sn
  (Reference _ (Citation ra) _ _) ->
    P.Ref P.Cite2    ra (spec sm (renderCitInfo notes))
  (Reference _ (URI ra) sn _) ->
    P.Ref P.External    ra $ spec sm $ renderURI sm sn
{-spec sm (Ref u n) = let reff = refResolve u (sm ^. ckdb . refTable) in
  case reff of
  (Reference _ (Citation ra) _ _) ->
    P.Ref P.Cite2    ra (spec sm (renderCitInfo n))
  _ -> error "Only citations should have citation information." -- should this be an error, or should all references get the ability to renderCitInfo?-}
spec sm (Quote q)          = P.Quote $ spec sm q
spec _  EmptyS             = P.EmptyS
spec sm (E e)              = P.E $ dispExpr e sm

-- | Renders a unit symbol as a printable expression.
pUnit :: USymb -> P.Expr
pUnit (US ls) = formatu t b
  where
    (t,b) = partition ((> 0) . snd) ls
    formatu :: [(Symbol,Integer)] -> [(Symbol,Integer)] -> P.Expr
    formatu [] l = line l
    formatu l [] = P.Row $ map powu l
    formatu nu de = P.Div (line nu) $ line $ map (second negate) de
    line :: [(Symbol,Integer)] -> P.Expr
    line []  = P.Row [] -- should not happen ?
    line [n] = powu n
    line l   = P.Row $ map powu l
    powu :: (Symbol,Integer) -> P.Expr
    powu (n,1) = symbol n
    powu (n,p) = P.Row [symbol n, P.Sup $ P.Int p]

-- | Renders the shortname of a reference/domain.
renderShortName :: ChunkDB -> IRefProg -> ShortName -> Sentence
renderShortName ctx (Deferred u) _ = S $ fromMaybe (error "Domain has no abbreviation.") $
  getA $ defResolve ctx u --Need defResolve instead of refResolve since only ConceptInstance
  -- uses this case for domains and we want the short name from there. 
  -- Used to be: S $ getRefAdd $ refResolve u (ctx ^. refTable)
renderShortName ctx (RConcat a b) sn = renderShortName ctx a sn :+: renderShortName ctx b sn
renderShortName _ (RS s) _ = S s
renderShortName _ Name sn = getSentSN sn

-- | Render a uniform resource locator as a 'Sentence'.
renderURI :: ctx -> ShortName -> Sentence
renderURI _ = getSentSN

-- | Renders citation information.
renderCitInfo :: RefInfo -> Sentence
renderCitInfo  None          = EmptyS
renderCitInfo (RefNote   rn) = sParen (S rn)
renderCitInfo (Equation [x]) = sParen (S "Eq." +:+ S (show x))
renderCitInfo (Equation  i ) = sParen (S "Eqs." +:+ foldNums "-" i)
renderCitInfo (Page     [x]) = sParen (S "pg." +:+ S (show x))
renderCitInfo (Page      i ) = sParen (S "pp." +:+ foldNums "-" i)

-- | Translates from 'Document' to a printable representation of 'T.Document'.
makeDocument :: PrintingInformation -> Document -> T.Document
makeDocument sm (Document titleLb authorName sections) =
  T.Document (spec sm titleLb) (spec sm authorName) (createLayout sm sections)

-- | Helper for translating sections into a printable representation of layout objects ('T.LayoutObj').
layout :: PrintingInformation -> Int -> SecCons -> T.LayoutObj
layout sm currDepth (Sub s) = sec sm (currDepth+1) s
layout sm _         (Con c) = lay sm c

-- | Helper function for creating sections as layout objects.
createLayout :: PrintingInformation -> [Section] -> [T.LayoutObj]
createLayout sm = map (sec sm 0)

-- | Helper function for creating sections at the appropriate depth.
sec :: PrintingInformation -> Int -> Section -> T.LayoutObj
sec sm depth x@(Section titleLb contents _) = --FIXME: should ShortName be used somewhere?
  let ref = P.S (refAdd x) in
  T.HDiv [concat (replicate depth "sub") ++ "section"]
  (T.Header depth (spec sm titleLb) ref :
   map (layout sm depth) contents) ref

-- | Helper that translates 'Contents' to a printable representation of 'T.LayoutObj'.
-- Called internally by 'layout'.
lay :: PrintingInformation -> Contents -> T.LayoutObj
lay sm (LlC x) = layLabelled sm x
lay sm (UlC x) = layUnlabelled sm (x ^. accessContents)

-- | Helper that translates 'LabelledContent's to a printable representation of 'T.LayoutObj'.
-- Called internally by 'lay'.
layLabelled :: PrintingInformation -> LabelledContent -> T.LayoutObj
layLabelled sm x@(LblC _ (Table hdr lls t b)) = T.Table ["table"]
  (map (spec sm) hdr : map (map (spec sm)) lls)
  (P.S $ getRefAdd x)
  b (spec sm t)
layLabelled sm x@(LblC _ (EqnBlock c))          = T.HDiv ["equation"]
  [T.EqnBlock (P.E (dispExpr c sm))]
  (P.S $ getRefAdd x)
layLabelled sm x@(LblC _ (Figure c f wp))     = T.Figure
  (P.S $ getRefAdd x)
  (spec sm c) f wp
layLabelled sm x@(LblC _ (Graph ps w h t))    = T.Graph
  (map (bimap (spec sm) (spec sm)) ps) w h (spec sm t)
  (P.S $ getRefAdd x)
layLabelled sm x@(LblC _ (Defini dtyp pairs)) = T.Definition
  dtyp (layPairs pairs)
  (P.S $ getRefAdd x)
  where layPairs = map (second (map (lay sm)))
layLabelled sm (LblC _ (Paragraph c))    = T.Paragraph (spec sm c)
layLabelled sm x@(LblC _ (DerivBlock h d)) = T.HDiv ["subsubsubsection"]
  (T.Header 3 (spec sm h) ref : map (layUnlabelled sm) d) ref
  where ref = P.S $ refAdd x ++ "Deriv"
layLabelled sm (LblC _ (Enumeration cs)) = T.List $ makeL sm cs
layLabelled  _ (LblC _ (Bib bib))        = T.Bib $ map layCite bib

-- | Helper that translates 'RawContent's to a printable representation of 'T.LayoutObj'.
-- Called internally by 'lay'.
layUnlabelled :: PrintingInformation -> RawContent -> T.LayoutObj
layUnlabelled sm (Table hdr lls t b) = T.Table ["table"]
  (map (spec sm) hdr : map (map (spec sm)) lls) (P.S "nolabel0") b (spec sm t)
layUnlabelled sm (Paragraph c)    = T.Paragraph (spec sm c)
layUnlabelled sm (EqnBlock c)     = T.HDiv ["equation"] [T.EqnBlock (P.E (dispExpr c sm))] P.EmptyS
layUnlabelled sm (DerivBlock h d) = T.HDiv ["subsubsubsection"]
  (T.Header 3 (spec sm h) ref : map (layUnlabelled sm) d) ref
  where ref = P.S "nolabel1"
layUnlabelled sm (Enumeration cs) = T.List $ makeL sm cs
layUnlabelled sm (Figure c f wp)  = T.Figure (P.S "nolabel2") (spec sm c) f wp
layUnlabelled sm (Graph ps w h t) = T.Graph (map (bimap (spec sm) (spec sm)) ps)
                               w h (spec sm t) (P.S "nolabel6")
layUnlabelled sm (Defini dtyp pairs)  = T.Definition dtyp (layPairs pairs) (P.S "nolabel7")
  where layPairs = map (second (map temp))
        temp  y   = layUnlabelled sm (y ^. accessContents)
layUnlabelled  _ (Bib bib)              = T.Bib $ map layCite bib

-- | For importing a bibliography.
layCite :: Citation -> P.Citation
layCite c = P.Cite (c ^. citeID) (c ^. citeKind) (map layField (c ^. getFields))

-- | Helper for translating 'Citefield's into a printable representation of 'P.CiteField's
layField :: CiteField -> P.CiteField
layField (Address      s) = P.Address      $ P.S s
layField (Author       p) = P.Author       p
layField (BookTitle    b) = P.BookTitle    $ P.S b
layField (Chapter      c) = P.Chapter      c
layField (Edition      e) = P.Edition      e
layField (Editor       e) = P.Editor       e
layField (Institution  i) = P.Institution  $ P.S i
layField (Journal      j) = P.Journal      $ P.S j
layField (Month        m) = P.Month        m
layField (Note         n) = P.Note         $ P.S n
layField (Number       n) = P.Number       n
layField (Organization o) = P.Organization $ P.S o
layField (Pages        p) = P.Pages        p
layField (Publisher    p) = P.Publisher    $ P.S p
layField (School       s) = P.School       $ P.S s
layField (Series       s) = P.Series       $ P.S s
layField (Title        t) = P.Title        $ P.S t
layField (Type         t) = P.Type         $ P.S t
layField (Volume       v) = P.Volume       v
layField (Year         y) = P.Year         y
layField (HowPublished (URL  u)) = P.HowPublished (P.URL  $ P.S u)
layField (HowPublished (Verb v)) = P.HowPublished (P.Verb $ P.S v)

-- | Translates lists to be printable.
makeL :: PrintingInformation -> ListType -> P.ListType
makeL sm (Bullet bs)      = P.Unordered   $ map (bimap (item sm) (fmap P.S)) bs
makeL sm (Numeric ns)     = P.Ordered     $ map (bimap (item sm) (fmap P.S)) ns
makeL sm (Simple ps)      = P.Simple      $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps
makeL sm (Desc ps)        = P.Desc        $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps
makeL sm (Definitions ps) = P.Definitions $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps

-- | Helper for translating list items to be printable.
item :: PrintingInformation -> ItemType -> P.ItemType
item sm (Flat i)     = P.Flat $ spec sm i
item sm (Nested t s) = P.Nested (spec sm t) (makeL sm s)
