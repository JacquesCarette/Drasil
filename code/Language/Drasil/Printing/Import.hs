module Language.Drasil.Printing.Import(space,expr,symbol) where

import Language.Drasil.Expr (Expr(..), BinOp(..), UFunc(..), ArithOper(..),
    BoolOper(..), RTopology(..),
    DerivType(..), DomainDesc(..), UID,
    RealInterval(..),Inclusive(..))
import Language.Drasil.Expr.Precedence (precA, precB, eprec)
import qualified Language.Drasil.Printing.AST as P

import qualified Language.Drasil.Chunk.SymbolForm as SF
import Language.Drasil.ChunkDB (HasSymbolTable(..), symbLookup)
import Language.Drasil.Symbol
import Language.Drasil.Unicode (Special(Partial))

import Control.Lens ((^.))
import Language.Drasil.Space
import Data.List (intersperse)

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

-- | expr translation function from Drasil to layout AST
expr :: HasSymbolTable s => Expr -> s -> P.Expr
expr (Dbl d)          _ = P.Dbl   d
expr (Int i)          _ = P.Int   i
expr (Str s)          _ = P.Str   s
expr (AssocB And l)    sm = P.Row $ intersperse (P.MO P.And) $ map (expr' sm (precB And)) l
expr (AssocB Or l)     sm = P.Row $ intersperse (P.MO P.Or) $ map (expr' sm (precB Or)) l
expr (AssocA Add l)     sm = P.Row $ intersperse (P.MO P.Add) $ map (expr' sm (precA Add)) l
expr (AssocA Mul l)     sm = P.Row $ intersperse (P.MO P.Mul) $ map (expr' sm (precA Mul)) l
expr (Deriv Part a b) sm =
  P.Div (P.Row [P.Spec Partial, P.Spc P.Thin, expr a sm])
        (P.Row [P.Spec Partial, P.Spc P.Thin,
                symbol $ SF.eqSymb $ symbLookup b $ sm^.symbolTable])
expr (Deriv Total a b)sm =
  P.Div (P.Row [P.Ident "d", P.Spc P.Thin, expr a sm])
        (P.Row [P.Ident "d", P.Spc P.Thin, symbol $ SF.eqSymb $ symbLookup b $ sm^.symbolTable])
expr (C c)            sm = symbol $ lookupC sm c
expr (FCall f [x])    sm = P.Row [expr f sm, P.Fenced P.Paren P.Paren $ expr x sm]
expr (FCall f l)      sm = P.Row [expr f sm,
  P.Fenced P.Paren P.Paren $ P.Row $ intersperse (P.MO P.Comma) $ map (flip expr sm) l]
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
expr (UnaryOp Sqrt u)   sm = P.Sqrt $ expr u sm
expr (UnaryOp Neg u)    sm = neg sm u
expr (BinaryOp Frac a b) sm = P.Div (expr a sm) (expr b sm)
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
expr (BinaryOp Subt a b)   sm = P.Row [expr a sm, P.MO P.Subt, expr b sm]
expr (Operator o dd e)     sm = eop sm o dd e
expr (IsIn  a b)        sm = P.Row  [expr a sm, P.MO P.IsIn, space b]
expr (RealI c ri)       sm = renderRealInt sm (lookupC sm c) ri

lookupC :: HasSymbolTable s => s -> UID -> Symbol
lookupC sm c = SF.eqSymb $ symbLookup c $ sm^.symbolTable

mkCall :: HasSymbolTable ctx => ctx -> P.Ops -> Expr -> P.Expr
mkCall s o e = P.Row [P.MO o, P.Fenced P.Paren P.Paren $ expr e s]

mkBOp :: HasSymbolTable ctx => ctx -> P.Ops -> Expr -> Expr -> P.Expr
mkBOp sm o a b = P.Row [expr a sm, P.MO o, expr b sm]

expr' :: HasSymbolTable ctx => ctx -> Int -> Expr -> P.Expr
expr' s p e = fence e'
  where
  e' = expr e s
  fence = if eprec e < p then P.Fenced P.Paren P.Paren else id

-- | Helper for properly rendering negation of expressions
neg' :: Expr -> Bool
neg' (Dbl     _)     = True
neg' (Int     _)     = True
neg' (Operator _ _ _) = True
neg' (AssocA Mul _)  = True
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
    s = SF.eqSymb $ symbLookup c $ sm^.symbolTable
    f (Corners [] [] [] [b] e) =
      let e' = symbol e
          b' = symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(Atomic _) = P.Row [symbol a, P.Sub i']
    f a@(Greek _)  = P.Row [symbol a, P.Sub i']
    f   e          = let e' = symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [expr a sm], P.Sub $ expr i sm]

-- | Helper function for translating 'EOperator's
eop :: HasSymbolTable s => s -> ArithOper -> DomainDesc Expr Expr -> Expr -> P.Expr
eop sm Mul (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Prod, P.Sub (P.Row [symbol v, P.MO P.Eq, expr l sm]), P.Sup (expr h sm),
         P.Row [expr e sm]]
eop sm Mul (AllDD _ Discrete) e = P.Row [P.MO P.Prod, P.Row[expr e sm]]
eop _  Mul (AllDD _ Continuous) _ = error "HTML/Import.hs Product-Integral not implemented."
eop _  Mul (BoundedDD _ Continuous _ _) _ = error "HTML/Import.hs Product-Integral not implemented."
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
symbol (Greek g)   = P.Gr g
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
pow :: HasSymbolTable ctx => ctx -> Expr -> Expr -> P.Expr
pow sm a@(AssocA Add _)  b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Subt _ _) b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Frac _ _) b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(AssocA Mul _)  b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a@(BinaryOp Pow _ _)  b = P.Row [P.Fenced P.Paren P.Paren (expr a sm), P.Sup (expr b sm)]
pow sm a                b = P.Row [expr a sm, P.Sup (expr b sm)]

{-
constraint :: (HasSymbolTable st, Quantity q) => st -> q -> Constraint -> P.Expr
constraint st s (Range _ rr)          = renderRealInt st (SF.eqSymb s) rr
constraint _  s (EnumeratedReal _ rr) = P.Row [symbol $ SF.eqSymb s, P.MO P.IsIn, space $ DiscreteD rr]
constraint _  s (EnumeratedStr _ rr)  = P.Row [symbol $ SF.eqSymb s, P.MO P.IsIn, space $ DiscreteS rr]
-}
renderRealInt :: HasSymbolTable st => st -> Symbol -> RealInterval Expr Expr -> P.Expr
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
