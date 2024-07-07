{-# LANGUAGE GADTs #-}

-- | Defines functions for printing expressions.
module Language.Drasil.Printing.Import.Expr (expr) where

import Language.Drasil hiding (neg, sec, symbol, isIn, Matrix)
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.Expr.Development (ArithBinOp(..), AssocArithOper(..),
  AssocBoolOper(..), BoolBinOp(..), EqBinOp(..), Expr(..),
  LABinOp(..), OrdBinOp(..), UFunc(..), UFuncB(..), UFuncVN(..), UFuncVV(..),
  VVNBinOp(..), VVVBinOp(..), NVVBinOp(..), eprec, precA, precB)
import Language.Drasil.Literal.Development (Literal(..))

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, ckdb, stg)

import Control.Lens ((^.))
import Data.List (intersperse)

import Language.Drasil.Printing.Import.Literal (literal)
import Language.Drasil.Printing.Import.Symbol (symbol)
import Language.Drasil.Printing.Import.Helpers (lookupC, parens)


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

-- | Helper for properly rendering negation of expressions.
neg' :: Expr -> Bool
neg' (Lit (Dbl _))          = True
neg' (Lit (Int _))          = True
neg' (Lit (ExactDbl _))     = True
neg' Operator{}             = True
neg' (AssocA Mul _)       = True
neg' (LABinaryOp Index _ _) = True
neg' (UnaryOp _ _)          = True
neg' (UnaryOpB _ _)         = True
neg' (UnaryOpVV _ _)        = True
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
call :: PrintingInformation -> UID -> [Expr] -> P.Expr
call sm f ps = P.Row [
    symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f,
    parens $ P.Row $ intersperse (P.MO P.Comma) $ map (`expr` sm) ps
  ]

-- | Helper function for addition 'EOperator's.
eopAdds :: PrintingInformation -> DomainDesc t Expr Expr -> Expr -> P.Expr
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
eopMuls :: PrintingInformation -> DomainDesc t Expr Expr -> Expr -> P.Expr
eopMuls sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Prod, P.Sub (P.Row [symbol v, P.MO P.Eq, expr l sm]), P.Sup (expr h sm),
         P.Row [expr e sm]]
eopMuls sm (AllDD _ Discrete) e = P.Row [P.MO P.Prod, P.Row [expr e sm]]
eopMuls _ (AllDD _ Continuous) _ = error "Printing/Import.hs Product-Integral not implemented."
eopMuls _ (BoundedDD _ Continuous _ _) _ = error "Printing/Import.hs Product-Integral not implemented."


-- | Helper function for translating 'EOperator's.
eop :: PrintingInformation -> AssocArithOper -> DomainDesc t Expr Expr -> Expr -> P.Expr
eop sm Add = eopAdds sm
eop sm Mul = eopMuls sm


-- | Translate Exprs to printable layout AST.
expr :: Expr -> PrintingInformation -> P.Expr
expr (Lit l)                  sm = literal l sm
expr (AssocB And l)           sm = assocExpr P.And (precB And) l sm
expr (AssocB Or l)            sm = assocExpr P.Or (precB Or) l sm
expr (AssocA Add l)          sm = P.Row $ addExpr l Add sm
expr (AssocA Mul l)         sm = P.Row $ mulExpr l Mul sm
expr (C c)                    sm = symbol $ lookupC (sm ^. stg) (sm ^. ckdb) c
expr (FCall f [x])            sm =
  P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f, parens $ expr x sm]
expr (FCall f l)              sm = call sm f l
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
expr (UnaryOpVN Norm u)       sm = P.Fenced P.Norm P.Norm $ expr u sm
expr (UnaryOpVN Dim u)        sm = mkCall sm P.Dim u
expr (UnaryOp Sqrt u)         sm = P.Sqrt $ expr u sm
expr (UnaryOp Neg u)          sm = neg sm u
expr (UnaryOpVV NegV u)       sm = neg sm u
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
expr (VVVBinaryOp Cross a b)  sm = mkBOp sm P.Cross a b
expr (VVVBinaryOp VAdd a b)   sm = mkBOp sm P.VAdd a b
expr (VVVBinaryOp VSub a b)   sm = mkBOp sm P.VSub a b
expr (VVNBinaryOp Dot a b)    sm = mkBOp sm P.Dot a b
expr (NVVBinaryOp Scale a b)  sm = mkBOp sm P.Scale a b
expr (Operator o d e)         sm = eop sm o d e
expr (RealI c ri)             sm = renderRealInt sm (lookupC (sm ^. stg)
  (sm ^. ckdb) c) ri

-- | Common method of converting associative operations into printable layout AST.
assocExpr :: P.Ops -> Int -> [Expr] -> PrintingInformation -> P.Expr
assocExpr op prec exprs sm = P.Row $ intersperse (P.MO op) $ map (expr' sm prec) exprs

-- | Helper for rendering printable expressions.
addExpr :: [Expr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
addExpr exprs o sm = addExprFilter (map (expr' sm (precA o)) exprs)

-- | Add add symbol only when the second Expr is not negation 
addExprFilter :: [P.Expr] -> [P.Expr]
addExprFilter [] = []
addExprFilter [x] = [x]
addExprFilter (x1:P.Row[P.MO P.Neg, x2]:xs) = x1 : addExprFilter (P.Row[P.MO P.Neg, x2] : xs)
addExprFilter (x:xs) = x : P.MO P.Add : addExprFilter xs

-- | Helper for rendering printable expressions.
mulExpr ::  [Expr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
mulExpr (hd1:hd2:tl) o sm = case (hd1, hd2) of
  (a, Lit (Int _))      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Lit (ExactDbl _)) ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Lit (Dbl _))      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, _)                ->  [expr' sm (precA o) a, P.MO P.Mul] ++ mulExpr (hd2 : tl) o sm
mulExpr [hd]         o sm = [expr' sm (precA o) hd]
mulExpr []           o sm = [expr' sm (precA o) (int 1)]


-- | Helper that adds parenthesis to the first expression. The second expression
-- is written as a superscript attached to the first.
withParens :: PrintingInformation -> Expr -> Expr -> P.Expr
withParens prI a b = P.Row [parens (expr a prI), P.Sup (expr b prI)]

-- | Helper for properly rendering exponents.
pow :: PrintingInformation -> Expr -> Expr -> P.Expr
pow prI a@(AssocA Add _)          b = withParens prI a b
pow prI a@(AssocA Mul _)         b = withParens prI a b
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
