{-# LANGUAGE GADTs #-}

-- | Defines functions to render 'CodeExpr's as printable 'P.Expr's.
module Language.Drasil.Printing.Import.CodeExpr (codeExpr) where

import Data.List (intersperse)

import Drasil.Code.CodeExpr.Development

import Drasil.Database (UID)
import Language.Drasil (DomainDesc(..), Inclusive(..),
  RTopology(..), RealInterval(..), LiteralC (int))
import qualified Language.Drasil.Display as S (Symbol(..))
import Language.Drasil.Literal.Development

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)
import Language.Drasil.Printing.Import.Helpers (lookupC', parens)
import Language.Drasil.Printing.Import.Literal (literal)
import Language.Drasil.Printing.Import.Symbol (symbol)

-- | Helper that creates an expression row given printing information, an operator, and an expression.
mkCall :: PrintingInformation -> P.Ops -> CodeExpr -> P.Expr
mkCall s o e = P.Row [P.MO o, parens $ codeExpr s e]

-- | Helper that creates a binary expression row given printing information, an operator, and two expressions.
mkBOp :: PrintingInformation -> P.Ops -> CodeExpr -> CodeExpr -> P.Expr
mkBOp sm o a b = P.Row [codeExpr sm a, P.MO o, codeExpr sm b]

-- | Helper that adds parenthesis to an expression where appropriate.
expr' :: PrintingInformation -> Int -> CodeExpr -> P.Expr
expr' s p e = fence $ codeExpr s e
  where fence = if eprec e < p then parens else id

-- | Helper for properly rendering negation of expressions.
neg' :: CodeExpr -> Bool
neg' (Lit (Dbl _))          = True
neg' (Lit (Int _))          = True
neg' (Lit (ExactDbl _))     = True
neg' Operator{}             = True
neg' (AssocA Mul _)        = True
neg' (LABinaryOp Index _ _) = True
neg' (UnaryOp _ _)          = True
neg' (UnaryOpB _ _)         = True
neg' (UnaryOpVV _ _)        = True
neg' (C _)                  = True
neg' _                      = False

-- | Render negated expressions.
neg :: PrintingInformation -> CodeExpr -> P.Expr
neg sm a = P.Row [P.MO P.Neg, (if neg' a then id else parens) $ codeExpr sm a]

-- | For printing indexes.
indx :: PrintingInformation -> CodeExpr -> CodeExpr -> P.Expr
indx sm (C c) i = f s
  where
    i' = codeExpr sm i
    s = lookupC' sm c
    f (S.Corners [] [] [] [b] e) =
      let e' = symbol e
          b' = symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(S.Variable _) = P.Row [symbol a, P.Sub i']
    f a@(S.Label _)    = P.Row [symbol a, P.Sub i']
--    f a@(Greek _)  = P.Row [symbol a, P.Sub i']
    f   e          = let e' = symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [codeExpr sm a], P.Sub $ codeExpr sm i]

-- | For printing expressions that call something.
call :: PrintingInformation -> UID -> [CodeExpr] -> [(UID, CodeExpr)] -> P.Expr
call sm f ps ns = P.Row [symbol $ lookupC' sm f,
  parens $ P.Row $ intersperse (P.MO P.Comma) $ map (codeExpr sm) ps ++
  zipWith (\n a -> P.Row [symbol $ lookupC' sm n,
  P.MO P.Eq, codeExpr sm a]) (map fst ns) (map snd ns)]

-- | Helper function for addition 'EOperator's.
eopAdds :: PrintingInformation -> DomainDesc t CodeExpr CodeExpr -> CodeExpr -> P.Expr
eopAdds sm (BoundedDD v Continuous l h) e =
  P.Row [P.MO P.Inte, P.Sub (codeExpr sm l), P.Sup (codeExpr sm h),
         P.Row [codeExpr sm e], P.Spc P.Thin, P.Ident "d", symbol v]
eopAdds sm (AllDD v Continuous) e =
  P.Row [P.MO P.Inte, P.Sub (symbol v), P.Row [codeExpr sm e], P.Spc P.Thin,
         P.Ident "d", symbol v]
eopAdds sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Summ, P.Sub (P.Row [symbol v, P.MO P.Eq, codeExpr sm l]), P.Sup (codeExpr sm h),
         P.Row [codeExpr sm e]]
eopAdds sm (AllDD _ Discrete) e = P.Row [P.MO P.Summ, P.Row [codeExpr sm e]]

-- | Helper function for multiplicative 'EOperator's.
eopMuls :: PrintingInformation -> DomainDesc t CodeExpr CodeExpr -> CodeExpr -> P.Expr
eopMuls sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Prod, P.Sub (P.Row [symbol v, P.MO P.Eq, codeExpr sm l]), P.Sup (codeExpr sm h),
         P.Row [codeExpr sm e]]
eopMuls sm (AllDD _ Discrete) e = P.Row [P.MO P.Prod, P.Row [codeExpr sm e]]
eopMuls _ (AllDD _ Continuous) _ = error "Printing/Import.hs Product-Integral not implemented."
eopMuls _ (BoundedDD _ Continuous _ _) _ = error "Printing/Import.hs Product-Integral not implemented."

-- | Helper function for translating 'EOperator's.
eop :: PrintingInformation -> AssocArithOper -> DomainDesc t CodeExpr CodeExpr -> CodeExpr -> P.Expr
eop sm Add = eopAdds sm
eop sm Mul = eopMuls sm

-- | Translate 'CodeExpr's to printable layout AST 'Expr's.
codeExpr :: PrintingInformation -> CodeExpr -> P.Expr
codeExpr sm (Lit l)                  = literal l sm
codeExpr sm (AssocB And l)           = assocExpr P.And (precB And) l sm
codeExpr sm (AssocB Or l)            = assocExpr P.Or (precB Or) l sm
codeExpr sm (AssocA Add l)           = P.Row $ addExpr l Add sm
codeExpr sm (AssocA Mul l)           = P.Row $ mulExpr l Mul sm
codeExpr sm (AssocC SUnion l)        = P.Row $ mulExpr l Mul sm
codeExpr sm (C c)                    = symbol $ lookupC' sm c
codeExpr sm (FCall f [x] [])         =
  P.Row [symbol $ lookupC' sm f, parens $ codeExpr sm x]
codeExpr sm (FCall f l ns)           = call sm f l ns
codeExpr sm (New c l ns)             = call sm c l ns
codeExpr sm (Message a m l ns)       =
  P.Row [symbol $ lookupC' sm a, P.MO P.Point, call sm m l ns]
codeExpr sm (Field o f)              = P.Row [symbol $ lookupC' sm o,
  P.MO P.Point, symbol $ lookupC' sm f]
codeExpr sm (Case _ ps)              =
  if length ps < 2
    then error "Attempting to use multi-case codeExpr incorrectly"
    else P.Case (zip (map (codeExpr sm . fst) ps) (map (codeExpr sm . snd) ps))
codeExpr sm (Matrix a)                  = P.Mtx $ map (map (codeExpr sm)) a
codeExpr sm (Set _ a)                   = P.Row $ map (codeExpr sm) a
codeExpr sm (Variable _ l)              = codeExpr sm l
codeExpr sm (UnaryOp Log u)             = mkCall sm P.Log u
codeExpr sm (UnaryOp Ln u)              = mkCall sm P.Ln u
codeExpr sm (UnaryOp Sin u)             = mkCall sm P.Sin u
codeExpr sm (UnaryOp Cos u)             = mkCall sm P.Cos u
codeExpr sm (UnaryOp Tan u)             = mkCall sm P.Tan u
codeExpr sm (UnaryOp Sec u)             = mkCall sm P.Sec u
codeExpr sm (UnaryOp Csc u)             = mkCall sm P.Csc u
codeExpr sm (UnaryOp Cot u)             = mkCall sm P.Cot u
codeExpr sm (UnaryOp Arcsin u)          = mkCall sm P.Arcsin u
codeExpr sm (UnaryOp Arccos u)          = mkCall sm P.Arccos u
codeExpr sm (UnaryOp Arctan u)          = mkCall sm P.Arctan u
codeExpr sm (UnaryOp Exp u)             = P.Row [P.MO P.Exp, P.Sup $ codeExpr sm u]
codeExpr sm (UnaryOp Abs u)             = P.Fenced P.Abs P.Abs $ codeExpr sm u
codeExpr sm (UnaryOpB Not u)            = P.Row [P.MO P.Not, codeExpr sm u]
codeExpr sm (UnaryOpVN Norm u)          = P.Fenced P.Norm P.Norm $ codeExpr sm u
codeExpr sm (UnaryOpVN Dim u)           = mkCall sm P.Dim u
codeExpr sm (UnaryOp Sqrt u)            = P.Sqrt $ codeExpr sm u
codeExpr sm (UnaryOp Neg u)             = neg sm u
codeExpr sm (UnaryOpVV NegV u)          = neg sm u
codeExpr sm (ArithBinaryOp Frac a b)    = P.Div (codeExpr sm a) (codeExpr sm b)
codeExpr sm (ArithBinaryOp Pow a b)     = pow sm a b
codeExpr sm (ArithBinaryOp Subt a b)    = P.Row [codeExpr sm a, P.MO P.Subt, codeExpr sm b]
codeExpr sm (EqBinaryOp Eq a b)         = mkBOp sm P.Eq a b
codeExpr sm (EqBinaryOp NEq a b)        = mkBOp sm P.NEq a b
codeExpr sm (LABinaryOp Index a b)      = indx sm a b
codeExpr sm (LABinaryOp IndexOf a b)    = indx sm a b
codeExpr sm (OrdBinaryOp Lt a b)        = mkBOp sm P.Lt a b
codeExpr sm (OrdBinaryOp Gt a b)        = mkBOp sm P.Gt a b
codeExpr sm (OrdBinaryOp LEq a b)       = mkBOp sm P.LEq a b
codeExpr sm (OrdBinaryOp GEq a b)       = mkBOp sm P.GEq a b
codeExpr sm (VVNBinaryOp Dot a b)       = mkBOp sm P.Dot a b
codeExpr sm (VVVBinaryOp Cross a b)     = mkBOp sm P.Cross a b
codeExpr sm (VVVBinaryOp VAdd a b)      = mkBOp sm P.VAdd a b
codeExpr sm (VVVBinaryOp VSub a b)      = mkBOp sm P.VSub a b
codeExpr sm (NVVBinaryOp Scale a b)     = mkBOp sm P.Scale a b
codeExpr sm (ESSBinaryOp SAdd a b)      = mkBOp sm P.SAdd a b
codeExpr sm (ESSBinaryOp SRemove a b)   = mkBOp sm P.SRemove a b
codeExpr sm (ESBBinaryOp SContains a b) = mkBOp sm P.SContains a b
codeExpr sm (Operator o d e)            = eop sm o d e
codeExpr sm (RealI c ri)                = renderRealInt sm (lookupC' sm c) ri

-- | Common method of converting associative operations into printable layout AST.
assocExpr :: P.Ops -> Int -> [CodeExpr] -> PrintingInformation -> P.Expr
assocExpr op prec exprs sm = P.Row $ intersperse (P.MO op) $ map (expr' sm prec) exprs

-- | Helper for rendering printable expressions.
addExpr :: [CodeExpr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
addExpr exprs o sm = addExprFilter (map (expr' sm (precA o)) exprs)

-- | Add add symbol only when the second Expr is not negation
addExprFilter :: [P.Expr] -> [P.Expr]
addExprFilter [] = []
addExprFilter [x] = [x]
addExprFilter (x1:P.Row[P.MO P.Neg, x2]:xs) = x1 : addExprFilter (P.Row[P.MO P.Neg, x2] : xs)
addExprFilter (x:xs) = x : P.MO P.Add : addExprFilter xs

-- | Helper for rendering printable expressions.
mulExpr ::  [CodeExpr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
mulExpr (hd1:hd2:tl) o sm = case (hd1, hd2) of
  (a, Lit (Int _))      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Lit (ExactDbl _)) ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Lit (Dbl _))      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, _)                ->  [expr' sm (precA o) a, P.MO P.Mul] ++ mulExpr (hd2 : tl) o sm
mulExpr [hd]         o sm = [expr' sm (precA o) hd]
mulExpr []           o sm = [expr' sm (precA o) (int 1)]

-- | Helper that adds parenthesis to the first expression. The second expression
-- is written as a superscript attached to the first.
withParens :: PrintingInformation -> CodeExpr -> CodeExpr -> P.Expr
withParens prI a b = P.Row [parens (codeExpr prI a), P.Sup (codeExpr prI b)]

-- | Helper for properly rendering exponents.
pow :: PrintingInformation -> CodeExpr -> CodeExpr -> P.Expr
pow prI a@(AssocA Add _)          b = withParens prI a b
pow prI a@(AssocA Mul _)         b = withParens prI a b
pow prI a@(ArithBinaryOp Subt _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Frac _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Pow _ _)  b = withParens prI a b
pow prI a                          b = P.Row [codeExpr prI a, P.Sup (codeExpr prI b)]

-- | Print a 'RealInterval'.
renderRealInt :: PrintingInformation -> S.Symbol -> RealInterval CodeExpr CodeExpr -> P.Expr
renderRealInt st s (Bounded (Inc,a) (Inc,b)) =
  P.Row [codeExpr st a, P.MO P.LEq, symbol s, P.MO P.LEq, codeExpr st b]
renderRealInt st s (Bounded (Inc,a) (Exc,b)) =
  P.Row [codeExpr st a, P.MO P.LEq, symbol s, P.MO P.Lt, codeExpr st b]
renderRealInt st s (Bounded (Exc,a) (Inc,b)) =
  P.Row [codeExpr st a, P.MO P.Lt, symbol s, P.MO P.LEq, codeExpr st b]
renderRealInt st s (Bounded (Exc,a) (Exc,b)) =
  P.Row [codeExpr st a, P.MO P.Lt, symbol s, P.MO P.Lt, codeExpr st b]
renderRealInt st s (UpTo (Inc,a))   = P.Row [symbol s, P.MO P.LEq, codeExpr st a]
renderRealInt st s (UpTo (Exc,a))   = P.Row [symbol s, P.MO P.Lt,  codeExpr st a]
renderRealInt st s (UpFrom (Inc,a)) = P.Row [symbol s, P.MO P.GEq, codeExpr st a]
renderRealInt st s (UpFrom (Exc,a)) = P.Row [symbol s, P.MO P.Gt,  codeExpr st a]
