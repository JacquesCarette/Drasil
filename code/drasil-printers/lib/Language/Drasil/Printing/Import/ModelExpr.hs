{-# LANGUAGE GADTs #-}

-- | Defines functions to render 'CodeExpr's as printable 'P.Expr's.
module Language.Drasil.Printing.Import.ModelExpr where -- TODO: tighten exports

-- TODO: tighten exports
import Language.Drasil (UID, DomainDesc(..), RealInterval(..), Inclusive(..),
  RTopology(..), Special(..), LiteralC(int))
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.Literal.Development (Literal(..))
import Language.Drasil.ModelExpr.Development

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, ckdb, stg)

import Control.Lens ((^.))
import Data.List (intersperse)

import Language.Drasil.Printing.Import.Literal (literal)
import Language.Drasil.Printing.Import.Space (space)
import Language.Drasil.Printing.Import.Symbol (symbol)
import Language.Drasil.Printing.Import.Helpers (lookupC, parens)

-- | Helper that adds parenthesis to a display expression where appropriate.
modelExpr' :: PrintingInformation -> Int -> ModelExpr -> P.Expr
modelExpr' s p e = fence $ modelExpr e s
  where fence = if mePrec e < p then parens else id

-- | Helper that creates an expression row given printing information, an operator, and an expression.
mkCall :: PrintingInformation -> P.Ops -> ModelExpr -> P.Expr
mkCall s o e = P.Row [P.MO o, parens $ modelExpr e s]

-- | Helper that creates a binary expression row given printing information, an operator, and two expressions.
mkBOp :: PrintingInformation -> P.Ops -> ModelExpr -> ModelExpr -> P.Expr
mkBOp sm o a b = P.Row [modelExpr a sm, P.MO o, modelExpr b sm]

-- | Helper for properly rendering negation of expressions.
neg' :: ModelExpr -> Bool
neg' (Lit (Dbl _))          = True
neg' (Lit (Int _))          = True
neg' (Lit (ExactDbl _))     = True
neg' Operator{}             = True
neg' (AssocA MulI _)        = True
neg' (AssocA MulRe _)       = True
neg' (LABinaryOp Index _ _) = True
neg' (UnaryOp _ _)          = True
neg' (UnaryOpB _ _)         = True
neg' (UnaryOpVV _ _)        = True
neg' (C _)                  = True
neg' _                      = False

-- | Render negated expressions.
neg :: PrintingInformation -> ModelExpr -> P.Expr
neg sm a = P.Row [P.MO P.Neg, (if neg' a then id else parens) $ modelExpr a sm]

-- | For printing indexes.
indx :: PrintingInformation -> ModelExpr -> ModelExpr -> P.Expr
indx sm (C c) i = f s
  where
    i' = modelExpr i sm
    s = lookupC (sm ^. stg) (sm ^. ckdb) c
    f (Corners [] [] [] [b] e) =
      let e' = symbol e
          b' = symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(Variable _) = P.Row [symbol a, P.Sub i']
    f a@(Label _)    = P.Row [symbol a, P.Sub i']
--    f a@(Greek _)  = P.Row [symbol a, P.Sub i']
    f   e          = let e' = symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [modelExpr a sm], P.Sub $ modelExpr i sm]

-- | For printing expressions that call something.
call :: PrintingInformation -> UID -> [ModelExpr] -> [(UID, ModelExpr)] -> P.Expr
call sm f ps ns = P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f,
  parens $ P.Row $ intersperse (P.MO P.Comma) $ map (`modelExpr` sm) ps ++
  zipWith (\n a -> P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) n,
  P.MO P.Eq, modelExpr a sm]) (map fst ns) (map snd ns)]

-- | Helper function for addition 'EOperator's.
eopAdds :: PrintingInformation -> DomainDesc t ModelExpr ModelExpr -> ModelExpr -> P.Expr
eopAdds sm (BoundedDD v Continuous l h) e =
  P.Row [P.MO P.Inte, P.Sub (modelExpr l sm), P.Sup (modelExpr h sm),
         P.Row [modelExpr e sm], P.Spc P.Thin, P.Ident "d", symbol v]
eopAdds sm (AllDD v Continuous) e =
  P.Row [P.MO P.Inte, P.Sub (symbol v), P.Row [modelExpr e sm], P.Spc P.Thin,
         P.Ident "d", symbol v]
eopAdds sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Summ, P.Sub (P.Row [symbol v, P.MO P.Eq, modelExpr l sm]), P.Sup (modelExpr h sm),
         P.Row [modelExpr e sm]]
eopAdds sm (AllDD _ Discrete) e = P.Row [P.MO P.Summ, P.Row [modelExpr e sm]]

-- | Helper function for multiplicative 'EOperator's.
eopMuls :: PrintingInformation -> DomainDesc t ModelExpr ModelExpr -> ModelExpr -> P.Expr
eopMuls sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Prod, P.Sub (P.Row [symbol v, P.MO P.Eq, modelExpr l sm]), P.Sup (modelExpr h sm),
         P.Row [modelExpr e sm]]
eopMuls sm (AllDD _ Discrete) e = P.Row [P.MO P.Prod, P.Row [modelExpr e sm]]
eopMuls _ (AllDD _ Continuous) _ = error "Printing/Import.hs Product-Integral not implemented."
eopMuls _ (BoundedDD _ Continuous _ _) _ = error "Printing/Import.hs Product-Integral not implemented."


-- | Helper function for translating 'EOperator's.
eop :: PrintingInformation -> AssocArithOper -> DomainDesc t ModelExpr ModelExpr -> ModelExpr -> P.Expr
eop sm AddI = eopAdds sm
eop sm AddRe = eopAdds sm
eop sm MulI = eopMuls sm
eop sm MulRe = eopMuls sm

-- | Helper function for display nth derivative
sup :: Integer -> [P.Expr]
sup n | n == 1 = []
      | n > 1 = [P.Sup (P.Int n)]
      | otherwise = error "non-positive argument to derivative"

-- | Translate Exprs to printable layout AST.
modelExpr :: ModelExpr -> PrintingInformation -> P.Expr
modelExpr (Lit l)                    sm = literal l sm
modelExpr (AssocB And l)             sm = assocExpr P.And (precB And) l sm
modelExpr (AssocB Or l)              sm = assocExpr P.Or (precB Or) l sm
modelExpr (AssocB Equivalence l)     sm = assocExpr P.Eq (precB Equivalence) l sm
modelExpr (AssocA AddI l)            sm = P.Row $ addExpr l AddI sm
modelExpr (AssocA AddRe l)           sm = P.Row $ addExpr l AddRe sm
modelExpr (AssocA MulI l)            sm = P.Row $ mulExpr l MulI sm
modelExpr (AssocA MulRe l)           sm = P.Row $ mulExpr l MulRe sm
modelExpr (Deriv 0 Part a _)         sm = P.Row [modelExpr a sm]
modelExpr (Deriv 0 Total a _)        sm = P.Row [modelExpr a sm]
modelExpr (Deriv n Part a b)         sm =
  let st = [P.Spc P.Thin, P.Spec Partial] in 
    P.Div (P.Row (st ++ sup n ++ [modelExpr a sm]))
    (P.Row (st ++ [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) b] ++ sup n))
modelExpr (Deriv n Total a b)        sm =
  let st = [P.Spc P.Thin, P.Ident "d"] in
    P.Div (P.Row (st ++ sup n ++ [modelExpr a sm]))
        (P.Row (st ++ [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) b] ++ sup n))
modelExpr (C c)                      sm = symbol $ lookupC (sm ^. stg) (sm ^. ckdb) c
modelExpr (FCall f [x] [])           sm =
  P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f, parens $ modelExpr x sm]
modelExpr (FCall f l ns)             sm = call sm f l ns
modelExpr (Case _ ps)                sm =
  if length ps < 2
    then error "Attempting to use multi-case modelExpr incorrectly"
    else P.Case (zip (map (flip modelExpr sm . fst) ps) (map (flip modelExpr sm . snd) ps))
modelExpr (Matrix a)                 sm = P.Mtx $ map (map (`modelExpr` sm)) a
modelExpr (UnaryOp Log u)            sm = mkCall sm P.Log u
modelExpr (UnaryOp Ln u)             sm = mkCall sm P.Ln u
modelExpr (UnaryOp Sin u)            sm = mkCall sm P.Sin u
modelExpr (UnaryOp Cos u)            sm = mkCall sm P.Cos u
modelExpr (UnaryOp Tan u)            sm = mkCall sm P.Tan u
modelExpr (UnaryOp Sec u)            sm = mkCall sm P.Sec u
modelExpr (UnaryOp Csc u)            sm = mkCall sm P.Csc u
modelExpr (UnaryOp Cot u)            sm = mkCall sm P.Cot u
modelExpr (UnaryOp Arcsin u)         sm = mkCall sm P.Arcsin u
modelExpr (UnaryOp Arccos u)         sm = mkCall sm P.Arccos u
modelExpr (UnaryOp Arctan u)         sm = mkCall sm P.Arctan u
modelExpr (UnaryOp Exp u)            sm = P.Row [P.MO P.Exp, P.Sup $ modelExpr u sm]
modelExpr (UnaryOp Abs u)            sm = P.Fenced P.Abs P.Abs $ modelExpr u sm
modelExpr (UnaryOpB Not u)           sm = P.Row [P.MO P.Not, modelExpr u sm]
modelExpr (UnaryOpVN Norm u)         sm = P.Fenced P.Norm P.Norm $ modelExpr u sm
modelExpr (UnaryOpVN Dim u)          sm = mkCall sm P.Dim u
modelExpr (UnaryOp Sqrt u)           sm = P.Sqrt $ modelExpr u sm
modelExpr (UnaryOp Neg u)            sm = neg sm u
modelExpr (UnaryOpVV NegV u)         sm = neg sm u
modelExpr (ArithBinaryOp Frac a b)   sm = P.Div (modelExpr a sm) (modelExpr b sm)
modelExpr (ArithBinaryOp Pow a b)    sm = pow sm a b
modelExpr (ArithBinaryOp Subt a b)   sm = P.Row [modelExpr a sm, P.MO P.Subt, modelExpr b sm]
modelExpr (BoolBinaryOp Impl a b)    sm = mkBOp sm P.Impl a b
modelExpr (BoolBinaryOp Iff a b)     sm = mkBOp sm P.Iff a b
modelExpr (EqBinaryOp Eq a b)        sm = mkBOp sm P.Eq a b
modelExpr (EqBinaryOp NEq a b)       sm = mkBOp sm P.NEq a b
modelExpr (LABinaryOp Index a b)     sm = indx sm a b
modelExpr (OrdBinaryOp Lt a b)       sm = mkBOp sm P.Lt a b
modelExpr (OrdBinaryOp Gt a b)       sm = mkBOp sm P.Gt a b
modelExpr (OrdBinaryOp LEq a b)      sm = mkBOp sm P.LEq a b
modelExpr (OrdBinaryOp GEq a b)      sm = mkBOp sm P.GEq a b
modelExpr (VVNBinaryOp Dot a b)      sm = mkBOp sm P.Dot a b
modelExpr (VVVBinaryOp Cross a b)    sm = mkBOp sm P.Cross a b
modelExpr (Operator o d e)           sm = eop sm o d e
modelExpr (RealI c ri)               sm = renderRealInt sm (lookupC (sm ^. stg)
  (sm ^. ckdb) c) ri
modelExpr (Spc s)                    sm = space sm s
modelExpr (SpaceBinaryOp IsIn l r)   sm = P.Row [modelExpr l sm, P.MO P.IsIn, modelExpr r sm]
modelExpr (StatBinaryOp Defines l r) sm = P.Row [modelExpr l sm, P.MO P.Eq, modelExpr r sm]
modelExpr (ForAll c s de)            sm = P.Row [
    P.MO P.ForAll, symbol $ lookupC (sm ^. stg) (sm ^. ckdb) c, P.MO P.IsIn, space sm s,
    P.MO P.Dot, modelExpr de sm
  ]

-- | Common method of converting associative operations into printable layout AST.
assocExpr :: P.Ops -> Int -> [ModelExpr] -> PrintingInformation -> P.Expr
assocExpr op prec exprs sm = P.Row $ intersperse (P.MO op) $ map (modelExpr' sm prec) exprs

-- | Helper for rendering printable expressions.
addExpr :: [ModelExpr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
addExpr exprs o sm = addExprFilter (map (modelExpr' sm (precA o)) exprs)

-- | Add add symbol only when the second Expr is not negation 
addExprFilter :: [P.Expr] -> [P.Expr]
addExprFilter [] = []
addExprFilter [x] = [x]
addExprFilter (x1:P.Row[P.MO P.Neg, x2]:xs) = x1 : addExprFilter (P.Row[P.MO P.Neg, x2] : xs)
addExprFilter (x:xs) = x : P.MO P.Add : addExprFilter xs

-- | Helper for rendering printable expressions.
mulExpr ::  [ModelExpr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
mulExpr (hd1:hd2:tl) o sm = case (hd1, hd2) of
  (a, Lit (Int _))      ->  [modelExpr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Lit (ExactDbl _)) ->  [modelExpr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Lit (Dbl _))      ->  [modelExpr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, _)                ->  [modelExpr' sm (precA o) a, P.MO P.Mul] ++ mulExpr (hd2 : tl) o sm
mulExpr [hd]         o sm = [modelExpr' sm (precA o) hd]
mulExpr []           o sm = [modelExpr' sm (precA o) (int 1)]


-- | Helper that adds parenthesis to the first expression. The second expression
-- is written as a superscript attached to the first.
withParens :: PrintingInformation -> ModelExpr -> ModelExpr -> P.Expr
withParens prI a b = P.Row [parens (modelExpr a prI), P.Sup (modelExpr b prI)]

-- | Helper for properly rendering exponents.
pow :: PrintingInformation -> ModelExpr -> ModelExpr -> P.Expr
pow prI a@(AssocA AddI _)          b = withParens prI a b
pow prI a@(AssocA AddRe _)         b = withParens prI a b
pow prI a@(AssocA MulI _)          b = withParens prI a b
pow prI a@(AssocA MulRe _)         b = withParens prI a b
pow prI a@(ArithBinaryOp Subt _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Frac _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Pow _ _)  b = withParens prI a b
pow prI a                          b = P.Row [modelExpr a prI, P.Sup (modelExpr b prI)]

-- | Print a 'RealInterval'.
renderRealInt :: PrintingInformation -> Symbol -> RealInterval ModelExpr ModelExpr -> P.Expr
renderRealInt st s (Bounded (Inc,a) (Inc,b)) =
  P.Row [modelExpr a st, P.MO P.LEq, symbol s, P.MO P.LEq, modelExpr b st]
renderRealInt st s (Bounded (Inc,a) (Exc,b)) =
  P.Row [modelExpr a st, P.MO P.LEq, symbol s, P.MO P.Lt, modelExpr b st]
renderRealInt st s (Bounded (Exc,a) (Inc,b)) =
  P.Row [modelExpr a st, P.MO P.Lt, symbol s, P.MO P.LEq, modelExpr b st]
renderRealInt st s (Bounded (Exc,a) (Exc,b)) =
  P.Row [modelExpr a st, P.MO P.Lt, symbol s, P.MO P.Lt, modelExpr b st]
renderRealInt st s (UpTo (Inc,a))   = P.Row [symbol s, P.MO P.LEq, modelExpr a st]
renderRealInt st s (UpTo (Exc,a))   = P.Row [symbol s, P.MO P.Lt,  modelExpr a st]
renderRealInt st s (UpFrom (Inc,a)) = P.Row [symbol s, P.MO P.GEq, modelExpr a st]
renderRealInt st s (UpFrom (Exc,a)) = P.Row [symbol s, P.MO P.Gt,  modelExpr a st]

