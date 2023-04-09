{-# LANGUAGE GADTs #-}
module Language.Drasil.CodeExpr.Convert (
    expr, realInterval, constraint,
    CanGenCode(..)
) where

import Language.Drasil.Space (RealInterval(..), DiscreteDomainDesc, DomainDesc(BoundedDD))
import Language.Drasil.Constraint (Constraint(..), ConstraintE)
import qualified Language.Drasil.Expr.Lang as E
import qualified Language.Drasil.Expr.Development as LD
import qualified Language.Drasil.Literal.Development as LL

import Language.Drasil.CodeExpr.Lang

import Data.Bifunctor (Bifunctor(bimap))

class CanGenCode e where
    toCodeExpr :: e -> CodeExpr

instance CanGenCode LL.Literal where
    toCodeExpr = Lit

instance CanGenCode LD.Expr where
    toCodeExpr = expr

-- | Render an algebraic expression into our code expression language.
expr :: LD.Expr -> CodeExpr
expr (LD.Lit l)                = Lit l
expr (LD.AssocA ao es)         = AssocA (assocArithOp ao) $ map expr es
expr (LD.AssocB bo es)         = AssocB (assocBoolOp bo) $ map expr es
expr (LD.C u)                  = C u
expr (LD.FCall u es)           = FCall u (map expr es) []
expr (LD.Case c es)            = Case c $ map (bimap expr expr) es
expr (LD.Matrix es)            = Matrix $ map (map expr) es
expr (LD.UnaryOp uo e)         = UnaryOp (uFunc uo) (expr e)
expr (LD.UnaryOpB uo e)        = UnaryOpB (uFuncB uo) (expr e)
expr (LD.UnaryOpVV uo e)       = UnaryOpVV (uFuncVV uo) (expr e)
expr (LD.UnaryOpVN uo e)       = UnaryOpVN (uFuncVN uo) (expr e)
expr (LD.ArithBinaryOp bo l r) = ArithBinaryOp (arithBinOp bo) (expr l) (expr r)
expr (LD.BoolBinaryOp bo l r)  = BoolBinaryOp (boolBinOp bo) (expr l) (expr r)
expr (LD.EqBinaryOp bo l r)    = EqBinaryOp (eqBinOp bo) (expr l) (expr r)
expr (LD.LABinaryOp bo l r)    = LABinaryOp (laBinOp bo) (expr l) (expr r)
expr (LD.OrdBinaryOp bo l r)   = OrdBinaryOp (ordBinOp bo) (expr l) (expr r)
expr (LD.VVVBinaryOp bo l r)   = VVVBinaryOp (vvvBinOp bo) (expr l) (expr r)
expr (LD.VVNBinaryOp bo l r)   = VVNBinaryOp (vvnBinOp bo) (expr l) (expr r)
expr (LD.NVVBinaryOp bo l r)   = NVVBinaryOp (nvvBinOp bo) (expr l) (expr r)
expr (LD.Operator aao dd e)    = Operator (assocArithOp aao) (renderDomainDesc dd) (expr e)
expr (LD.RealI u ri)           = RealI u (realInterval ri)

-- | Convert 'RealInterval' 'Expr' 'Expr's into 'RealInterval' 'CodeExpr' 'CodeExpr's.
realInterval :: RealInterval E.Expr E.Expr -> RealInterval CodeExpr CodeExpr
realInterval (Bounded (il, el) (ir, er)) = Bounded (il, expr el) (ir, expr er)
realInterval (UpTo (i, e))               = UpTo (i, expr e)
realInterval (UpFrom (i, e))             = UpFrom (i, expr e)

-- | Convert constrained expressions ('ConstraintE') into 'Constraint''CodeExpr's.
constraint :: ConstraintE -> Constraint CodeExpr
constraint (Range r ri) = Range r (realInterval ri)

-- | Convert 'DomainDesc Expr Expr' into 'DomainDesc CodeExpr CodeExpr's.
renderDomainDesc :: DiscreteDomainDesc E.Expr E.Expr -> DiscreteDomainDesc CodeExpr CodeExpr
renderDomainDesc (BoundedDD s t l r) = BoundedDD s t (expr l) (expr r)

arithBinOp :: LD.ArithBinOp -> ArithBinOp
arithBinOp LD.Frac = Frac
arithBinOp LD.Pow = Pow
arithBinOp LD.Subt = Subt

eqBinOp :: LD.EqBinOp -> EqBinOp
eqBinOp LD.Eq = Eq
eqBinOp LD.NEq = NEq

boolBinOp :: LD.BoolBinOp -> BoolBinOp
boolBinOp LD.Impl = Impl
boolBinOp LD.Iff = Iff

laBinOp :: LD.LABinOp -> LABinOp
laBinOp LD.Index = Index

ordBinOp :: LD.OrdBinOp -> OrdBinOp
ordBinOp LD.Lt  = Lt
ordBinOp LD.Gt  = Gt
ordBinOp LD.LEq = LEq
ordBinOp LD.GEq = GEq

vvvBinOp :: LD.VVVBinOp -> VVVBinOp
vvvBinOp LD.Cross = Cross
vvvBinOp LD.VAdd = VAdd
vvvBinOp LD.VSub = VSub

vvnBinOp :: LD.VVNBinOp -> VVNBinOp
vvnBinOp LD.Dot = Dot

nvvBinOp :: LD.NVVBinOp -> NVVBinOp
nvvBinOp LD.Scale = Scale

assocArithOp :: LD.AssocArithOper -> AssocArithOper
assocArithOp LD.AddI = AddI -- TODO: These L.'s should be exported through L.D.Development
assocArithOp LD.AddRe = AddRe
assocArithOp LD.MulI = MulI
assocArithOp LD.MulRe = MulRe

assocBoolOp :: LD.AssocBoolOper -> AssocBoolOper
assocBoolOp LD.And = And -- TODO: These L.'s should be exported through L.D.Development
assocBoolOp LD.Or = Or

uFunc :: LD.UFunc -> UFunc
uFunc LD.Abs = Abs -- TODO: These L.'s should be exported through L.D.Development
uFunc LD.Log = Log
uFunc LD.Ln = Ln
uFunc LD.Sin = Sin
uFunc LD.Cos = Cos
uFunc LD.Tan = Tan
uFunc LD.Sec = Sec
uFunc LD.Csc = Csc
uFunc LD.Cot = Cot
uFunc LD.Arcsin = Arcsin
uFunc LD.Arccos = Arccos
uFunc LD.Arctan = Arctan
uFunc LD.Exp = Exp
uFunc LD.Sqrt = Sqrt
uFunc LD.Neg = Neg

uFuncB :: LD.UFuncB -> UFuncB
uFuncB LD.Not = Not

uFuncVV :: LD.UFuncVV -> UFuncVV
uFuncVV LD.NegV = NegV

uFuncVN :: LD.UFuncVN -> UFuncVN
uFuncVN LD.Norm = Norm
uFuncVN LD.Dim = Dim
