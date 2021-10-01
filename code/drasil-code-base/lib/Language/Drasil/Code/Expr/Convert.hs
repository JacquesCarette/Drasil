module Language.Drasil.Code.Expr.Convert (
    expr, realInterval, constraint
) where

import qualified Language.Drasil as L
import qualified Language.Drasil.Expr.Development as LD

import Data.Bifunctor (Bifunctor(bimap, second))

import Language.Drasil.Code.Expr

-- | Render an algebraic expression into our code expression language.
expr :: LD.Expr -> CodeExpr
expr (LD.Dbl d) = Dbl d
expr (LD.Int i) = Int i
expr (LD.ExactDbl i) = ExactDbl i
expr (LD.Str s) = Str s
expr (LD.Perc n d) = Perc n d
expr (LD.AssocA ao es) = AssocA (assocArithOp ao) $ map expr es
expr (LD.AssocB bo es) = AssocB (assocBoolOp bo) $ map expr es
expr (LD.C u) = C u
expr (LD.FCall u es ns) = FCall u (map expr es) (map (second expr) ns)
expr (LD.Case c es) = Case c $ map (bimap expr expr) es
expr (LD.Matrix es) = Matrix $ map (map expr) es
expr (LD.UnaryOp uo e) = UnaryOp (uFunc uo) (expr e)
expr (LD.UnaryOpB uo e) = UnaryOpB (uFuncB uo) (expr e)
expr (LD.UnaryOpVV uo e) = UnaryOpVV (uFuncVV uo) (expr e)
expr (LD.UnaryOpVN uo e) = UnaryOpVN (uFuncVN uo) (expr e)
expr (LD.ArithBinaryOp bo l r) = ArithBinaryOp (arithBinOp bo) (expr l) (expr r)
expr (LD.BoolBinaryOp bo l r) = BoolBinaryOp (boolBinOp bo) (expr l) (expr r)
expr (LD.EqBinaryOp bo l r) = EqBinaryOp (eqBinOp bo) (expr l) (expr r)
expr (LD.LABinaryOp bo l r) = LABinaryOp (laBinOp bo) (expr l) (expr r)
expr (LD.OrdBinaryOp bo l r) = OrdBinaryOp (ordBinOp bo) (expr l) (expr r)
expr (LD.VVVBinaryOp bo l r) = VVVBinaryOp (vvvBinOp bo) (expr l) (expr r)
expr (LD.VVNBinaryOp bo l r) = VVNBinaryOp (vvnBinOp bo) (expr l) (expr r)
expr (LD.Operator aao dd e) = Operator (assocArithOp aao) (renderDomainDesc dd) (expr e)
expr (LD.RealI u ri) = RealI u (realInterval ri)

-- | Convert 'RealInterval' 'Expr' 'Expr's into 'RealInterval' 'CodeExpr' 'CodeExpr's.
realInterval :: L.RealInterval L.Expr L.Expr -> L.RealInterval CodeExpr CodeExpr
realInterval (L.Bounded (il, el) (ir, er)) = L.Bounded (il, expr el) (ir, expr er)
realInterval (L.UpTo (i, e)) = L.UpTo (i, expr e)
realInterval (L.UpFrom (i, e)) = L.UpFrom (i, expr e)

-- | Convert constrained expressions ('ConstraintE') into 'Constraint''CodeExpr's.
constraint :: L.ConstraintE -> L.Constraint CodeExpr
constraint (L.Range r ri) = L.Range r (realInterval ri)

-- | Convert 'DomainDesc Expr Expr' into 'DomainDesc CodeExpr CodeExpr's.
renderDomainDesc :: L.DomainDesc L.Expr L.Expr -> L.DomainDesc CodeExpr CodeExpr
renderDomainDesc (L.BoundedDD s t l r) = L.BoundedDD s t (expr l) (expr r)
renderDomainDesc (L.AllDD s t) = L.AllDD s t

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

vvnBinOp :: LD.VVNBinOp -> VVNBinOp
vvnBinOp LD.Dot = Dot

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
