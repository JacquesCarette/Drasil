module Language.Drasil.Code.Expr.Render (renderExpr) where

import qualified Language.Drasil as L
import qualified Language.Drasil.Development as LD

import Data.Bifunctor (Bifunctor(bimap, second))

import Language.Drasil.Code.Expr

-- | Render an algebraic expression into our code expression language
renderExpr :: L.Expr -> CodeExpr
renderExpr (L.Dbl d) = Dbl d
renderExpr (L.Int i) = Int i
renderExpr (L.ExactDbl i) = ExactDbl i 
renderExpr (L.Str s) = Str s
renderExpr (L.Perc n d) = Perc n d
renderExpr (L.AssocA ao es) = AssocA (renderAssocArithOp ao) $ map renderExpr es
renderExpr (L.AssocB bo es) = AssocB (renderAssocBoolOp bo) $ map renderExpr es
renderExpr (L.Deriv dt e u) = Deriv dt (renderExpr e) u
renderExpr (L.C u) = C u
renderExpr (L.FCall u es ns) = FCall u (map renderExpr es) (map (second renderExpr) ns)
renderExpr (L.Case c es) = Case c $ map (bimap renderExpr renderExpr) es
renderExpr (L.Matrix es) = Matrix $ map (map renderExpr) es
renderExpr (L.UnaryOp uo e) = UnaryOp (renderUFunc uo) (renderExpr e)
renderExpr (L.UnaryOpB uo e) = UnaryOpB (renderUFuncB uo) (renderExpr e)
renderExpr (L.UnaryOpVec uo e) = UnaryOpVec (renderUFuncVec uo) (renderExpr e)
renderExpr (L.ArithBinaryOp bo l r) = ArithBinaryOp (renderArithBinOp bo) (renderExpr l) (renderExpr r)
renderExpr (L.BoolBinaryOp bo l r) = BoolBinaryOp (renderBoolBinOp bo) (renderExpr l) (renderExpr r)
renderExpr (L.EqBinaryOp bo l r) = EqBinaryOp (renderEqBinOp bo) (renderExpr l) (renderExpr r)
renderExpr (L.LABinaryOp bo l r) = LABinaryOp (renderLABinOp bo) (renderExpr l) (renderExpr r)
renderExpr (L.OrdBinaryOp bo l r) = OrdBinaryOp (renderOrdBinOp bo) (renderExpr l) (renderExpr r)
renderExpr (L.VVVBinaryOp bo l r) = VVVBinaryOp (renderVVVBinOp bo) (renderExpr l) (renderExpr r)
renderExpr (L.VVNBinaryOp bo l r) = VVNBinaryOp (renderVVNBinOp bo) (renderExpr l) (renderExpr r)
renderExpr (L.Operator aao dd e) = Operator (renderAssocArithOp aao) (renderDomainDesc dd) (renderExpr e)
renderExpr (L.RealI u ri) = RealI u (renderRI ri)

renderDomainDesc :: L.DomainDesc L.Expr L.Expr -> L.DomainDesc CodeExpr CodeExpr
renderDomainDesc (L.BoundedDD s t l r) = L.BoundedDD s t (renderExpr l) (renderExpr r)
renderDomainDesc (L.AllDD s t) = L.AllDD s t

renderRI :: L.RealInterval L.Expr L.Expr -> L.RealInterval CodeExpr CodeExpr
renderRI (L.Bounded (incl, el) (incr, er)) = L.Bounded (incl, renderExpr el) (incr, renderExpr er)
renderRI (L.UpTo (inc, e)) = L.UpTo (inc, renderExpr e)
renderRI (L.UpFrom (inc, e)) = L.UpFrom (inc, renderExpr e)

renderArithBinOp :: L.ArithBinOp -> ArithBinOp
renderArithBinOp LD.Frac = Frac
renderArithBinOp LD.Pow = Pow
renderArithBinOp LD.Subt = Subt

renderEqBinOp :: L.EqBinOp -> EqBinOp
renderEqBinOp LD.Eq = Eq
renderEqBinOp LD.NEq = NEq

renderBoolBinOp :: L.BoolBinOp -> BoolBinOp
renderBoolBinOp LD.Impl = Impl
renderBoolBinOp LD.Iff = Iff

renderLABinOp :: L.LABinOp -> LABinOp
renderLABinOp LD.Index = Index

renderOrdBinOp :: L.OrdBinOp -> OrdBinOp
renderOrdBinOp LD.Lt  = Lt
renderOrdBinOp LD.Gt  = Gt
renderOrdBinOp LD.LEq = LEq
renderOrdBinOp LD.GEq = GEq

renderVVVBinOp :: L.VVVBinOp -> VVVBinOp
renderVVVBinOp LD.Cross = Cross

renderVVNBinOp :: L.VVNBinOp -> VVNBinOp
renderVVNBinOp LD.Dot = Dot

renderAssocArithOp :: L.AssocArithOper -> AssocArithOper
renderAssocArithOp L.AddI = AddI -- TODO: These L.'s should be exported through L.D.Development
renderAssocArithOp L.AddRe = AddRe
renderAssocArithOp L.MulI = MulI
renderAssocArithOp L.MulRe = MulRe

renderAssocBoolOp :: L.AssocBoolOper -> AssocBoolOper
renderAssocBoolOp L.And = And -- TODO: These L.'s should be exported through L.D.Development
renderAssocBoolOp L.Or = Or

renderUFunc :: L.UFunc -> UFunc
renderUFunc L.Abs = Abs -- TODO: These L.'s should be exported through L.D.Development
renderUFunc L.Log = Log
renderUFunc L.Ln = Ln 
renderUFunc L.Sin = Sin 
renderUFunc L.Cos = Cos
renderUFunc L.Tan = Tan
renderUFunc L.Sec = Sec
renderUFunc L.Csc = Csc
renderUFunc L.Cot = Cot
renderUFunc L.Arcsin = Arcsin
renderUFunc L.Arccos = Arccos
renderUFunc L.Arctan = Arctan 
renderUFunc L.Exp = Exp 
renderUFunc L.Sqrt = Sqrt 
renderUFunc L.Neg = Neg 

renderUFuncB :: L.UFuncB -> UFuncB
renderUFuncB LD.Not = Not

renderUFuncVec :: L.UFuncVec -> UFuncVec
renderUFuncVec LD.Norm = Norm
renderUFuncVec LD.Dim = Dim
