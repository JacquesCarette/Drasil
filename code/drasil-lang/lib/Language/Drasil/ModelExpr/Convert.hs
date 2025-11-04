{-# LANGUAGE GADTs #-}
-- | Defines functions to convert from the base expression language to 'ModelExpr's.
module Language.Drasil.ModelExpr.Convert where

import Data.Bifunctor (bimap)

import Language.Drasil.Space
    (RealInterval(..), DiscreteDomainDesc, DomainDesc(BoundedDD))
import qualified Language.Drasil.Expr.Lang as E
import Language.Drasil.ModelExpr.Lang

assocArithOper :: E.AssocArithOper -> AssocArithOper
assocArithOper E.Add  = Add
assocArithOper E.Mul  = Mul

assocBoolOper :: E.AssocBoolOper -> AssocBoolOper
assocBoolOper E.And = And
assocBoolOper E.Or  = Or

assocConcatOper :: E.AssocConcatOper -> AssocConcatOper
assocConcatOper E.SUnion  = SUnion

uFunc :: E.UFunc -> UFunc
uFunc E.Abs    = Abs
uFunc E.Log    = Log
uFunc E.Ln     = Ln
uFunc E.Sin    = Sin
uFunc E.Cos    = Cos
uFunc E.Tan    = Tan
uFunc E.Sec    = Sec
uFunc E.Csc    = Csc
uFunc E.Cot    = Cot
uFunc E.Arcsin = Arcsin
uFunc E.Arccos = Arccos
uFunc E.Arctan = Arctan
uFunc E.Exp    = Exp
uFunc E.Sqrt   = Sqrt
uFunc E.Neg    = Neg

uFuncB :: E.UFuncB -> UFuncB
uFuncB E.Not = Not

uFuncVV :: E.UFuncVV -> UFuncVV
uFuncVV E.NegV = NegV

uFuncVN :: E.UFuncVN -> UFuncVN
uFuncVN E.Norm = Norm
uFuncVN E.Dim  = Dim

arithBinOp :: E.ArithBinOp -> ArithBinOp
arithBinOp E.Frac = Frac
arithBinOp E.Pow  = Pow
arithBinOp E.Subt = Subt

boolBinOp :: E.BoolBinOp -> BoolBinOp
boolBinOp E.Impl = Impl
boolBinOp E.Iff  = Iff

eqBinOp :: E.EqBinOp -> EqBinOp
eqBinOp E.Eq  = Eq
eqBinOp E.NEq = NEq

laBinOp :: E.LABinOp -> LABinOp
laBinOp E.Index = Index
laBinOp E.IndexOf = IndexOf

ordBinOp :: E.OrdBinOp -> OrdBinOp
ordBinOp E.Lt  = Lt
ordBinOp E.Gt  = Gt
ordBinOp E.LEq = LEq
ordBinOp E.GEq = GEq

vvvBinOp :: E.VVVBinOp -> VVVBinOp
vvvBinOp E.Cross = Cross
vvvBinOp E.VAdd = VAdd
vvvBinOp E.VSub = VSub

vvnBinOp :: E.VVNBinOp -> VVNBinOp
vvnBinOp E.Dot = Dot

nvvBinOp :: E.NVVBinOp -> NVVBinOp
nvvBinOp E.Scale = Scale

essBinOp :: E.ESSBinOp -> ESSBinOp
essBinOp E.SAdd = SAdd
essBinOp E.SRemove = SRemove

esbBinOp :: E.ESBBinOp -> ESBBinOp
esbBinOp E.SContains = SContains

expr :: E.Expr -> ModelExpr
expr (E.Lit a)               = Lit a
expr (E.AssocA ao es)        = AssocA (assocArithOper ao) $ map expr es
expr (E.AssocB bo es)        = AssocB (assocBoolOper bo) $ map expr es
expr (E.AssocC ao es)        = AssocC (assocConcatOper ao) $ map expr es
expr (E.C u)                 = C u
expr (E.FCall u es)          = FCall u (map expr es)
expr (E.Case c ces)          = Case c (map (bimap expr expr) ces)
expr (E.Matrix es)           = Matrix $ map (map expr) es
expr (E.Set s e)             = Set s $ map expr e
expr (E.Variable s e)        = Variable s $ expr e
expr (E.UnaryOp u e)         = UnaryOp (uFunc u) (expr e)
expr (E.UnaryOpB u e)        = UnaryOpB (uFuncB u) (expr e)
expr (E.UnaryOpVV u e)       = UnaryOpVV (uFuncVV u) (expr e)
expr (E.UnaryOpVN u e)       = UnaryOpVN (uFuncVN u) (expr e)
expr (E.ArithBinaryOp a l r) = ArithBinaryOp (arithBinOp a) (expr l) (expr r)
expr (E.BoolBinaryOp b l r)  = BoolBinaryOp (boolBinOp b) (expr l) (expr r)
expr (E.EqBinaryOp e l r)    = EqBinaryOp (eqBinOp e) (expr l) (expr r)
expr (E.LABinaryOp la l r)   = LABinaryOp (laBinOp la) (expr l) (expr r)
expr (E.OrdBinaryOp o l r)   = OrdBinaryOp (ordBinOp o) (expr l) (expr r)
expr (E.VVVBinaryOp v l r)   = VVVBinaryOp (vvvBinOp v) (expr l) (expr r)
expr (E.VVNBinaryOp v l r)   = VVNBinaryOp (vvnBinOp v) (expr l) (expr r)
expr (E.NVVBinaryOp v l r)   = NVVBinaryOp (nvvBinOp v) (expr l) (expr r)
expr (E.ESSBinaryOp o l r)   = ESSBinaryOp (essBinOp o) (expr l) (expr r)
expr (E.ESBBinaryOp o l r)   = ESBBinaryOp (esbBinOp o) (expr l) (expr r)
expr (E.Operator ao dd e)    = Operator (assocArithOper ao) (domainDesc dd) (expr e)
expr (E.RealI u ri)          = RealI u (realInterval ri)

realInterval :: RealInterval E.Expr E.Expr -> RealInterval ModelExpr ModelExpr
realInterval (Bounded (li, l) (ri, r)) = Bounded (li, expr l) (ri, expr r)
realInterval (UpTo (i, e))             = UpTo (i, expr e)
realInterval (UpFrom (i, e))           = UpFrom (i, expr e)

domainDesc :: DiscreteDomainDesc E.Expr E.Expr -> DiscreteDomainDesc ModelExpr ModelExpr
domainDesc (BoundedDD s rt l r) = BoundedDD s rt (expr l) (expr r)
-- domainDesc (AllDD s rt) = AllDD s rt
