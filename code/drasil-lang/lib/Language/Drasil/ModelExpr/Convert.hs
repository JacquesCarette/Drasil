{-# LANGUAGE GADTs #-}
-- | Defines functions to convert from the base expression language to 'ModelExpr's.
module Language.Drasil.ModelExpr.Convert where

import Data.Bifunctor (bimap)

import Language.Drasil.Space
    (RealInterval(..), DiscreteDomainDesc, DomainDesc(BoundedDD))
import qualified Language.Drasil.Expr.Lang as E
import Language.Drasil.ModelExpr.Lang

assocBoolOper :: E.AssocBoolOper -> AssocBoolOper
assocBoolOper E.And = And
assocBoolOper E.Or  = Or

expr :: E.Expr -> ModelExpr
expr (E.Lit a)               = Lit a
expr (E.AssocA ao es)        = AssocA ao $ map expr es
expr (E.AssocB bo es)        = AssocB (assocBoolOper bo) $ map expr es
expr (E.AssocC ao es)        = AssocC ao $ map expr es
expr (E.C u)                 = C u
expr (E.FCall u es)          = FCall u (map expr es)
expr (E.Case c ces)          = Case c (map (bimap expr expr) ces)
expr (E.Matrix es)           = Matrix $ map (map expr) es
expr (E.Set s e)             = Set s $ map expr e
expr (E.Variable s e)        = Variable s $ expr e
expr (E.UnaryOp u e)         = UnaryOp u (expr e)
expr (E.UnaryOpB u e)        = UnaryOpB u (expr e)
expr (E.UnaryOpVV u e)       = UnaryOpVV u (expr e)
expr (E.UnaryOpVN u e)       = UnaryOpVN u (expr e)
expr (E.ArithBinaryOp a l r) = ArithBinaryOp a (expr l) (expr r)
expr (E.BoolBinaryOp b l r)  = BoolBinaryOp b (expr l) (expr r)
expr (E.EqBinaryOp e l r)    = EqBinaryOp e (expr l) (expr r)
expr (E.LABinaryOp la l r)   = LABinaryOp la (expr l) (expr r)
expr (E.OrdBinaryOp o l r)   = OrdBinaryOp o (expr l) (expr r)
expr (E.VVVBinaryOp v l r)   = VVVBinaryOp v (expr l) (expr r)
expr (E.VVNBinaryOp v l r)   = VVNBinaryOp v (expr l) (expr r)
expr (E.NVVBinaryOp v l r)   = NVVBinaryOp v (expr l) (expr r)
expr (E.ESSBinaryOp o l r)   = ESSBinaryOp o (expr l) (expr r)
expr (E.ESBBinaryOp o l r)   = ESBBinaryOp o (expr l) (expr r)
expr (E.Operator ao dd e)    = Operator ao (domainDesc dd) (expr e)
expr (E.RealI u ri)          = RealI u (realInterval ri)

realInterval :: RealInterval E.Expr E.Expr -> RealInterval ModelExpr ModelExpr
realInterval (Bounded (li, l) (ri, r)) = Bounded (li, expr l) (ri, expr r)
realInterval (UpTo (i, e))             = UpTo (i, expr e)
realInterval (UpFrom (i, e))           = UpFrom (i, expr e)

domainDesc :: DiscreteDomainDesc E.Expr E.Expr -> DiscreteDomainDesc ModelExpr ModelExpr
domainDesc (BoundedDD s rt l r) = BoundedDD s rt (expr l) (expr r)
-- domainDesc (AllDD s rt) = AllDD s rt
