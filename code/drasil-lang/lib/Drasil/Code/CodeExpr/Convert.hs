{-# LANGUAGE GADTs #-}
module Drasil.Code.CodeExpr.Convert (
    expr, realInterval, constraint,
    CanGenCode(..)
) where

import Language.Drasil.Space (RealInterval(..), DiscreteDomainDesc, DomainDesc(BoundedDD))
import Language.Drasil.Constraint (Constraint(..), ConstraintE)
import qualified Language.Drasil.Expr.Lang as E
import qualified Language.Drasil.Expr.Development as LD
import qualified Language.Drasil.Literal.Development as LL

import Drasil.Code.CodeExpr.Lang

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
expr (LD.AssocA ao es)         = AssocA ao $ map expr es
expr (LD.AssocB bo es)         = AssocB bo $ map expr es
expr (LD.AssocC bo es)         = AssocC bo $ map expr es
expr (LD.C u)                  = C u
expr (LD.FCall u es)           = FCall u (map expr es) []
expr (LD.Case c es)            = Case c $ map (bimap expr expr) es
expr (LD.Matrix es)            = Matrix $ map (map expr) es
expr (LD.Set e es)             = Set e $ map expr es
expr (E.Variable s e)          = Variable s $ expr e
expr (LD.UnaryOp uo e)         = UnaryOp uo (expr e)
expr (LD.UnaryOpB uo e)        = UnaryOpB uo (expr e)
expr (LD.UnaryOpVV uo e)       = UnaryOpVV uo (expr e)
expr (LD.UnaryOpVN uo e)       = UnaryOpVN uo (expr e)
expr (LD.ArithBinaryOp bo l r) = ArithBinaryOp bo (expr l) (expr r)
expr (LD.EqBinaryOp bo l r)    = EqBinaryOp bo (expr l) (expr r)
expr (LD.LABinaryOp bo l r)    = LABinaryOp bo (expr l) (expr r)
expr (LD.OrdBinaryOp bo l r)   = OrdBinaryOp bo (expr l) (expr r)
expr (LD.VVVBinaryOp bo l r)   = VVVBinaryOp bo (expr l) (expr r)
expr (LD.VVNBinaryOp bo l r)   = VVNBinaryOp bo (expr l) (expr r)
expr (LD.NVVBinaryOp bo l r)   = NVVBinaryOp bo (expr l) (expr r)
expr (LD.ESSBinaryOp bo l r)   = ESSBinaryOp bo (expr l) (expr r)
expr (LD.ESBBinaryOp bo l r)   = ESBBinaryOp bo (expr l) (expr r)
expr (LD.Operator aao dd e)    = Operator aao (renderDomainDesc dd) (expr e)
expr (LD.RealI u ri)           = RealI u (realInterval ri)

-- | Convert 'RealInterval' 'Expr' 'Expr's into 'RealInterval' 'CodeExpr' 'CodeExpr's.
realInterval :: RealInterval E.Expr E.Expr -> RealInterval CodeExpr CodeExpr
realInterval (Bounded (il, el) (ir, er)) = Bounded (il, expr el) (ir, expr er)
realInterval (UpTo (i, e))               = UpTo (i, expr e)
realInterval (UpFrom (i, e))             = UpFrom (i, expr e)

-- | Convert constrained expressions ('ConstraintE') into 'Constraint''CodeExpr's.
constraint :: ConstraintE -> Constraint CodeExpr
constraint (Range r ri) = Range r (realInterval ri)
constraint (Elem r ri) = Elem r (expr ri)

-- | Convert 'DomainDesc Expr Expr' into 'DomainDesc CodeExpr CodeExpr's.
renderDomainDesc :: DiscreteDomainDesc E.Expr E.Expr -> DiscreteDomainDesc CodeExpr CodeExpr
renderDomainDesc (BoundedDD s t l r) = BoundedDD s t (expr l) (expr r)
