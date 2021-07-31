module Language.Drasi.ModelExpr.Convert where

import Language.Drasil.Space (DomainDesc(..), RealInterval(..))
import Language.Drasil.UID (UID)
import qualified Language.Drasil.Expr as E
import Language.Drasil.ModelExpr (ModelExpr(..))

expr :: E.Expr -> ModelExpr
expr (E.Dbl d) = Dbl d
expr (E.Int i) = Int i
expr (E.ExactDbl i) = ExactDbl i
expr (E.Str s) = Str s --      :: String -> Expr
expr (E.Perc n d) = Perc n d --     :: Integer -> Integer -> Expr
expr (E.AssocA ao es) = AssocA ao $ map expr es --   :: AssocArithOper -> [Expr] -> Expr
expr (E.AssocB bo es) = _ --   :: AssocBoolOper  -> [Expr] -> Expr
expr (E.Deriv dt e u) = Deriv dt (expr e) u
expr (E.C u) = C u
expr (E.FCall u es nes) = _ --    :: UID -> [Expr] -> [(UID, Expr)] -> Expr
expr (E.Case c ces) = _ --     :: Completeness -> [(Expr,Relation)] -> Expr
expr (E.Matrix es) = _ --   :: [[Expr]] -> Expr
expr (E.UnaryOp u e) = _ --       :: UFunc -> Expr -> Expr
expr (E.UnaryOpB u e) = _ --      :: UFuncB -> Expr -> Expr
expr (E.UnaryOpVV u e) = _ --     :: UFuncVV -> Expr -> Expr
expr (E.UnaryOpVN u e) = _ --     :: UFuncVN -> Expr -> Expr
expr (E.ArithBinaryOp a l r) = _ -- :: ArithBinOp -> Expr -> Expr -> Expr
expr (E.BoolBinaryOp b l r) = _ --  :: BoolBinOp -> Expr -> Expr -> Expr
expr (E.EqBinaryOp e l r) = _ --    :: EqBinOp -> Expr -> Expr -> Expr
expr (E.LABinaryOp la l r) = _ --    :: LABinOp -> Expr -> Expr -> Expr
expr (E.OrdBinaryOp o l r) = _ --   :: OrdBinOp -> Expr -> Expr -> Expr
expr (E.VVVBinaryOp v l r) = _ --   :: VVVBinOp -> Expr -> Expr -> Expr
expr (E.VVNBinaryOp v l r) = _ --   :: VVNBinOp -> Expr -> Expr -> Expr
expr (E.Operator ao dd e) = _ -- :: AssocArithOper -> DomainDesc Expr Expr -> Expr -> Expr
expr (E.RealI u ri) = RealI u (realInterval ri) --    :: UID -> RealInterval Expr Expr -> Expr

realInterval :: RealInterval E.Expr E.Expr -> RealInterval ModelExpr ModelExpr
realInterval (Bounded (li, l) (ri, r)) = Bounded (li, expr l) (ri, expr r)
realInterval (UpTo (i, e)) = UpTo (i, expr e)
realInterval (UpFrom (i, e)) = UpFrom (i, expr e)

domainDesc :: DomainDesc E.Expr E.Expr -> DomainDesc ModelExpr ModelExpr
domainDesc dd = _
