module Language.Drasil.Code.Imperative.Helpers (
  liftS, mkVal, mkVar, getUpperBound, lookupC
) where

liftS :: Reader a b -> Reader a [b]
liftS = fmap (: [])

mkVal :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Value repr))
mkVal v = value (v ^. uid) (codeName v) (convType $ codeType v)

mkVar :: (RenderSym repr, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Variable repr))
mkVar v = variable (codeName v) (convType $ codeType v)

getUpperBound :: Expr -> Expr
getUpperBound (BinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

lookupC :: State -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ csi $ codeSpec g)