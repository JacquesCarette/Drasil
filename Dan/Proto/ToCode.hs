module ToCode where

import ASTCode
import qualified ASTInternal as E
import Control.Lens
import ExprTools (get_dep)
import EqChunk
import Chunk (name)

toCode :: Lang -> CodeType -> EqChunk -> Code
-- Name should be the name of what you are calculating. For example h_g
toCode CLang Calc ec = C [
  ((MethodDecl DblType ("calc_"++(ec ^. name)) (makeArgs $ get_dep (equat ec))),
  [Return (makeCode $ equat ec)])]
-- toCode _ _ _ = error "Unimplemented code translation in ToCode.hs"

makeArgs :: [String] -> [ArgsDecl]
makeArgs = map (ArgsDecl DblType)

-- Currently assuming type of double for arguments as all examples involve
  -- calculations using known (or calculated) values.
-- TODO: Add either a toggle or a field in chunks to declare their precision
  
makeCode :: E.Expr -> CodeExpr
makeCode (E.V v)    = Var v
makeCode (E.Dbl d)  = Dbl d
makeCode (E.Int i)  = Int i
makeCode (E.C c)    = Var (c ^. name)
makeCode (b E.:^ e) = Pow (makeCode b) (makeCode e)
makeCode (b E.:* e) = Mult (makeCode b) (makeCode e)
makeCode (b E.:/ e) = Div (makeCode b) (makeCode e)
makeCode (b E.:+ e) = Add (makeCode b) (makeCode e)
makeCode (b E.:- e) = Sub (makeCode b) (makeCode e)
