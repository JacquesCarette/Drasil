module Language.Drasil.CCode.Import where

import Control.Lens

import Language.Drasil.CCode.AST
import qualified Language.Drasil.Expr as E
import Language.Drasil.Expr.Extract (dep)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk (name)

-- for testing
--toCode :: Lang -> CodeType -> EqChunk -> Code
--toCode CLang Calc ec = C [StdLibHeader, StdIOHeader] [VarDecl IntType "gVar", VarDecl DblType "gVar2"] [(MethodDecl DblType "test" [VarDecl DblType "a", VarDecl DblType "b"], [Declare DblType "f" (Just $ Dbl 20), Assign "a" (Int $ -1), If (Leq (Var "a") (Var "b")) [(Return (Int 1))] (Just [(Return (Int 0))]), Return (Var "a")])]

toCode :: Lang -> CodeType -> [EqChunk] -> Code
-- Name should be the name of what you are calculating. For example h_g
toCode CLang Calc ecs = C [] [] (map makeMethod ecs)
-- toCode _ _ _ = error "Unimplemented code translation in ToCode.hs"

makeMethod :: EqChunk -> Method
makeMethod ec = ((MethodDecl DblType ("calc_"++(ec ^. name)) (makeArgs $ dep (equat ec))),
                  (makeDivZeroGuards $ getDenoms $ equat ec) ++ [Return (makeCode $ equat ec)])


makeArgs :: [String] -> [VarDecl]
makeArgs = map (VarDecl DblType)


--makeArgs :: [String] -> [ArgsDecl]
--makeArgs = map (ArgsDecl DblType)

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

makeDivZeroGuards :: [E.Expr] -> [Statement]
makeDivZeroGuards []   = []
makeDivZeroGuards (e:es) = (If (Eq (makeCode e) (Int 0)) [Print "error"] Nothing):makeDivZeroGuards es

getDenoms :: E.Expr -> [E.Expr]
getDenoms (b E.:/ e) = e : (getDenoms b ++ getDenoms e)
getDenoms (b E.:^ e) = getDenoms b ++ getDenoms e
getDenoms (b E.:* e) = getDenoms b ++ getDenoms e
getDenoms (b E.:+ e) = getDenoms b ++ getDenoms e
getDenoms (b E.:- e) = getDenoms b ++ getDenoms e
getDenoms _ = []
