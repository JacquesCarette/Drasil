module Language.Drasil.Code.Import where

import Prelude hiding (return, id)
import Control.Lens hiding (makeFields, assign)

import qualified Language.Drasil.Expr as E
import Language.Drasil.Chunk.Method
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Eq
import Language.Drasil.Expr.Extract
import Language.Drasil.Code.Imperative.AST as A
import Language.Drasil.Code.Imperative.Helpers
import Language.Drasil.Space as S
import Language.Drasil.Chunk as C
import Language.Drasil.Chunk.Quantity as Q

toCode :: NamedIdea c => c -> [ModuleChunk] -> AbstractCode
toCode prog mcs = AbsCode $ Pack (prog ^. id) (makeModules mcs)

makeModules :: [ModuleChunk] -> [Class]
makeModules [] = []
makeModules (mc:mcs) = makeModule mc : makeModules mcs

makeModule :: ModuleChunk -> Class
makeModule mc = pubClass (makeClassNameValid $ (modcc mc) ^. id) noParent
  (makeFields (field mc)) (makeMethods (method mc))

makeFields :: [VarChunk] -> [StateVar]
makeFields = map makeField

makeField :: VarChunk -> StateVar
makeField vc = pubMVar 2 (makeType (vc ^. Q.typ)) (vc ^. id)

makeMethods :: [MethodChunk] -> [Method]
makeMethods = map makeMethod

makeMethod :: MethodChunk -> Method
makeMethod meth@(MeC { mType = MCalc (EC a b)}) =
  pubMethod (A.typ $ makeType $ a ^. Q.typ) ("calc_" ++ ((methcc meth) ^. id))
  (map (\vc -> param (vc ^. id) (makeType (vc ^. Q.typ))) (vars b))
  (oneLiner $ return $ makeExpr b)
makeMethod      (MeC { mType = MInput IOStd vc}) =
  pubMethod (A.typ $ makeType $ vc ^. Q.typ) ("in_" ++ (vc ^. id)) []
  [ Block [ varDec (vc ^. id) (makeType $ vc ^. Q.typ),
            assign (Var (vc ^. id)) (Input)
          ]
  ]


makeType :: Space -> StateType
makeType S.Rational = float
makeType S.Boolean  = bool
makeType S.Integer  = int
makeType S.Char     = char
makeType S.String   = string
makeType (S.Obj s)  = Type s

makeExpr :: E.Expr -> Value
makeExpr (E.V v)    = Var v
makeExpr (E.Dbl d)  = litFloat d
makeExpr (E.Int i)  = litInt i
makeExpr (E.C c)    = Var (c ^. id)
makeExpr (b E.:^ e) = (makeExpr b) #^ (makeExpr e)
makeExpr (b E.:* e) = (makeExpr b) #* (makeExpr e)
makeExpr (b E.:/ e) = (makeExpr b) #/ (makeExpr e)
makeExpr (b E.:+ e) = (makeExpr b) #+ (makeExpr e)
makeExpr (b E.:- e) = (makeExpr b) #- (makeExpr e)
makeExpr _          = error "Unimplemented expression in code generation"
