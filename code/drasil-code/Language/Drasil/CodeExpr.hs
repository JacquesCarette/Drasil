module Language.Drasil.CodeExpr (new, message) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeChunk(..), VarOrFunc(..), codeType)

import GOOL.Drasil (CodeType(Object))

import Control.Lens ((^.))

new :: CodeChunk -> [Expr] -> Expr
new c ps = checkFunc (kind c)
  where checkFunc Var = error $ "Attempt to create new actor but passed " ++
          "CodeChunk is not callable"
        checkFunc Func = New (c ^. uid) ps

message :: CodeChunk -> CodeChunk -> [Expr] -> Expr
message o m ps = checkKinds (kind o) (kind m) 
  where checkKinds Var Func = checkObj (codeType o) 
        checkKinds _ _ = error $ "Invalid actor message: message should be " ++
          "passed a Var and a Func"
        checkObj (Object _) = Message (o ^. uid) (m ^. uid) ps
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "object type"