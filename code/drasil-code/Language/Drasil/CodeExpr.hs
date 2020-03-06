{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.CodeExpr (new, message) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeIdea)

import Control.Lens ((^.))

new :: (Callable f, HasUID f, CodeIdea f) => f -> [Expr] -> Expr
new c = New (c ^. uid)

message :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, CodeIdea c) 
  => c -> f -> [Expr] -> Expr
message o m ps = checkObj (o ^. typ)
  where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "Actor space"
