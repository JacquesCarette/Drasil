{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.CodeExpr (new, newWithNamedArgs, message,
  msgWithNamedArgs, field) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeIdea, CodeVarChunk)

import Control.Lens ((^.))

-- | Constructs an Expr for actor creation (constructor call)
new :: (Callable f, HasUID f, CodeIdea f) => f -> [Expr] -> Expr
new c ps = New (c ^. uid) ps []

-- | Constructs an Expr for actor creation (constructor call) that uses named arguments
newWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID a, 
  IsArgumentName a) => f -> [Expr] -> [(a, Expr)] -> Expr
newWithNamedArgs c ps ns = New (c ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

-- | Constructs an Expr for actor messaging (method call)
message :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, CodeIdea c) 
  => c -> f -> [Expr] -> Expr
message o m ps = checkObj (o ^. typ)
  where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps []
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "Actor space"

-- | Constructs an Expr for actor messaging (method call) that uses named arguments
msgWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, 
  CodeIdea c, HasUID a, IsArgumentName a) => c -> f -> [Expr] -> [(a, Expr)] -> 
  Expr
msgWithNamedArgs o m ps as = checkObj (o ^. typ)
  where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps 
          (zip (map ((^. uid) . fst) as) (map snd as))
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "Actor space"

-- | Constructs an Expr representing the field of an actor
field :: CodeVarChunk -> CodeVarChunk -> Expr
field o f = checkObj (o ^. typ)
  where checkObj (Actor _) = Field (o ^. uid) (f ^. uid)
        checkObj _ = error $ "Invalid actor field: Actor should have " ++
          "Actor space"
