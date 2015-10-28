{-# OPTIONS -Wall #-} 
module Chunk where

import qualified Data.Map.Strict as Map
import qualified ASTInternal as AST
import Data.Maybe (fromMaybe)

newChunk :: String -> [(AST.FName, AST.FDesc a)] -> AST.Chunk a
newChunk nm l = AST.Chunk nm (Map.fromList l)

find :: AST.FName -> AST.Chunk a -> String -> AST.FDesc a
find f@AST.Equation (AST.Chunk _ chunk) errmsg = 
  fromMaybe (fromMaybe (error errmsg) (Map.lookup AST.Symbol chunk)) (Map.lookup f chunk)
find f (AST.Chunk _ chunk) errmsg = 
  fromMaybe (error errmsg) (Map.lookup f chunk)

findOptional :: AST.FName -> AST.Chunk a -> Maybe (AST.FDesc a)
findOptional f (AST.Chunk _ m) = Map.lookup f m
