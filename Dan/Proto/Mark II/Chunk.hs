{-# OPTIONS -Wall #-} 
module Chunk where

import qualified Data.Map.Strict as Map
import qualified ASTInternal as AST
import Data.Maybe (fromMaybe)

newChunk :: [(AST.FName, AST.FDesc)] -> Map.Map AST.FName AST.FDesc
newChunk = Map.fromList

find :: AST.FName -> AST.Chunk -> String -> AST.FDesc
find f@AST.Equation chunk errmsg = 
  fromMaybe (fromMaybe (error errmsg) (Map.lookup AST.Symbol chunk)) (Map.lookup f chunk)
find f chunk errmsg = 
  fromMaybe (error errmsg) (Map.lookup f chunk)

findOptional :: AST.FName -> AST.Chunk -> AST.FDesc
findOptional f chunk = fromMaybe (AST.Empty) (Map.lookup f chunk) 
