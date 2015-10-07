{-# OPTIONS -Wall #-} 
module Chunk where

import qualified Data.Map.Strict as Map
import qualified ASTInternal as AST
import Data.Maybe (fromMaybe)

newChunk :: [(AST.FName, AST.FDesc)] -> Map.Map AST.FName AST.FDesc
newChunk = Map.fromList

find :: AST.FName -> AST.Chunk -> String -> AST.FDesc
find f chunk errmsg = 
  fromMaybe (error errmsg) (Map.lookup f chunk)