{-# OPTIONS -Wall #-} 
module Chunk where
import qualified Data.Map.Strict as Map
import qualified ASTInternal as AST

newChunk :: [(AST.FName, AST.FDesc)] -> Map.Map AST.FName AST.FDesc
newChunk l = Map.fromList l