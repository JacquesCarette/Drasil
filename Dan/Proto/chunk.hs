{-# OPTIONS -Wall #-} 
module Chunk where

import Text.PrettyPrint

import qualified Data.Map.Strict as Map

import qualified ASTInternal as AST
import ToTex

newChunk :: [(AST.FName, AST.FDesc)] -> Map.Map AST.FName AST.FDesc
newChunk l = Map.fromList l

getWFormat :: [AST.Chunk AST.FName AST.FDesc] -> (AST.FName,AST.FName) -> Doc ->
                Doc -> [Doc]
getWFormat [] _ _ _ = [empty]
getWFormat (c:cs) (x,y) between after = 
  [(get x c AST.Pg <+> between <+> get y c AST.Pg <> after)] ++
  (getWFormat cs (x,y) between after)
  
writeDep :: [AST.FName] -> AST.Dependency -> String -> String -> AST.Context -> [Doc]
writeDep [] _ _ _ _ = [empty]
writeDep _ [] _ _ _ = [empty]
writeDep (x:[]) (c:[]) _ es con = 
  [get x c con <+> text es]
writeDep (x:[]) (c:_) _ es con= 
  [get x c con <+> text es]
writeDep (x:xs) (c:[]) is es con= 
  [get x c con <+> text is] ++ writeDep xs [c] is es con
writeDep (x:xs) (c:cs) is es con= 
  writeDep (x:xs) (c:[]) is es con ++ writeDep (x:xs) cs is es con