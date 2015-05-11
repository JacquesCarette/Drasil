module Chunk where

import Text.PrettyPrint
import Data.Maybe
import qualified Data.Map.Strict as Map

import Config
import Helpers

type Chunk = Map.Map
type FName = Field
type FDesc = String
type Dependency = [Chunk FName FDesc]

get :: FName -> Chunk FName FDesc -> Doc
get name chunk = text $ getStr name chunk

getStr name chunk = (fromMaybe "" (Map.lookup name chunk))

newChunk l = Map.fromList l

getWFormat :: [Chunk FName FDesc] -> (FName,FName) -> Doc -> Doc -> [Doc]
getWFormat [] _ _ _ = [empty]
getWFormat (c:cs) (x,y) between after = 
  [(get x c <+> between <+> get y c <> after)] ++
  (getWFormat cs (x,y) between after)
  
writeDep :: [FName] -> Dependency -> String -> String -> [Doc]
writeDep [] _ _ _ = [empty]
writeDep _ [] _ _ = [empty]
writeDep (x:[]) (c:[]) _ es = [get x c <+> text es]
writeDep (x:[]) (c:cs) is es = [get x c <+> text es]
writeDep (x:xs) (c:[]) is es = [get x c <+> text is] ++ writeDep xs [c] is es
writeDep (x:xs) (c:cs) is es = writeDep (x:xs) (c:[]) is es ++ writeDep (x:xs) cs is es


