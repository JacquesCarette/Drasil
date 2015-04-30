module Chunk where

import Text.PrettyPrint
import Data.Maybe
import qualified Data.Map.Strict as Map

type Chunk = Map.Map
type FName = String
type FDesc = Doc
type Dependency = [Chunk FName FDesc]

get :: FName -> Chunk FName FDesc -> FDesc
get name chunk = fromMaybe empty (Map.lookup name chunk)

newChunk l = Map.fromList l

getWFormat _ [] _ _ = [empty]
getWFormat chunk (x:xs) between after = 
  [(text x <+> between <+> get x chunk <> after)] ++ 
  (getWFormat chunk xs between after)

-- writeDep :: FName -> Dependency -> String -> String -> [Doc]  
-- writeDep _ [] _ _ = [empty]
-- writeDep x (c:[]) _ es = [get x c]
-- writeDep x (c:cs) is es = [get x c <+> text es] ++ writeDep x cs is es
  
  --Infinite Loop 
writeDep :: [FName] -> Dependency -> String -> String -> [Doc]
writeDep [] _ _ _ = [empty]
writeDep _ [] _ _ = [empty]
writeDep (x:[]) (c:[]) _ es = [get x c <+> text es]
writeDep (x:[]) (c:cs) is es = [get x c <+> text es]
writeDep (x:xs) (c:[]) is es = [get x c <+> text is] ++ writeDep xs [c] is es
writeDep (x:xs) (c:cs) is es = writeDep (x:xs) (c:[]) is es ++ writeDep (x:xs) cs is es
writeDep _ _ _ _ = [empty]

-- writeNext :: FName -> Dependency -> String -> String -> [Doc]
-- writeNext _ [] _ _ = [empty]
-- writeNext x (c:cs) is es = [get x c <+> text es] ++ writeNext x cs is es
  
 