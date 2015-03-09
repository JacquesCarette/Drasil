module Chunk where

import Text.PrettyPrint
import Data.Maybe
import qualified Data.Map.Strict as Map

type Chunk = Map.Map
type FName = String
type FDesc = Doc

get :: FName -> Chunk FName FDesc -> FDesc
get name chunk = fromMaybe empty (Map.lookup name chunk)

newChunk l = Map.fromList l

getWFormat _ [] _ _ = [empty]
getWFormat chunk (x:xs) between after = 
  [(text x <+> between <+> get x chunk <> after)] ++ 
  (getWFormat chunk xs between after)