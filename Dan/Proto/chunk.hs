module Chunk where

import Text.PrettyPrint

data Chunk = Chunk [(FName,FDesc)]
type FName = String
type FDesc = Doc

get :: FName -> Chunk -> FDesc
get name (Chunk []) = error "Chunk field not found"            
get name (Chunk (x:xs)) = if (find name x) then getd x else get name (Chunk xs)

find name (n,_) = (name == n)

getd (n,d) = d