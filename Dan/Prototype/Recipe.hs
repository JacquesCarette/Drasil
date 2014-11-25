-------------------------------------------------------------------------------
-- Recipe
-------------------------------------------------------------------------------
import Chunks

data Recipe = Recipe [String] [Chunk]
  deriving (Show, Eq, Read)

checkDupes (x:xs) r = if (hasRef x r)
                  then error ("A chunk with that " ++ refHeader ++ " field already exists")
                  else checkDupes xs r
checkDupes [] _ = False

makeChunkList :: [Chunk] -> [Chunk] -> [Chunk]
makeChunkList cl (c@(Chunk r _):cs) = if (not (checkDupes cl r) && validChunk c) 
                                      then makeChunkList (cl ++ [c]) cs
                                      else error "Duplicate Chunk Reference"
makeChunkList cl [] = cl
                                      
main = do
  let x = makeChunk [("Name","My Name Is X"),("Data","I am a Variable")] in
    if (validChunk x)
      then print "Hi"
      else print "bye"