-- Need > cabal install split
import Data.List
import Data.List.Split

toLink :: String -> String
toLink l = "<a href=#" ++ l1 ++ ">" ++ l ++ "</a>"
  where l1 = (\y -> head y ++ ":" ++ last y) (splitOn ": " l)

leftAlign :: String -> String
leftAlign = (":" ++)

main :: IO ()
main = do
  s <- readFile "input.txt"
  let s1 = lines s
  let s2 = map (splitOn "|") s1
  let s3 = (map toLink $ head s2) : (map leftAlign $ head $ tail s2) :
         (map (\x -> ("|" ++) (toLink (head (tail x))) : tail (tail x))) (tail $ tail s2)


  writeFile "output.txt" (intercalate "\n" $ map (intercalate "|") s3)
