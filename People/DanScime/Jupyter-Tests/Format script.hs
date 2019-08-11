
f :: String -> String
f ('"':xs) = '\\' : '"' : f xs
f (x:xs) = x: f xs
f [] = []


main :: IO ()
main = do
  temp <- readFile "GamePhysics HTML"
  let s = concat $ lines temp
  print s
  writeFile "NEW" (f s)
    