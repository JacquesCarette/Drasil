{-
This is a quick and dirty test to see if EqnBlocks are
printing correctly
-}

import System.IO
import System.Directory
import System.Environment
import Data.List
import Data.Char
import qualified Example.Drasil.SWHS.Generate as G

readContents :: [String] -> Bool
readContents (s:e:ss) = if (isInfixOf "begin{equation}" s) 
                        then (isInfixOf "$" e) 
                        else readContents (e:ss)
readContents _ = False

main = do        
    G.generate
    handle <- openFile "SRS/SWHS_SRS.tex" ReadMode
    contents <- hGetContents handle
    let retval = readContents (lines contents) in
      putStrLn (if retval then "EqnBlock printing failed ($ exists)" else "$ is gone!")
    hClose handle  
