module Main where

import Drasil.SWHS.Generate (generate)
import Drasil.SWHS.Body

main :: IO ()
main = do
	generate
	mapM_ print outputuid
