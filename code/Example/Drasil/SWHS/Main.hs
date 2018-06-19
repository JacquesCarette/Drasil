module Main where

import Drasil.SWHS.Generate (generate)
import Drasil.SWHS.Body

main :: IO ()
main = do
	mapM_ print outputuid
