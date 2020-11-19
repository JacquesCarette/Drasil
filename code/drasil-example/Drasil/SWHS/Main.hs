module Main where

import GHC.IO.Encoding

import Drasil.SWHS.Generate (generate)

main :: IO ()
main = 
  do
    setLocaleEncoding utf8
    generate
