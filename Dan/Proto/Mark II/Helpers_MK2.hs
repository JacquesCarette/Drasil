{-# OPTIONS -Wall #-} 
module Helpers_MK2 where

import Text.PrettyPrint
import Data.Char

--basic
bslash,dbs,eq,dlr,ast,pls :: Doc
bslash = text "\\"
dbs = bslash <> bslash
eq = text "="
dlr = text "$"
ast = text "*"
pls = text "+"

sq,br :: String -> Doc
sq t = text $ "[" ++ t ++ "]"
br t = text $ "{" ++ t ++ "}"

paren :: String -> String
paren t = "(" ++ t ++ ")"

--format strings
upcase, lowcase :: [Char] -> Doc
upcase [] = text []
upcase (c:cs) = text $ toUpper c:cs --capitalize first letter of string
lowcase [] = text []
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase