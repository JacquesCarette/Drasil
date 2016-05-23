{-# OPTIONS -Wall #-} 
module Language.Drasil.Printing.Helpers where

import Text.PrettyPrint
import Data.Char

--basic docs
bslash,dbs,eq,dlr,ast,pls,hat,slash,hyph :: Doc
bslash = text "\\"
dbs    = bslash <> bslash
eq     = text "="
dlr    = text "$"
ast    = text "*"
pls    = text "+"
hat    = text "^"
slash  = text "/"
hyph   = text "-"

sq,br :: String -> Doc
sq t = text $ "[" ++ t ++ "]"
br t = text $ "{" ++ t ++ "}"

--basic plaintext manipulation
paren,brace,dollar,quotes,sqbrac :: String -> String
paren  = \x -> "(" ++ x ++ ")"
brace  = \x -> "{" ++ x ++ "}"
dollar = \x -> "$" ++ x ++ "$"
quotes = \x -> "\"" ++ x ++ "\""
sqbrac = \x -> "[" ++ x ++ "]"

--capitalize
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c:map toLower cs

--format strings and convert -> Doc
upcase, lowcase :: [Char] -> Doc
upcase []      = text []
upcase (c:cs)  = text $ toUpper c:cs --capitalize first letter of string
lowcase []     = text []
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase
