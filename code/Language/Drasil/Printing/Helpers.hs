-- | Helper functions for printing
module Language.Drasil.Printing.Helpers where

import Text.PrettyPrint
import Data.Char

-- | Basic text-rendering helper function
bslash,dbs,assign,eq,lt,gt,leq,geq,dlr,ast,pls,hat,slash,hyph,tab,unders :: Doc
bslash = text "\\"
-- | Double backslash
dbs    = text "\\\\"
-- | "="
assign = text "="
-- | "=="
eq     = text "=="
lt     = text "<"
gt     = text ">"
leq    = text "<="
geq    = text ">="
dlr    = text "$"
ast    = text "*"
pls    = text "+"
hat    = text "^"
slash  = text "/"
hyph   = text "-"
tab    = text "\t"
unders = text "_"

-- | Text-rendering helper for wrapping strings with brackets/braces
sq,br :: String -> Doc
sq t = text $ "[" ++ t ++ "]"
br t = text $ "{" ++ t ++ "}"

-- | indent helper
indent :: Doc -> Doc
indent = nest 4

-- | basic plaintext (String) wrapping
paren,brace,dollar,quotes,sqbrac,angbrac,dot,comm :: String -> String
paren  = \x -> "(" ++ x ++ ")"
brace  = \x -> "{" ++ x ++ "}"
dollar = \x -> "$" ++ x ++ "$"
quotes = \x -> "\"" ++ x ++ "\""
sqbrac = \x -> "[" ++ x ++ "]"
angbrac = \x -> "<" ++ x ++ ">"
dot    = \x -> x ++ "."
comm   = \x -> x ++ ","

-- | String capitalization
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c:map toLower cs

-- | Format strings and convert to Doc
upcase, lowcase :: [Char] -> Doc
upcase []      = text []
upcase (c:cs)  = text $ toUpper c:cs --capitalize first letter of string
lowcase []     = text []
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase


--FIXME: move this. It is here for not since TeX and HTML
--       use this for bibliography rendering
-- Used only on single digit Int
sufx :: Int -> String
sufx 1 = "st"
sufx 2 = "nd"
sufx 3 = "rd"
sufx _ = "th"

-- Use on any sized Int
sufxer :: Int -> String
sufxer = dot . sufx . mod 10
