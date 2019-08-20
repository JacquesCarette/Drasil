-- | Helper functions for printing
module Language.Drasil.Printing.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (text, Doc, (<>))
import Data.Char (toUpper, toLower)

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

dot, comm :: Doc -> Doc
dot  = (<> text ".")
comm = (<> text ",")

-- For wrapping $ on both sides of a Doc
dollarDoc :: Doc -> Doc
dollarDoc x = dlr <> x <> dlr

-- | basic plaintext (String) wrapping
paren, brace, dollar, sqbrac, angbrac :: String -> String
paren   x = "(" ++ x ++ ")"
brace   x = "{" ++ x ++ "}"
dollar  x = "$" ++ x ++ "$"
sqbrac  x = "[" ++ x ++ "]"
angbrac x = "<" ++ x ++ ">"

-- | Format strings and convert to Doc
upcase, lowcase :: String -> Doc
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
sufxer = (++ ".") . sufx . mod 10
