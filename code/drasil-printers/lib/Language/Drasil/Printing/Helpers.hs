-- | General helper functions for printing Drasil documents.
module Language.Drasil.Printing.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (text, Doc, (<>))
import Data.Char (toUpper, toLower)
import Language.Drasil.Printing.Citation ( CiteField(HowPublished), HP (..) )

-- | Basic text-rendering helper function.
bslash,dbs,assign,eq,lt,gt,leq,geq,dlr,ast,pls,hat,slash,hyph,tab,unders :: Doc
-- | Single backslash.
bslash = text "\\"
-- | Double backslash.
dbs    = text "\\\\"
-- | Variable assignment character ("=").
assign = text "="
-- | Equality character ("==").
eq     = text "=="
-- | Less than.
lt     = text "<"
-- | Greater than.
gt     = text ">"
-- | Less than or equal to.
leq    = text "<="
-- | Greater than or equal to.
geq    = text ">="
-- | Dollar sign.
dlr    = text "$"
-- | Asterisk.
ast    = text "*"
-- | Plus.
pls    = text "+"
-- | Hat symbol ("^").
hat    = text "^"
-- | Forward slash.
slash  = text "/"
-- | Hyphen.
hyph   = text "-"
-- | Tab.
tab    = text "\t"
-- | Underscore.
unders = text "_"

-- | Text-rendering helper for wrapping strings with brackets/braces.
sq,br :: String -> Doc
-- | Square brackets.
sq t = text $ "[" ++ t ++ "]"
-- | Curly braces.
br t = text $ "{" ++ t ++ "}"

-- | Text-rendering helper for appending a period/decimal point (dot symbol) or a comma.
dot, comm :: Doc -> Doc
-- | Dot symbol (".")
dot  = (<> text ".")
-- | Comma (",")
comm = (<> text ",")

-- | For wrapping $ on both sides of a 'Doc'.
dollarDoc :: Doc -> Doc
dollarDoc x = dlr <> x <> dlr

-- | Basic plaintext (String) wrapping.
paren, brace, dollar, sqbrac, angbrac :: String -> String
-- | Wraps in parenthesis.
paren   x = "(" ++ x ++ ")"
-- | Wraps in curly braces.
brace   x = "{" ++ x ++ "}"
-- | Wraps in dollar signs.
dollar  x = "$" ++ x ++ "$"
-- | Wraps in square brackets.
sqbrac  x = "[" ++ x ++ "]"
-- | Wraps in angular brackets ("<>").
angbrac x = "<" ++ x ++ ">"

-- | Format strings and convert to Doc.
upcase, lowcase :: String -> Doc
-- | Capitalize first letter of string.
upcase []      = text []
upcase (c:cs)  = text $ toUpper c:cs
-- | Make first letter lowercase.
lowcase []     = text []
lowcase (c:cs) = text $ toLower c:cs

--FIXME: move this. It is here for not since TeX and HTML
--       use this for bibliography rendering
-- | Appends a suffix for a number. Used only on single digit 'Int's.
sufx :: Int -> String
sufx 1 = "st"
sufx 2 = "nd"
sufx 3 = "rd"
sufx _ = "th"

-- | Similar to 'sufx' but used on any sized 'Int'.
sufxer :: Int -> String
sufxer x = sufx r ++ "."
    where
        r = if x `elem` [11, 12, 13] then 0 else mod x 10

sufxPrint :: [CiteField] -> String
sufxPrint fields = if any isUrl fields then "" else " Print."
  where
    isUrl (HowPublished (URL _)) = True
    isUrl _ = False
