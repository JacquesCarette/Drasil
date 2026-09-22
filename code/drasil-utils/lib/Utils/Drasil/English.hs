-- | Useful functions for working with English-related 'String's.
module Utils.Drasil.English (capitalize, foldlList, stringList) where

import Data.Char (toLower, toUpper)
import Data.List.Extras (foldle1)
import Data.String (IsString (fromString))

-- | String capitalization.
capitalize :: String -> String
capitalize [] = error "capitalize called on an empty String"
capitalize (c:cs) = toUpper c:map toLower cs

-- | Organize a list of Strings (or other 'IsString'/'Semigroup' types),
-- separated by commas and inserting "and" before the last item.
foldlList :: (IsString a, Semigroup a) => [a] -> a
foldlList []    = fromString ""
foldlList [a,b] = a <> fromString " and " <> b
foldlList lst   = foldle1 (\a b -> a <> fromString ", " <> b) (\a b -> a <> fromString ", and " <> b) lst

-- | Comma separated list with "and" before final item, omitting empty strings.
stringList :: [String] -> String
stringList = foldlList . filter (not . null)
