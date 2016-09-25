module GOOL.Auxil.Helper (
    capitalize,containsAll,makeLiteralNameValid,makeVarNameValid,powerSet,
    hmap,himap,vicat,vibcat,vmap,vimap,vibmap
) where

import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.String.Utils (replace)
import Data.List (elem,nub,intersperse)
import Text.PrettyPrint.HughesPJ (Doc,hcat,text,vcat)

capitalize :: String -> String
capitalize (c:cs) = (toUpper c):cs

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll l = and . map (\x -> x `elem` l)

makeLiteralNameValid :: String -> String
makeLiteralNameValid s =
    let chars = ["\\","\""]
    in foldl myLiteralNameReplace s chars

makeVarNameValid :: String -> String
makeVarNameValid s =
    let illegalChars = [
            "~","`","-","=","!","@","#","$","%","^","&","*","(",")","+",
            "[","]","\\",";","'",".","/","{","}","|",":","\"","<",">","?"," "]
    in foldl myVarNameReplace s illegalChars

--http://community.livejournal.com/evan_tech/220036.html
powerSet :: Eq a => [a] -> [[a]]
powerSet = filterM (const [True, False]) . nub

hmap :: (a -> Doc) -> [a] -> Doc
hmap f l = hcat $ map f l

himap :: Doc -> (a -> Doc) -> [a] -> Doc
himap c f l = hcat $ intersperse c (map f l)

vicat :: Doc -> [Doc] -> Doc
vicat c l = vcat $ intersperse c l

vibcat :: [Doc] -> Doc
vibcat = vicat blank

vmap :: (a -> Doc) -> [a] -> Doc
vmap f l = vcat $ map f l

vimap :: Doc -> (a -> Doc) -> [a] -> Doc
vimap c f l = vicat c (map f l)

vibmap :: (a -> Doc) -> [a] -> Doc
vibmap = vimap blank

--private
blank :: Doc
blank = text ""

myLiteralNameReplace :: String -> String -> String
myLiteralNameReplace l old = replace old ("\\" ++ old) l

myVarNameReplace :: String -> String -> String
myVarNameReplace l old = replace old "_" l
----------
