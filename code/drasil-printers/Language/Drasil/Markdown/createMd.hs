module Language.Drasil.Markdown.CreateMd (
    makeMd, sumInfo, invalidOS, regularSec, contSep, filtEmp) 
    where

import Data.List (intersperse, filter)
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty, vcat, text, (<>))

type Seperator = Doc


makeMd :: [Doc] -> Doc
makeMd = vcat . intersperse secSep . filtEmp 

sumInfo :: String -> String -> String -> Doc
sumInfo name pl plv = summarySec (text $
   "Summary of Key Information for Running " ++ name ++ " on " ++ pl) 
    (text $ pl ++ " Version: " ++ plv)

invalidOS :: Maybe String -> Doc
invalidOS Nothing = empty
invalidOS (Just unsuppOS) = regularSec (text "Unsupported OSs")
    (text $ "- " ++ unsuppOS)

secSep, contSep :: Seperator
secSep = text "\n------------------------------------------------------------"
contSep = text "\n"

-- Functions to construct section from header and message
summarySec :: Doc -> Doc -> Doc
summarySec hd ms = text "###" <> hd <> contSep <> ms

regularSec :: Doc -> Doc -> Doc
regularSec hd ms = text "**" <> hd <> text ":**" <> contSep <> ms

-- Helper for makeMd and dependencies
filtEmp :: [Doc] -> [Doc]
filtEmp = filter (not . isEmpty) 