module Language.Drasil.Markdown.CreateMd (
	makeReadMe, sumInfo, dependencies, invalidOS, makeInstr) 
	where

import Language.Drasil.Markdown.AST
import Drasil.Language.Code (Name, Version, ImplementationType(..))

import Data.List (intersperse, filter)
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty, vcat, text, 
	doubleQuotes, (<+>), (<>))

makeReadMe :: [Section] -> Doc
makeReadMe = vcat . intersperse secsep . filtEmp . map sectiontoDoc

sumSec, dependSec, unsupOS, buildSec :: Header -> Message -> Section
sumSec = Summary
dependSec = Depends 
unsupOS = UnsupOS
buildSec = BuildIns

sectiontoDoc :: Section -> Doc
sectiontoDoc (Intro hd ms) =  text "###" <+> hd <> contSep <> ms
sectiontoDoc Depends = regularSec
sectiontoDoc (UnsupOS hd ms) = regularSec hd ms
sectiontoDoc (BuildIns hd ms) = regularSec hd ms
sectiontoDoc Empty = empty

sumInfo :: String -> String -> String -> Section
sumInfo name pl plv = introSec (text $
   "Summary of Key Information for Running " ++ name ++ " on " ++ pl) 
	(text $ pl ++ " Version: " ++ plv)

dependencies:: [(Name, Version)] -> Doc
dependencies lib = dependSec (text "Program Dependencies") 
	(vcat . intersperse contSep . filtEmp . map libStatment $ lib)

invalidOS :: Maybe String -> Section
invalidOS Nothing = Empty
invalidOS (Just unsuppOS) = unsupOS (text "Unsupported OSs")
    (text $ "- " ++ unsuppOS)

makeInstr :: ImplementationType -> [Doc]
makeInstr Library = Empty
makeInstr Program = buildSec (text "How to Run Program") 
    (text $ "Enter in the following line into your terminal command line: "
    ++ "\n`make run RUNARGS=" ++ "input.txt" ++ "`")

regularSec :: Header -> Message -> Doc
regularSec hd ms = text "**" <> text hd <> text ":**" <> contSep <> ms

contSep
secSep, contSep :: Seperator
secsep = text "\n------------------------------------------------------------"
contSep = text "\n"

-- Helper for makeReadMe and dependencies
filtEmp :: [Doc] -> [Doc]
filtEmp = filter isEmpty 

-- Helper for dependencies
libStatment :: (Name, Version) -> Doc
libStatment ("","") = empty
libStatment (nam,vers) = (doubleQuotes . text) nam <+> text "version" <+> text vers