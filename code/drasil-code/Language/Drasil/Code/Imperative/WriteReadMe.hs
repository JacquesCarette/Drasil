module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Language.Drasil.Choices (ImplementationType(..))

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, text)

makeReadMe :: String -> String -> Maybe String -> ImplementationType -> 
    String -> Doc
makeReadMe progLang progLangVers unsupportedOSs imp caseName = 
    (vcat . map text) $ intersperse seperator $ [
    introInformation caseName progLang progLangVers,
    dependencies libraries] ++
    operatingSystems unsupportedOSs ++
    buildInstructions imp

--Information we still need to find
libraries :: String
libraries = "___Libraries___"

seperator :: String
seperator = "\n------------------------------------------------------------"

introInformation :: String -> String -> String -> String
introInformation name pl plv = 
    "### Summary of Key Information for Running " ++ 
    name ++ " on " ++ pl ++ 
    -- "\nLast Updated: " ++ getDateTime currentDateTime ++ 
    "\n" ++ pl ++ " Version: " ++ plv

dependencies:: String -> String
dependencies lib = "**Program Dependencies:**" ++ "\n - " ++ lib

operatingSystems :: Maybe String -> [String]
operatingSystems Nothing = []
operatingSystems (Just unsuppOS) = ["**Unsupported OSs:**" ++ "\n - " ++
    unsuppOS]

buildInstructions :: ImplementationType -> [String]
buildInstructions Library = []
buildInstructions Program =  ["**How to Run Program:**" ++ 
    "\n - Enter in the following line into your terminal command line: "
    ++ "\n`make run RUNARGS=" ++ "input.txt" ++ "`"]