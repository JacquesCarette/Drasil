module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, text)

makeReadMe :: String -> Doc
makeReadMe caseName = (vcat . map text) $ intersperse seperator [
    introInformation caseName progLang progLangVers,
    dependencies libraries,
    operatingSystems unsupportedOSs,
    buildInstructions inputFilePath]


--Information we still need to find
progLang, progLangVers, libraries, unsupportedOSs, inputFilePath :: String
progLang = "___Lang___"
progLangVers = "___Version___"
libraries = "___Libraries___"
unsupportedOSs = "___Unsupported_OS's__"
inputFilePath = "___path___"



introInformation :: String -> String -> String -> String
dependencies, operatingSystems, buildInstructions 
    :: String -> String

seperator :: String

seperator = "\n------------------------------------------------------------\n"

introInformation name pl plv = 
    "### Summary of Key Information for Running " ++ 
    name ++ " on " ++ pl ++ 
    -- "\nLast Updated: " ++ getDateTime currentDateTime ++ 
    "\n" ++ pl ++ " Version: " ++ plv

dependencies lib = "**Program Dependencies:**" ++ "\n - " ++ lib

operatingSystems unsuppOS = "**Unsupported OSs:**" ++ "\n - " ++
    unsuppOS

buildInstructions inpFP =  "**How to Run Program:**" ++ 
    "\n - Enter in the following line into your terminal command line: "
    ++ "\n`make run RUNARGS=" ++ inpFP ++ "`"