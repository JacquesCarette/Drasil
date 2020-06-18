module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, text)

makeReadMe :: String -> String -> String -> String -> String -> Doc
makeReadMe progLang progLangVers unsupportedOSs inputFilePath caseName = 
    (vcat . map text) $ intersperse seperator [
    introInformation caseName progLang progLangVers,
    dependencies libraries,
    operatingSystems unsupportedOSs,
    buildInstructions inputFilePath]

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

operatingSystems :: String -> String
operatingSystems unsuppOS = "**Unsupported OSs:**" ++ "\n - " ++
    unsuppOS

buildInstructions :: String -> String
buildInstructions inpFP =  "**How to Run Program:**" ++ 
    "\n - Enter in the following line into your terminal command line: "
    ++ "\n`make run RUNARGS=" ++ inpFP ++ "`"