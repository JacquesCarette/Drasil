module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Language.Drasil.Mod (Name)
import Language.Drasil.Choices (ImplementationType(..))

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, text)


makeReadMe :: String -> String -> Maybe String -> ImplementationType -> [Name] ->
    String -> Doc
makeReadMe progLang progLangVers unsupportedOSs imp extLibs caseName = 
    vcat $ intersperse seperator $ [
    introInformation caseName progLang progLangVers,
    dependencies extLibs] ++
    operatingSystems unsupportedOSs ++
    buildInstructions imp

--Information we still need to find

seperator :: Doc
seperator = text "\n------------------------------------------------------------"

introInformation :: String -> String -> String -> Doc
introInformation name pl plv = text $
    "### Summary of Key Information for Running " ++ 
    name ++ " on " ++ pl ++ 
    -- "\nLast Updated: " ++ getDateTime currentDateTime ++ 
    "\n" ++ pl ++ " Version: " ++ plv

dependencies:: [Name] -> Doc
dependencies lib = text $ "**Program Dependencies:**" ++ "\n - " ++ (show lib)

operatingSystems :: Maybe String -> [Doc]
operatingSystems Nothing = []
operatingSystems (Just unsuppOS) = [text $ "**Unsupported OSs:**" ++ "\n - " ++
    unsuppOS]

buildInstructions :: ImplementationType -> [Doc]
buildInstructions Library = []
buildInstructions Program =  [text $ "**How to Run Program:**" ++ 
    "\n - Enter in the following line into your terminal command line: "
    ++ "\n`make run RUNARGS=" ++ "input.txt" ++ "`"]