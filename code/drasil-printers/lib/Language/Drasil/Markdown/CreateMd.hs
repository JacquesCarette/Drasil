-- | Markdown file creator for generated GOOL code.
module Language.Drasil.Markdown.CreateMd (
    -- * Main Function
    makeMd,
    -- * Section Creators
    introInfo, whatInfo, verInfo, unsupOS, extLibSec, regularSec, instDoc, endNote) 
    where

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty, vcat, text, (<+>),
    (<>), punctuate, hsep)
import Language.Drasil.Printing.Helpers (upcase)
import Utils.Drasil

-- | Combines a list of sentences into a final Doc, also appends end note.
makeMd :: [Doc] -> Doc
makeMd = vcat . punctuate secSep . filterEmpty

-- | Example title, authors, and maybe purpose section.
introInfo :: String -> [String] -> Maybe String -> Doc
introInfo name auths descr = introSec (text name) (listToDoc auths) (length auths) (maybePurpDoc descr)

-- | Instruction section, contains 4 paragraphs, Running, Building, Input-Output and Config Files.
-- The Config file section is only displayed if there are configuration files.
instDoc :: [String] -> String -> (String, String) -> Doc
instDoc cfp name inoutn = regularSec (text "Making Examples") 
    (runInstDoc inoutn <> doubleSep <> makeInstDoc) <> inOutFile name inoutn <> configSec cfp

-- | Helper for creating optional Purpose subsection as Doc
maybePurpDoc :: Maybe String -> Doc
maybePurpDoc = maybe empty (\descr-> doubleSep <> text "> Purpose:" <+> upcase descr)

-- | 'What' section in generated README file, does not display if empty
whatInfo :: Maybe String -> Doc
whatInfo = maybe empty (regularSec (text "What") . text)

-- | Helper for giving instructions on the command line.
commandLine :: Doc
commandLine = text $ "In your terminal command line, enter the same directory as this " ++
    "README file. Then enter the following line:"

-- | Helper for giving instructions on how to run the program.
runInstDoc :: (String, String) -> Doc
runInstDoc (inFile, _) = text "How to Run the Program:" <> contSep <>
    commandLine <> contSep <> bkQuote3 <> contSep <> text "make run RUNARGS=" <> text inFile
      <> contSep <> bkQuote3

-- | Helper for giving instructions on how to build the program.
makeInstDoc :: Doc
makeInstDoc = text "How to Build the Program:" <> contSep <> commandLine <> contSep <>
    bkQuote3 <> contSep <> text "make build" <> contSep <> bkQuote3

-- | Helper for giving instuctions and Input and Output files.
-- * This needs a more permanent solution (For cases of no Input/Output file).
inOutFile :: String -> (String, String) -> Doc
inOutFile name (inFile, outFile) = doubleSep <>
      text "How to Change Input:" <> contSep <> text name <+> 
      text "will take the inputs from" <+> bkQuote <> text inFile <> bkQuote <+> 
      text "and write the outputs to" <+> bkQuote <> text outFile <> bkQuote <> 
      text "." <> contSep <> text "Inputs can be changed by editing" <+> bkQuote <> 
      text inFile <> bkQuote <> text "."

-- | Helper for giving instructions for configuration files.
configSec :: [String] -> Doc
configSec [] = empty
configSec cfp = doubleSep <> regularSec (text "Configuration Files") 
  (text ("Configuration files are files that must be " ++
    "in the same directory as the executable in order to run or build successfully.")
    <> doubleSep <> bkQuote <> listToDoc cfp <> bkQuote)

-- | Language version section.
verInfo :: String -> String -> Doc
verInfo pl plv = regularSec (text "Version") (bkQuote <> text pl <+> text plv <> bkQuote)

-- | Invalid Operating Systems section, does not display unless atleast 1 invalid OS.
unsupOS :: Maybe String -> Doc
unsupOS = maybe empty (\uns-> regularSec (text "Unsupported Operating Systems")
    (text $ "- " ++ uns))

-- | External Libraries section. The inputs are a list of name and version pairs
-- and a list of the corresponding version numbers, these are first combined into a 
-- list of triplets, and then each printed on a new line.
extLibSec:: [(String, String)] -> [String]-> Doc
extLibSec libns libfps = 
    let libs = addListToTuple libns libfps
        formattedLibs = (hsep . punctuate contSep . filterEmpty . 
            map libStatment) libs
    in if isEmpty formattedLibs then empty else 
            regularSec (text "External Libraries") formattedLibs

-- | Helper for formatting the library section.
libStatment :: (String, String, String) -> Doc
libStatment ("","", _) = empty
libStatment (nam,vers, fp) = bkQuote <> text nam <+>
    text vers <> bkQuote <> if fp == "" then empty else
    text ". The local file path to the library is" <+> bkQuote <> text fp <> bkQuote

-- | Helper for converting a list of tuples and another list into a list of triplets.
addListToTuple :: [(String,String)] -> [String] -> [(String, String, String)]
addListToTuple [] [] = []
addListToTuple ((n,v):_) [] = [(n,v,"")]
addListToTuple ((n,v):xtup) (l:xlst) = (n,v,l):addListToTuple xtup xlst
addListToTuple _ _ = []

-- TODO: Allow licenses to have updated date information.
-- | License section.
license :: Doc -> Doc
license auth = text "Copyright (c) 2021," <+> auth <>
  text ". All rights reserved. Please see the [full license](https://github.com/JacquesCarette/Drasil/blob/4b9ad0a3016fecb3c7a2aa82ab142f9e805b5cc8/LICENSE) for more details."

-- | End section.
endNote :: Int -> [String] -> Doc
endNote num auth = text "*This README is a software artifact generated by Drasil.*" 
  <> doubleSep <> license (listToDoc auth) <> doubleSep <>
  drasilImage num

-- | Section seperators.
secSep, doubleSep, bkQuote, bkQuote3 :: Separator
-- | Horizontal line separator.
secSep = text "\n\n------------------------------------------------------------"
-- | Double newline separator.
doubleSep = text "\n\n"
-- | Back quote separator.
bkQuote = text "`"
-- | Triple backquote separator.
bkQuote3 = text "```"


-- FIXME as explained in #2224 we still need to add in the purpose section, 
-- this could be done by adding a third parameter to introSec
-- | Constructs introduction section from header and message.
introSec ::  Doc -> Doc -> Int -> Doc -> Doc
introSec hd ms1 l descr = text "#" <+> hd <+> contSep <> (if l == 1 then text "> Author:" else text "> Authors: ") 
  <+> ms1 <> descr

-- | Constructs regular section section from header and message.
regularSec :: Doc -> Doc -> Doc
regularSec hd ms = text "##" <+> hd <+> contSep <+> ms

-- Function to create the prefix for the path of the Drasil Logo
buildPath :: Int -> String
buildPath num = filter (/= ' ') $ unwords $ replicate num "../"

-- | Drasil Tree icon. Uses HTML directly to format image since normal markdown doesn't support it.
drasilImage :: Int -> Doc
drasilImage num = alignImage (buildPath num ++
  "drasil-website/WebInfo/images/Icon.png")

-- | Aligns an image to the center using HTML, since markdown doesn't support it.
alignImage :: FilePath -> Doc
alignImage img = text "<p align=\"center\">" <> contSep <> text ("<img src=\"" 
  ++ img ++ "\" alt=\"Drasil Tree\" width=\"200\" />") <> contSep <> text "</p>"
