module Language.Drasil.Markdown.CreateMd (
    makeMd, introInfo, verInfo, unsupOS, extLibSec, regularSec, instDoc) 
    where

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty, vcat, text, (<+>),
    (<>), comma, punctuate, hsep)

-- | Separates document sections.
type Seperator = Doc

-- | Combines a list of sentences into a final Doc, also appends end note.
makeMd :: [Doc] -> Doc
makeMd lst = (vcat . punctuate secSep . filtEmp) lst <> contSep <>
    doubleSep <> endNote 

-- | Example title and purpose section.
introInfo :: String -> [String] -> Doc
introInfo name auths = introSec (text name) (listToDoc auths)

-- | Instruction section, contains 3 paragraphs, Running, Building and Config Files.
-- The Config file section is only displayed if there are configuration files.
instDoc :: [String] -> Doc
instDoc cfp = regularSec (text "Making Examples") 
    (runInstDoc <> doubleSep <> makeInstDoc) <> configSec cfp 

-- | Helper for giving instructions on the command line.
commandLine :: Doc
commandLine = text $ "In your terminal command line, enter the same directory as this " ++
    "README file. Then enter the following line:"

-- | Helper for giving instructions on how to run the program.
runInstDoc :: Doc
runInstDoc = text "How to Run the Program:" <> contSep <>
    commandLine <> contSep <> bkQuote3 <> contSep <> text "make run RUNARGS=input.txt" <> contSep <> bkQuote3

-- | Helper for giving instructions on how to build the program.
makeInstDoc :: Doc
makeInstDoc = text "How to Build the Program:" <> contSep <> commandLine <> contSep <>
    bkQuote3 <> contSep <> text "make build" <> contSep <> bkQuote3

-- | Helper for giving instructions for configuration files.
configSec :: [String] -> Doc
configSec [] = empty
configSec cfp = doubleSep <> regularSec (text "Configuration Files") (text ("Configuration files are files that must be " ++
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
        formattedLibs = (hsep . punctuate contSep . filtEmp . 
            map libStatment) libs
    in if isEmpty formattedLibs then empty else 
            regularSec (text "External Libraries") formattedLibs

-- | Helper for formatting the library section.
libStatment :: (String, String, String) -> Doc
libStatment ("","", _) = empty
libStatment (nam,vers, fp) = bkQuote <> text nam <> bkQuote
    <+> text vers <> if fp == "" then empty else
    text ". The local file path to the library is" <+> bkQuote <> text fp <> bkQuote

-- | Helper for converting a list of tuples and another list into a list of triplets.
addListToTuple :: [(String,String)] -> [String] -> [(String, String, String)]
addListToTuple [] [] = []
addListToTuple ((n,v):_) [] = [(n,v,"")]
addListToTuple ((n,v):xtup) (l:xlst) = (n,v,l):addListToTuple xtup xlst
addListToTuple _ _ = []

-- | License section (currently empty).
license :: Doc
license = text "Copyright (c) 2014, JacquesCarette\
  \All rights reserved.\n \
  \Redistribution and use in source and binary forms, with or without\
  \modification, are permitted provided that the following conditions are met:\n \
  \* Redistributions of source code must retain the above copyright notice, this\
  \  list of conditions and the following disclaimer.\n \
  \* Redistributions in binary form must reproduce the above copyright notice,\
  \  this list of conditions and the following disclaimer in the documentation\
  \  and/or other materials provided with the distribution.\n\n \
  \THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\
  \AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\
  \IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\
  \DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE\
  \FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\
  \DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR\
  \SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER\
  \CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,\
  \OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\
  \OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
-- | Drasil Tree icon.
drasilImage :: Doc
drasilImage = text "![Drasil Tree](../../../../drasil-website/WebInfo/images/Icon.png)"
-- | End section.
endNote :: Doc
endNote = secSep <> contSep <> text "*This README is a software artifact generated by Drasil.*" <> doubleSep <> drasilImage <> contSep <> license

-- | Section seperators.
secSep, contSep, doubleSep, bkQuote, bkQuote3 :: Seperator
-- | Horizontal line separator.
secSep = text "\n\n------------------------------------------------------------"
-- | Newline separator.
contSep = text "\n"
-- | Double newline separator.
doubleSep = text "\n\n"
-- | Back quote separator.
bkQuote = text "`"
-- | Triple backquote separator.
bkQuote3 = text "```"


-- FIXME as explained in #2224 we still need to add in the purpose section, 
-- this could be done by adding a third parameter to introSec
-- | Constructs introduction section from header and message.
introSec ::  Doc -> Doc -> Doc
introSec hd ms1 = text "#" <+> hd <+> contSep <> text "> Authors: " <+> ms1 

-- | Constructs regular section section from header and message.
regularSec :: Doc -> Doc -> Doc
regularSec hd ms = text "##" <+> hd <+> contSep <+> ms

-- | Helper for 'makeMd' and 'extLibSec'.
filtEmp :: [Doc] -> [Doc]
filtEmp = filter (not . isEmpty) 

-- | Helper for authors and configuration files.
listToDoc :: [String] -> Doc
listToDoc = hsep . punctuate comma . map text
