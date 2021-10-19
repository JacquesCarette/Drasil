-- | Defines helper functions for creating jupyter notebooks.
module Language.Drasil.JSON.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, empty, (<>), vcat, hcat)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import qualified Text.JSON as J (encode)

import Language.Drasil (MaxWidthPercent)
import Language.Drasil.HTML.Helpers (img)

data Variation = Class | Id

tr, td, figure, li, pa, ba :: Doc -> Doc
-- | Table row tag wrapper
tr         = wrap "tr" []
-- | Table cell tag wrapper
td         = wrap "td" []
-- | Figure tag wrapper
figure     = wrap "figure" []
-- | List tag wrapper
li         = wrap' "li" []
-- | Paragraph in list tag wrapper
pa         = wrap "p" []

ba         = wrap "b" []

ol, ul, table :: [String] -> Doc -> Doc
-- | Ordered list tag wrapper
ol       = wrap "ol"
-- | Unordered list tag wrapper
ul       = wrap "ul"
-- | Table tag wrapper
table    = wrap "table"

nbformat :: Doc -> Doc
nbformat s = text ("    " ++ J.encode (show s ++ "\n") ++ ",")

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen' vcat Class a empty

wrap' :: String -> [String] -> Doc -> Doc
wrap' a = wrapGen' hcat Class a empty

wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x -> 
  let tb c = text $ "<" ++ c ++ ">"
  --in sepf [quote(tb s), x, quote(tb $ '/':s)]
  in if s == "li" then sepf [tb s, x, tb $ '/':s] else sepf [nbformat(tb s), x, nbformat(tb $ '/':s)]
wrapGen' sepf Class s _ ts = \x ->
  let tb c = text $ "<" ++ c ++ " class=\\\"" ++ foldr1 (++) (intersperse " " ts) ++ "\\\">"
  in let te c = text $ "</" ++ c ++ ">"
  in sepf [nbformat(tb s), x, nbformat(te s)]
wrapGen' sepf Id s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " id=\\\"") <> ti <> text "\\\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [nbformat(tb s), x, nbformat(te s)] 

refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen' vcat Id "div") [""]

refID :: Doc -> Doc 
refID i = nbformat $ text "<a id=\"" <> i <> text "\"></a>"

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text "[" <> txt <> text ("](#" ++ ref ++ ")")
--reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

-- | Helper for setting up links to external URIs
reflinkURI :: String -> Doc -> Doc
reflinkURI ref txt = text ("<a href=\\\"" ++ ref ++ "\\\">") <> txt <> text "</a>"

-- | Helper for setting up figures
image :: Doc -> Doc -> MaxWidthPercent -> Doc
image f c 100 = 
  figure $ vcat [
  nbformat $ img [("src", f), ("alt", c)]]
image f c wp =
  figure $ vcat [
  nbformat $ img [("src", f), ("alt", c), ("width", text $ show wp ++ "%")]]

h :: Int -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 4 = error "Illegal header (too large)"          
          | otherwise = text (hash n)
              where hash 1 = "# "
                    hash 2 = "## "
                    hash 3 = "### "
                    hash 4 = "#### "
                    hash _ = "Illegal header"

{- Will delete it after checking encode works well
-- JSON formatter
formatter :: String -> String
formatter ('"':xs) = '\\' : '"' : formatter xs
formatter ('\\':xs) = '\\' : '\\' : formatter xs
formatter (x:xs) = x: formatter xs
formatter [] = []

jf :: String -> Doc
jf s = text $ replace "`" "\'" (formatter s)
-}

stripnewLine :: String -> Doc
stripnewLine s = hcat (map text (splitOn "\n" s))
--filter (`notElem` "\n" )

makeMetadata :: Doc  
makeMetadata = vcat [
  text " \"metadata\": {", 
  vcat[
    text "  \"kernelspec\": {", 
    text "   \"display_name\": \"Python 3\",", 
    text "   \"language\": \"python\",",
    text "   \"name\": \"python3\"", 
    text "  },"],
  vcat[
    text "  \"language_info\": {", 
    text "   \"codemirror_mode\": {", 
    text "    \"name\": \"ipython\",",
    text "    \"version\": 3",
    text "   },"],
  text "   \"file_extension\": \".py\",", 
  text "   \"mimetype\": \"text/x-python\",",
  text "   \"name\": \"python\",",
  text "   \"nbconvert_exporter\": \"python\",",
  text "   \"pygments_lexer\": \"ipython3\",",
  text "   \"version\": \"3.9.1\"",
  text "  }",
  text " },",
  text " \"nbformat\": 4,", 
  text " \"nbformat_minor\": 4" 
  ]
