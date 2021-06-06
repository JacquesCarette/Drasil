module Language.Drasil.JSON.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, empty, (<>), (<+>), vcat, hcat, cat, nest)
import Data.List (intersperse)
import Data.List.Split
import Data.List.Utils (replace)

import Language.Drasil (MaxWidthPercent)
import Language.Drasil.HTML.Helpers (img)

{-
conHTMLformat :: String -> Document -> [String]
conHTMLformat fn (Document t a c) = 
  let splitLine = splitOn "\n" (conHTMLtoStr fn (Document t a c))
      delLeadspace = dropWhile (\c -> c == ' ')
  in map delLeadspace splitLine
 -}

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

quote :: Doc -> Doc
quote = wrapq "\"" []

cell :: Doc -> Doc
cell = wrapc " " []

--quote' :: Doc
--quote' = quote empty

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen' vcat Class a empty

wrap' :: String -> [String] -> Doc -> Doc
wrap' a = wrapGen' hcat Class a empty

wrapq :: String -> [String] -> Doc -> Doc
wrapq a = wrapquote hcat a empty
--wrap' a = wrapGen' hcat Class a empty

wrapc :: String -> [String] -> Doc -> Doc
wrapc a = wrapcell hcat a empty

wrapquote :: ([Doc] -> Doc) -> String -> Doc -> [String] -> Doc -> Doc
wrapquote sepf s _ [] = \x -> sepf [text $ "    " ++ s, x, text $ "\\n" ++ s ++ ","]

wrapcell :: ([Doc] -> Doc) -> String -> Doc -> [String] -> Doc -> Doc
wrapcell sepf s _ [] = \x -> 
  let tb = text "  {\n   \"cell_type\": \"markdown\",\n   \"metadata\": {},\n   \"source\": [" 
      te = text "   ]\n  },"
  in  sepf [text "  {\n   \"cell_type\":\n", x, text "\n   ]\n  },"]

wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x -> 
  let tb c = text $ "<" ++ c ++ ">"
  --in sepf [quote(tb s), x, quote(tb $ '/':s)]
  in if s == "li" then sepf [tb s, x, tb $ '/':s] else sepf [quote(tb s), x, quote(tb $ '/':s)]
wrapGen' sepf Class s _ ts = \x ->
  let tb c = text $ "<" ++ c ++ " class=\\\"" ++ foldr1 (++) (intersperse " " ts) ++ "\\\">"
  in let te c = text $ "</" ++ c ++ ">"
  in sepf [quote(tb s), x, quote(te s)]
wrapGen' sepf Id s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " id=\\\"") <> ti <> text "\\\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [quote(tb s), x, quote(te s)] 

--wrapGen :: Variation -> String -> Doc -> [String] -> Doc -> Doc
--wrapGen = wrapGen' cat

refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen' vcat Id "div") [""]

refID :: Doc -> Doc 
refID id = quote $ text "<a id=\\\"" <> id <> text "\\\"></a>"

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text "[" <> txt <> text ("](#" ++ ref ++ ")")
--reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

-- | Helper for setting up links to external URIs
reflinkURI :: String -> Doc -> Doc
reflinkURI ref txt = text ("<a href=\\\"" ++ ref ++ "\\\">") <> txt <> text "</a>"

indent :: Doc -> Doc
indent = nest 2

-- | Helper for setting up figures
image :: Doc -> Doc -> MaxWidthPercent -> Doc
image f c 100 = 
  figure $ vcat [
  quote $ jf $ show $ img [("src", f), ("alt", c)]]
image f c wp =
  figure $ vcat [
  quote $ jf $ show $ img [("src", f), ("alt", c), ("width", text $ show wp ++ "%")]]

h :: Int -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 4 = error "Illegal header (too large)"
          | otherwise = text (hash n)
              where hash 1 = "# "
                    hash 2 = "## "
                    hash 3 = "### "
                    hash 4 = "#### "


-- JSON formatter
formatter, f' :: String -> String
formatter ('"':xs) = '\\' : '"' : formatter xs
formatter ('\\':xs) = '\\' : '\\' : formatter xs
formatter (x:xs) = x: formatter xs
formatter [] = []

f' ('"':xs) = '\\' : '"' : f' xs
f' ('|':xs) = '\\' : '\\' : '|' : f' xs
f' (x:xs) = x: f' xs
f' [] = []

jf, jf' :: String -> Doc
jf s = text $ replace "`" "\'" (formatter s)
jf' s = text $ f' s

stripnewLine :: String -> Doc
stripnewLine s = hcat (map text (splitOn ("\n") s))
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

