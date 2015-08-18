{-# OPTIONS -Wall #-} 
module PrintTeX_MK2 where
import ASTTeX_MK2
import ToTeX_MK2
import Text.PrettyPrint
import qualified ASTInternal_MK2 as A
import Prelude hiding (print)
import Config_MK2 (srsTeXParams)
import Helpers_MK2

genTeX :: A.DocType -> A.Document -> Doc
genTeX typ doc = build typ $ makeDocument doc

build :: A.DocType -> Document -> Doc
build A.SRS doc = buildSRS srsTeXParams doc
build A.LPM _ = error "Unimplemented"
build A.Code _ = error "Unimplemented"

buildSRS :: [A.DocParams] -> Document -> Doc
buildSRS ((A.DocClass sb b) : (A.UsePackages ps) : []) (Document t a c) =
  docclass sb b $$ listpackages ps $$ title (p_spec t) $$ author (p_spec a) $$
  print c
buildSRS _ _ = error "Invalid syntax in Document Parameters"

listpackages :: [String] -> Doc
listpackages []     = empty
listpackages (p:ps) = usepackage p $$ listpackages ps

print :: [LayoutObj] -> Doc
print [] = empty
print (c:cs) = text "" $$ print cs

p_spec :: Spec -> String
p_spec (S s) = s
p_spec _ = ""