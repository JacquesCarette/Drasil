{-# OPTIONS -Wall #-} 
module PrintTeX_MK2 where
import ASTTeX_MK2
import ToTeX_MK2
import Text.PrettyPrint
import qualified ASTInternal_MK2 as A
import Prelude hiding (print)
import Config_MK2 (srsTeXParams)
import Helpers_MK2
import Chunk_MK2 (find)

genTeX :: A.DocType -> A.Document -> Doc
genTeX typ doc = build typ $ makeDocument doc

build :: A.DocType -> Document -> Doc
build A.SRS doc = buildSRS srsTeXParams doc
build A.LPM _ = error "Unimplemented"
build A.Code _ = error "Unimplemented"

buildSRS :: [A.DocParams] -> Document -> Doc
buildSRS ((A.DocClass sb b) : (A.UsePackages ps) : []) (Document t a c) =
  docclass sb b $$ listpackages ps $$ title (p_spec Pg t) $$ 
  author (p_spec Pg a) $$ print c
buildSRS _ _ = error "Invalid syntax in Document Parameters"

listpackages :: [String] -> Doc
listpackages []     = empty
listpackages (p:ps) = usepackage p $$ listpackages ps

print :: [LayoutObj] -> Doc
print [] = empty
print (c:cs) = text "" $$ print cs

p_spec :: Context -> Spec -> String
p_spec _ (S s) = s
p_spec _ (E e) = p_expr e
p_spec con (a :+: b) = p_spec con a ++ p_spec con b
p_spec con (a :-: b) = p_spec con a ++ "_" ++ brace (p_spec con b)
p_spec con (a :^: b) = p_spec con a ++ "^" ++ brace (p_spec con b)
p_spec Pg (CS c) = dollar (p_spec Pg (spec (find A.Symbol c "Erroneous use of chunk")))
p_spec con (CS c) = p_spec con $ spec (find A.Symbol c "Erroneous use of chunk")

p_expr _ = ""