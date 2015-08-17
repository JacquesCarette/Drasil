{-# OPTIONS -Wall #-} 
module PrintTeX_MK2 where
import ASTTeX_MK2
import ToTeX_MK2
import Text.PrettyPrint
import qualified ASTInternal_MK2 as A
import Prelude hiding (print)

genTeX :: A.DocType -> A.Document -> Doc
genTeX typ doc = print typ $ makeDocument doc

print :: A.DocType -> Document -> Doc
print typ (Document title author contents) = text ""
  