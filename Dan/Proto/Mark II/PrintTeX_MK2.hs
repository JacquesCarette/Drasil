{-# OPTIONS -Wall #-} 
module PrintTeX_MK2 where
import ASTTeX_MK2
import ToTeX_MK2
import Text.PrettyPrint
import qualified ASTInternal_MK2 as A
import Prelude hiding (print)

genTeX :: A.DocType -> A.Document -> Doc
genTeX typ doc = build typ $ makeDocument doc

build :: A.DocType -> Document -> Doc
build A.SRS (Document title author contents) = text ""
build A.LPM (Document title author contents) = text ""
build A.Code (Document title author contents) = error "Unimplemented"

