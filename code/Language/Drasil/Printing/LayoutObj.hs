module Language.Drasil.Printing.LayoutObj where

import Language.Drasil.Document (MaxWidthPercent, DType)
import Language.Drasil.Printing.AST
import Language.Drasil.Printing.Citation (BibRef)

data Document = Document Title Author [LayoutObj]
type Author   = Spec
type Contents = Spec
type Items    = [LayoutObj]
type Tags     = [String]
type Depth    = Int
type Width    = Float
type Height   = Float
type Label    = Spec
type Filepath = String
type Caption  = Spec

data ALUR = Assumption | LikelyChange | UnlikelyChange | Requirement
data LayoutObj = 
     Table Tags [[Spec]] Label Bool Caption
   | Header Depth Title Label
   | Paragraph Contents
   | EqnBlock Contents
   | Definition DType [(String,[LayoutObj])] Label
   | List ListType
   | Figure Label Caption Filepath MaxWidthPercent
   | ALUR ALUR Contents Label Label -- two labels?
   | Graph [(Spec, Spec)] (Maybe Width) (Maybe Height) Caption Label
   | HDiv Tags [LayoutObj] Label
   -- this shouldn't be here, it should have been expanded.
   | Bib BibRef
