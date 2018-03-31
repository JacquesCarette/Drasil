module Language.Drasil.TeX.AST where

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

data LayoutObj = Table Tags [[Spec]] Label Bool Caption
               | Section Depth Title [LayoutObj] Label
               | Paragraph Contents
               | EqnBlock Contents
               | Definition [(String,[LayoutObj])] Label
               | List ListType
               | Figure Label Caption Filepath MaxWidthPercent
               | Requirement Contents Label
               | Assumption Contents Label
               | LikelyChange Contents Label
               | UnlikelyChange Contents Label
               | Graph [(Spec, Spec)] (Maybe Width) (Maybe Height) Caption Label
               | Bib BibRef
               | Defnt DType [(String,[LayoutObj])] Label -- To replace Definition eventually
