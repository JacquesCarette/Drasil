{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
module LayoutObjs where

import EqChunk
import RelationChunk
import Spec
import ASTCode

type Title    = Spec
type Contents = Spec
type Author   = Spec
type Item     = Spec
type Bullets  = [Item]
type Items    = [Item]
type Depth    = Int
type Pairs    = [(Title,Item)] -- Title: Item

data Document = Document Title Author [LayoutObj]

--Types of layout objects we deal with explicitly
data LayoutObj = Table [Spec] [[Spec]] Title Bool
  --table header data label showlabel?
               | Section Depth Title [LayoutObj] 
                  --Section = 0 depth, subsection = 1, subsub = 2 ... etc.
               | Paragraph Contents
               | EqnBlock Contents
               | CodeBlock Code
               | Definition DType
               | BulletList Bullets
               | NumberedList Items
               | SimpleList Pairs

-- Types of definitions
data DType = Data EqChunk 
           | General 
           | Theory RelationChunk