module LayoutObjs where

import EqChunk
import RelationChunk
import Spec
import ASTCode

type Title    = Spec
type Contents = Spec
type Author   = Spec
type Bullets  = [Spec]
type Items    = [Spec]
type Depth    = Int

data Document = Document Title Author [LayoutObj]

--Types of layout objects we deal with explicitly
data LayoutObj = Table [Spec] [[Spec]] -- table header then data
               | Section Depth Title [LayoutObj] 
                  --Section = 0 depth, subsection = 1, subsub = 2 ... etc.
               | Paragraph Contents
               | EqnBlock Contents
               | CodeBlock Code
               | Definition DType
               | BulletList Bullets
               | NumberedList Items

-- Types of definitions
data DType = Data EqChunk 
           | General 
           | Theory RelationChunk