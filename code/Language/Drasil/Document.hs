{-# LANGUAGE GADTs #-}
module Language.Drasil.Document where

import Language.Drasil.Chunk (name)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Other
import Language.Drasil.Chunk.Req
import Language.Drasil.Chunk.LC
import Language.Drasil.Spec (Sentence(..), RefType(..))
import Language.Drasil.RefHelpers
import Language.Drasil.Expr
import Control.Lens ((^.))

type Title    = Sentence
type Author   = Sentence
type Header   = Sentence -- Used when creating sublists
type Depth    = Int
type Width    = Float
type Height   = Float
type Pair     = (Title,ItemType) -- Title: Item
type Filepath = String
type Label    = Sentence
type Sections = [Section]

data Document = Document Title Author Sections

data SecCons = Sub Section
             | Con Contents

data Section = Section Title [SecCons]

--Types of layout objects we deal with explicitly
data Contents = Table [Sentence] [[Sentence]] Title Bool
  --table header data label showlabel?
               | Paragraph Sentence
               | EqnBlock Expr
     --          | CodeBlock Code   -- GOOL complicates this.  Removed for now.
               | Definition DType
               | Enumeration ListType
               | Figure Label Filepath--Should use relative file path.
               | Module ModuleChunk
               | Requirement ReqChunk
               | Assumption AssumpChunk
               | LikelyChange LCChunk
               | UnlikelyChange UCChunk
     --          | UsesHierarchy [(ModuleChunk,[ModuleChunk])]
               | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Label

data ListType = Bullet [ItemType]
              | Number [ItemType] 
              | Simple [Pair]
              | Desc [Pair]
         
data ItemType = Flat Sentence 
              | Nested Header ListType
               
-- Types of definitions
data DType = Data QDefinition 
           | General 
           | Theory RelationChunk

class LayoutObj l where
  refName :: l -> Sentence
  rType   :: l -> RefType

instance LayoutObj Section where
  refName (Section t _) = S "Sec:" :+: inferName t
  rType _ = Sect

instance LayoutObj Contents where
  refName (Table _ _ l _)     = S "Table:" :+: inferName l
  refName (Figure l _)        = S "Figure:" :+: inferName l
  refName (Paragraph _)       = error "Can't reference paragraphs" --yet
  refName (EqnBlock _)        = error "EqnBlock ref unimplemented"
--  refName (CodeBlock _)       = error "Codeblock ref unimplemented"
  refName (Definition d)      = getDefName d
  refName (Enumeration _)     = error "List refs unimplemented"
  refName (Module mc)         = S $ "M" ++ alphanumOnly (mc ^. name)
  refName (Requirement rc)    = S $ "R" ++ alphanumOnly (rc ^. name)
  refName (Assumption ac)     = S $ "A" ++ alphanumOnly (ac ^. name)
  refName (LikelyChange lcc)  = S $ "LC" ++ alphanumOnly (lcc ^. name)
  refName (UnlikelyChange ucc)= S $ "UC" ++ alphanumOnly (ucc ^. name)
--  refName (UsesHierarchy _)   = S $ "Figure:UsesHierarchy"
  refName (Graph _ _ _ l)     = S "Figure:" :+: inferName l
  rType (Table _ _ _ _)    = Tab
  rType (Figure _ _)       = Fig
  rType (Definition _)     = Def
  rType (Module _)         = Mod
  rType (Requirement _)    = Req
  rType (Assumption _)     = Assump
  rType (LikelyChange _)   = LC
  rType (UnlikelyChange _) = UC
--  rType (UsesHierarchy _)  = Fig
  rType (Graph _ _ _ _)    = Fig
  rType _ = error "Attempting to reference unimplemented reference type"
  
getDefName :: DType -> Sentence
getDefName (Data c)   = S $ "DD:" ++ (repUnd (c ^. name))
getDefName (Theory c) = S $ "T:" ++ firstLetter (repUnd (c ^. name))
getDefName _          = error "Unimplemented definition type reference"