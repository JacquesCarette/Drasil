-- | Contains the types associated to references
module Language.Drasil.RefTypes(
  RefAdd, RefType(..), DType(..), LinkType(..)) where

import Language.Drasil.UID (UID)

-- | Types of definitions
data DType = General
           | Instance
           | TM
           | DD

type RefAdd = String

-- | For building references. Defines the possible type of reference.
data RefType = Tab    -- ^ Table
             | Lst   -- ^ List
             | Fig    -- ^ Figure
             | Sect   -- ^ Section
             | Def DType  -- ^ Definition (includes theoretical models) (DType used to set shortnames)
             | Assump -- ^ Assumption
             | EqnB   -- ^ Equation Block
             | Cite   -- ^ Citation
             | Blank  -- ^ Prefix filler for ConceptInstance
             | DeferredCC UID  -- ^ For ConceptInstances --FIXME: Used by References to create a Deferred ShortName (#562)
             | Link -- ^ URI

-- we do need to know a bit more about references, such as whether they are
-- an internal (document) link or a citation (and eventually, external).
-- basically, this amounts to a choice between
-- \ref (or \hyperref) and \cite in LaTeX.
-- in a sense, Cite2 is a very special kind of External reference.
data LinkType = Internal | Cite2 | External

instance Show RefType where
  show Tab    = "Table"
  show Lst    = "List"
  show Fig    = "Figure"
  show Sect   = "Section"
  show (Def _)= "Definition"
  show Assump = "Assumption"
  show Cite   = "Citation"
  show EqnB   = "Equation"
  show Blank  = "Blank"
  show (DeferredCC _) = error "Cannot directly display deferred reference types." -- FIXME
  show Link   = "Link"
