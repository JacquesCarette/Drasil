-- | Contains the types associated to references
module Language.Drasil.RefTypes(
  RefAdd, RefType(..), DType(..), Reference(Reference), ReqType(..), LinkType(..)) where

import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

-- | Types of definitions
data DType = General
           | Instance
           | TM
           | DD

type RefAdd = String

data ReqType = FR | NFR
-- | For building references. Defines the possible type of reference.
data RefType = Tab    -- ^ Table
             | Lst   -- ^ List
             | Fig    -- ^ Figure
             | Sect   -- ^ Section
             | Def DType  -- ^ Definition (includes theoretical models) (DType used to set shortnames)
             | Assump -- ^ Assumption
             | LCh     -- ^ Likely Change
             | UnCh     -- ^ Unlikely Change
             | Req ReqType
             | EqnB   -- ^ Equation Block
             | Cite   -- ^ Citation
             | Label RefType    -- ^ Label --FIXME: hack (#971)
             | Blank  -- ^ Prefix filler for ConceptInstance
             | DeferredCC UID  -- ^ For ConceptInstances --FIXME: Used by References to create a Deferred ShortName (#562)
             | Link -- ^ URI

-- we do need to know a bit more about references, such as whether they are
-- an internal (document) link or a citation (and eventually, external).
-- basically, this amounts to a choice between
-- \ref (or \hyperref) and \cite in LaTeX.
-- in a sense, Cite2 is a very special kind of External reference.
data LinkType = Internal | Cite2 | External

data Reference = Reference RefType RefAdd ShortName

instance Show RefType where
  show Tab    = "Table"
  show Lst    = "List"
  show Fig    = "Figure"
  show Sect   = "Section"
  show (Label x) = show x --FIXME: hack (#971)
  show (Def _)= "Definition"
  show (Req _)= "Requirement"
  show Assump = "Assumption"
  show LCh    = "Likely Change"
  show UnCh   = "Unlikely Change"
  show Cite   = "Citation"
  show EqnB   = "Equation"
  show Blank  = "Blank"
  show (DeferredCC _) = error "Cannot directly display deferred reference types." -- FIXME
  show Link   = "Link"
