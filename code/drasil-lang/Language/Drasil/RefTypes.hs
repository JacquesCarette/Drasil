-- | Contains the types associated to references
module Language.Drasil.RefTypes(
  RefAdd, RefType(..), DType(..), Reference(Reference)) where

import Language.Drasil.ShortName (ShortName)
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
             | Mod    -- ^ Module
             | Assump -- ^ Assumption
             | LCh     -- ^ Likely Change
             | UnCh     -- ^ Unlikely Change
             | EqnB   -- ^ Equation Block
             | Cite   -- ^ Citation
             | Goal   -- ^ Goal Statement
             | Label RefType    -- ^ Label --FIXME: hack (#971)
             | Blank  -- ^ Prefix filler for ConceptInstance
             | DeferredCC UID  -- ^ For ConceptInstances --FIXME: Used by References to create a Deferred ShortName (#562)
             | Link -- ^ URI

data Reference = Reference RefType RefAdd ShortName

instance Show RefType where
  show Tab    = "Table"
  show Lst    = "List"
  show Fig    = "Figure"
  show Sect   = "Section"
  show (Label x) = show x --FIXME: hack (#971)
  show Mod    = "Module"
  show (Def _)= "Definition"
  show Assump = "Assumption"
  show LCh    = "Likely Change"
  show UnCh   = "Unlikely Change"
  show Cite   = "Citation"
  show Goal   = "Goal Statement"
  show EqnB   = "Equation"
  show Blank  = "Blank"
  show (DeferredCC _) = error "Cannot directly display deferred reference types." -- FIXME
  show Link   = "Link"
