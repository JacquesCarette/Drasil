{-# LANGUAGE GADTs #-}
-- | Contains the types associated to references
module Language.Drasil.RefTypes(RefAdd, RefType(..), DType(..), ReqType(..)) where

import Language.Drasil.UID (UID)

--import Language.Drasil.Document.Core (DType(..)) cannot be imported due to importcycles

-- | Types of definitions
data DType = General
           | Instance
           | TM
           | DD

type RefAdd  = String

-- | What type of requirement are we dealing with?
data ReqType = FR  -- ^ Functional Requirement
             | NFR -- ^ Non-Functional Requirement
  deriving Eq

instance Show ReqType where
  show FR  = "FR"
  show NFR = "NFR"

-- | For building references. Defines the possible type of reference.
data RefType = Tab    -- ^ Table
             | Lst   -- ^ List
             | Fig    -- ^ Figure
             | Sect   -- ^ Section
             | Def DType  -- ^ Definition (includes theoretical models) (DType used to set shortnames)
             | Mod    -- ^ Module
             | Req ReqType -- ^ Requirement
             | Assump -- ^ Assumption
             | LCh     -- ^ Likely Change
             | UnCh     -- ^ Unlikely Change
             | EqnB   -- ^ Equation Block
             | Cite   -- ^ Citation
             | Goal   -- ^ Goal Statement
             | PSD    -- ^ Physical System Description
             | Label RefType    -- ^ Label --FIXME: hack (#971)
             | Blank  -- ^ Prefix filler for ConceptInstance
             | DeferredCC UID  -- ^ For ConceptInstances --FIXME: Used by References to create a Deferred ShortName (#562)

instance Show RefType where
  show Tab    = "Table"
  show Lst    = "List"
  show Fig    = "Figure"
  show Sect   = "Section"
  show (Label x) = show x --FIXME: hack (#971)
  show Mod    = "Module"
  show (Def _)= "Definition"
  show (Req _)= "Requirement"
  show Assump = "Assumption"
  show LCh    = "Likely Change"
  show UnCh   = "Unlikely Change"
  show Cite   = "Citation"
  show Goal   = "Goal Statement"
  show PSD    = "Physical System Description"
  show EqnB   = "Equation"
  show Blank  = "Blank"
  show (DeferredCC _) = error "Cannot directly display deferred reference types." -- FIXME
