{-# LANGUAGE GADTs #-}
-- | Contains the types associated to references
module Language.Drasil.RefTypes(RefAdd, RefType(..), DType(..), ReqType(..)) where

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

-- | For building references. Defines the possible type of reference.
data RefType = Tab    -- ^ Table
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
             | Label    -- ^ Label


instance Show RefType where
  show Tab    = "Table"
  show Fig    = "Figure"
  show Sect   = "Section"
  show Label  = "Section" --FIXME: hack until section has labels
  show Mod    = "Module"
  show (Def _)= "Definition"
  show (Req _)= "Requirement"
  show Assump = "Assumption"
  show LCh     = "Likely Change"
  show UnCh     = "Unlikely Change"
  show Cite   = "Citation"
  show Goal   = "Goal Statement"
  show PSD    = "Physical System Description"
  show EqnB   = "Equation"
