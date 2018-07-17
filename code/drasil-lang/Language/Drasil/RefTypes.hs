{-# LANGUAGE GADTs #-}
-- | Contains the types associated to references
module Language.Drasil.RefTypes(RefAdd, RefType(..)) where

type RefAdd  = String

-- | For building references. Defines the possible type of reference.
data RefType = Tab    -- ^ Table
             | Fig    -- ^ Figure
             | Sect   -- ^ Section
             | Def    -- ^ Definition (includes theoretical models)
             | Mod    -- ^ Module
             | Req    -- ^ Requirement
             | Assump -- ^ Assumption
             | LC     -- ^ Likely Change
             | UC     -- ^ Unlikely Change
             | EqnB   -- ^ Equation Block
             | Cite   -- ^ Citation
             | Goal   -- ^ Goal Statement
             | PSD    -- ^ Physical System Description
             | Lbl    -- ^ Label
             | Cntnts -- ^ Contents (Paragraph, Lists, etc.)

instance Show RefType where
  show Tab    = "Table"
  show Fig    = "Figure"
  show Sect   = "Section"
  show Mod    = "Module"
  show Def    = "Definition"
  show Req    = "Requirement"
  show Assump = "Assumption"
  show LC     = "Likely Change"
  show UC     = "Unlikely Change"
  show Cite   = "Citation"
  show Goal   = "Goal Statement"
  show PSD    = "Physical System Description"
  show EqnB   = "Equation"
  show Lbl    = "Label"
  show Cntnts = "Content"