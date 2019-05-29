{-# Language GADTs #-}
module Drasil.Sections.SolutionCharacterSpec
  (
  SecItem,
  SubSec,
  sSubSec,
  siCon,
  siSect,
  siTMod,
  siIMod,
  siDDef,
  siSent,
  siSTitl,
  siCC,
  siUQI,
  siUQO
  ) where

import Language.Drasil
import Theory.Drasil (DataDefinition)

data SecItem where 
  Cont      :: [Contents] -> SecItem
  Sect      :: [Section] -> SecItem
  TMods     :: [RelationConcept] -> SecItem
  IMods     :: [RelationConcept] -> SecItem
  DataDef   :: [DataDefinition] -> SecItem
  --GenDef    :: [RelationConcept] -> SecItem
  ConChunk  :: [ConceptChunk] -> SecItem
  Sent      :: [Sentence] -> SecItem
  UnQuantI  :: [UncertQ] -> SecItem
  UnQuantO  :: [UncertQ] -> SecItem 
  SingularTitle :: SecItem


data SubSec where
  SectionModel :: NamedIdea c => c -> [SecItem] -> SubSec

sSubSec :: (NamedIdea c) => c -> [SecItem] -> SubSec
sSubSec = SectionModel

--------------------------
-- SECITEM CONSTRUCTORS --
--------------------------

siCon :: [Contents] -> SecItem
siCon = Cont

siSect :: [Section] -> SecItem
siSect = Sect

siTMod :: [RelationConcept] -> SecItem
siTMod = TMods

siIMod :: [RelationConcept] -> SecItem
siIMod = IMods

siDDef :: [DataDefinition] -> SecItem
siDDef = DataDef

siSent :: [Sentence] -> SecItem
siSent = Sent

siSTitl :: SecItem
siSTitl = SingularTitle

siCC :: [ConceptChunk] -> SecItem
siCC = ConChunk

siUQI :: [UncertQ] -> SecItem
siUQI = UnQuantI

siUQO :: [UncertQ] -> SecItem
siUQO = UnQuantO

------------------------------------------------
-- GENERAL SYSTEM DESCRIPTION SECTION BUILDER --
------------------------------------------------

-------------------------------------------------
-- Specific System Description SECTION BUILDER --
-------------------------------------------------

-----------------------------------------------------------
-- Solution Characteristic Specification SECTION BUILDER --
-----------------------------------------------------------

--------------------------------------------
-- CONTENT BUILDING FUNCTIONS & CONSTANTS --
--------------------------------------------

--------------------------------
-- GENERAL SYSTEM DESCRIPTION --
--------------------------------


--------------------------
-- USER CHARACTERISTICS --
--------------------------


------------------------
-- SYSTEM CONSTRAINTS --
------------------------

---------------------------------
-- SPECIFIC SYSTEM DESCRIPTION --
---------------------------------

-------------------------
-- PROBLEM DESCRIPTION --
-------------------------

--------------------------
-- TERM AND DEFINITIONS --
--------------------------

--------------------
-- GOAL STATEMENT --
--------------------

-------------------------------------------
-- SOLUTION CHARACTERISTIC SPECIFICATION --
-------------------------------------------

-----------------
-- ASSUMPTIONS --
-----------------

------------------------
-- THEORETICAL MODELS --
------------------------

-------------------------
-- GENERAL DEFINITIONS --
-------------------------

----------------------
-- DATA DEFINITIONS --
----------------------

---------------------
-- INSTANCE MODELS --
---------------------
  
---------------------
-- DATA CONSTRAINTS --
---------------------

------------------
-- REQUIREMENTS --
------------------

---------------------------------
-- NON-FUNCTIONAL REQUIREMENTS --
---------------------------------


-----------------------------
-- FUNCTIONAL REQUIREMENTS --
-----------------------------
