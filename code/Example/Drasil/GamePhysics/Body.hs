module Drasil.GamePhysics.Body where

import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.SI_Units

import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Physics (rigidBody, elasticity, cartesian, friction, 
                   rightHand, collision, space, surface)
import Data.Drasil.Concepts.PhysicalProperties (ctrOfMass)
import Data.Drasil.Concepts.Math

import Data.Drasil.Quantities.Physics (restitutionCoef)

import Drasil.TableOfSymbols
import Drasil.TableOfUnits
import Drasil.TableOfAbbAndAcronyms
import Drasil.SpecificSystemDescription
import Drasil.OrganizationOfSRS
import Drasil.SRS
import qualified Drasil.ReferenceMaterial as RM

import Drasil.GamePhysics.Unitals
import Drasil.GamePhysics.Concepts
import Drasil.GamePhysics.TMods
import Drasil.GamePhysics.DataDefs

import Drasil.GamePhysics.Modules
import Drasil.GamePhysics.Changes
import Drasil.GamePhysics.Reqs

import Drasil.DocumentLanguage

authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = manyNames authors

chipmunkSRS :: Document
chipmunkSRS = srsDoc chipmunk auths [s1, s2, s3, s4, s5, s6, s7]

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc mkSRS chipmunkSysInfo

mkSRS :: DocDesc
mkSRS = RefSec (RefProg RM.intro [ TUnits, tsymb s1_2_intro, TAandA ]) : 
  map Verbatim [s2, s3, s4, s5, s6, s7]
  
chipmunkSysInfo :: SystemInformation
chipmunkSysInfo = SI chipmunk srs authors chipUnits cpSymbols ([] :: [ConVar])
  cpAcronyms --FIXME: All named ideas, not just acronyms.

chipUnits :: [UnitDefn]
chipUnits = map UU [metre, kilogram, second] ++ map UU [newton, radians]

chipmunkMG :: Document
chipmunkMG = mgDoc chipmunk auths mgBod

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

--FIXME: The SRS has been partly switched over to the new docLang, so some of
-- the sections below are now redundant. I have not removed them yet, because
-- it makes it easier to test between the two different versions as changes
-- are introduced. Once the SRS is entirely switched to docLang, the redundant
-- sections should be removed.


-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------------
-- Section : REFERENCE MATERIAL --
------------------------------------

s1 :: Section
s1 = RM.refSec [s1_1, s1_2, s1_3]

--------------------------
-- 1.1 : Table of Units --
--------------------------

-- should be computed!
s1_1 :: Section
s1_1 = table_of_units $ map UU [metre, kilogram, second] ++ map UU [newton, radians]

----------------------------
-- 1.2 : Table of Symbols --
----------------------------

s1_2 :: Section
s1_2_intro, s1_2_table :: Contents

s1_2 = Section (S "Table of Symbols") [Con s1_2_intro, Con s1_2_table]

s1_2_intro = Paragraph $
  S "The table that follows summarizes the symbols used in this" +:+
  S "document along with their units.  More specific instances of these" +:+
  S "symbols will be described in their respective sections. Throughout" +:+
  S "the document, symbols in bold will represent vectors, and scalars" +:+.
  S "otherwise. The symbols are listed in alphabetical order"

s1_2_table = table cpSymbols (^.term)

--------------------------------------
-- 1.3 : Abbreviations and Acronyms --
--------------------------------------

s1_3 :: Section

s1_3 = table_of_abb_and_acronyms cpAcronyms

------------------------------
-- Section : INTRODUCTION --
------------------------------

s2 :: Section
s2_intro :: [Contents]

s2 = Section (S "Introduction") ((map Con s2_intro)++[Sub s2_1, Sub s2_2,
  Sub s2_3])

s2_intro = [Paragraph (S "Due to the rising cost of developing video" +:+
  S "games, developers are looking for ways to save time and money for" +:+
  S "their projects. Using an open source" +:+ (physLib ^. term) +:+
  S "that is reliable and free will cut down development costs and lead" +:+.
  S "to better quality products"),
  Paragraph (S "The following section provides an overview of the" +:+
  (srs ^. term) +:+ sParen (getAcc srs) +:+ S "for" +:+
  (chipmunk ^. term) `sC` S "an open source" +:+ (getAcc twoD) +:+ 
  (rigidBody ^. term) +:+. (physLib ^. term) +:+
  S "This section explains the purpose of this document, the scope" +:+.
  S "of the system, and the organization of the document")]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

s2_1 :: Section
s2_1_intro :: [Contents]

s2_1 = Section (S "Purpose of Document") (map Con s2_1_intro)

s2_1_intro = [Paragraph (S "This document descibes the modeling of an" +:+
  S "open source" +:+ (getAcc twoD) +:+ (rigidBody ^. term) +:+ 
  (physLib ^. term) +:+ S "used for games. The" +:+ 
  (addS (sLower (goalStmt ^. term))) +:+ S "and" +:+ 
  (addS (sLower (thModel ^. term))) +:+ S "used in" +:+ (chipmunk ^. term) +:+
  S "are provided. This" +:+
  S "document is intended to be used as a reference to provide all" +:+.
  S "necessary information to understand and verify the model"),
  Paragraph (S "This document will be used as a starting point for" +:+
  S "subsequent development phases, including writing the design" +:+.
  S "specification and the software verification and validation plan" +:+
  S "The design document will show how the" +:+
  (addS (sLower (requirement ^. term))) +:+ S "are to be" +:+
  S "realized. The verification and validation plan will show the steps" +:+
  S "that will be used to increase confidence in the software" +:+.
  S "documentation and the implementation")]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

s2_2 :: Section
s2_2_intro :: Contents

s2_2 = Section (S "Scope of" +:+ addS (requirement ^. term))
  [Con s2_2_intro]

s2_2_intro = Paragraph $ S "The scope of the" +:+
  (addS (sLower (requirement ^. term))) +:+ S "includes the" +:+
  S "physical simulation of" +:+ (getAcc twoD) +:+ 
  irregPlur (rigidBody ^. term) +:+ S "acted on by forces. Given" +:+ 
  (getAcc twoD) +:+ irregPlur (rigidBody ^. term) `sC` (chipmunk ^. term) +:+ 
  S "is intended to simulate how these" +:+ irregPlur (rigidBody ^. term) +:+. 
  S "interact with one another"

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

s2_3 :: Section
s2_3_intro :: Sentence

s2_3 = orgSec s2_3_intro inModel s4_2_5

-- FIXME: Citations.
-- FIXME: This can probably be completely pulled out is we decide on the 
--  right convention for the intro across examples.
s2_3_intro = S "The organization of this document follows the" +:+
  S "template for an" +:+ (getAcc srs) +:+ S "for scientific" +:+.
  S "computing software proposed by [1] and [2]"

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

s3 :: Section
s3_intro :: Contents

s3 = Section (S "General System Description") [Con s3_intro, Sub s3_1,
  Sub s3_2]

--FIXME: This can be generalized to use more chunks
s3_intro = Paragraph $ S "This section provides general information" +:+
  S "about the system, identifies the interfaces between the system and" +:+
  S "its environment, and describes the user characteristics and the" +:+.
  S "system constraints"

--------------------------------
-- 3.1 : User Characteristics --
--------------------------------

s3_1 :: Section
s3_1_intro :: Contents

s3_1 = Section (S "User Characteristics") [Con s3_1_intro]

s3_1_intro = Paragraph $ S "The end user of" +:+ (chipmunk ^. term) +:+
  S "should have an understanding of first year programming concepts" +:+.
  S "and an understanding of high school physics"

-------------------------------
-- 3.2 : System Constraints  --
-------------------------------

s3_2 :: Section
s3_2_intro :: Contents

s3_2 = Section (S "System Constraints") [Con s3_2_intro]

s3_2_intro = Paragraph $ S "There are no system constraints."

---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

s4 :: Section

s4 = specSysDescr physLib [s4_1, s4_2]

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1 :: Section
s4_1_intro :: Contents

s4_1 = Section (S "Problem Description") [Con s4_1_intro, Sub s4_1_1,
  Sub s4_1_2]

s4_1_intro = Paragraph $ S "Creating a gaming" +:+ (physLib ^. term) +:+
  S "is a difficult task. Games need physics libraries that simulate" +:+
  S "objects acting under various physical conditions, while" +:+
  S "simultaneously being fast and efficient enough to work in soft" +:+
  S "real-time during the game. Developing a" +:+ (physLib ^. term) +:+
  S "from scratch takes a long period of time and is very costly" `sC`
  S "presenting barriers of entry which make it difficult for game" +:+
  S "developers to include physics in their products. There are a few" +:+
  S "free, open source and high quality physics libraries available to" +:+
  S "be used for consumer products" +:+ sParen (makeRef s7) :+:
  S ". By creating a simple, lightweight, fast and portable" +:+
  (getAcc twoD) +:+ (rigidBody ^. term) +:+ (physLib ^. term) `sC` 
  S "game development will be more accessible" +:+.
  S "to the masses and higher quality products will be produced"

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1 :: Section
s4_1_1_intro, s4_1_1_bullets :: Contents

s4_1_1 = Section (S "Terminology and Definitions") [Con s4_1_1_intro,
  Con s4_1_1_bullets]

s4_1_1_intro = Paragraph $ S "This subsection provides a list of terms" +:+
  S "that are used in subsequent sections and their meaning, with the" +:+
  S "purpose of reducing ambiguity and making it easier to correctly" +:+
  S "understand the requirements:"

--FIXME: Handle plurals properly. This is a really bad hack.
s4_1_1_bullets = Enumeration (Bullet $ map (termDefn)
  [rigidBody, elasticity, ctrOfMass] ++ [termDefns cartesian] ++ 
  map termDefn [rightHand])
  where termDefn t = Flat (
          (sMap capitalize (t ^. term)) :+:
          S ":" +:+ (t ^. defn))
        termDefns t = Flat (
          (sMap capitalize (t ^. term)) :+:
          S "s:" +:+ (t ^. defn))

-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

s4_1_2 :: Section
s4_1_2_list :: Contents

s4_1_2 = Section (addS (goalStmt ^. term)) [Con s4_1_2_list]

s4_1_2_list = Enumeration (Simple [
  ((getAcc goalStmt) :+: S "1", Flat (S "Given the physical" +:+
  S "properties, initial" +:+ addS (position ^. term) +:+ S "and" +:+
  irregPlur (vel ^. term) `sC` S "and" +:+ addS (force ^. term) +:+
  S "applied on a set of" +:+ irregPlur (rigidBody ^. term) `sC`
  S "determine their new" +:+ addS (position ^. term) +:+ S "and" +:+
  irregPlur (vel ^. term) +:+ S "over a period of" +:+. (time ^. term))),
--
  ((getAcc goalStmt) :+: S "2", Flat (S "Given the physical" +:+
  S "properties, initial" +:+ addS (orientation ^. term) +:+ S "and" +:+
  irregPlur (angVel ^. term) `sC` S "and" +:+ addS (force ^. term) +:+
  S "applied on a set of" +:+ irregPlur (rigidBody ^. term) `sC`
  S "determine their new" +:+ addS (orientation ^. term) +:+ S "and" +:+
  irregPlur (angVel ^. term) +:+ S "over a period of" +:+. (time ^. term))),
--
  ((getAcc goalStmt) :+: S "3", Flat (S "Given the initial" +:+
  addS (position ^. term) +:+ S "and" +:+ irregPlur (vel ^. term) +:+ S "of a" +:+
  S "set of" +:+ irregPlur (rigidBody ^. term) `sC` S "determine if any of" +:+
  S "them will collide with one another over a period of" +:+. (time ^. term))),
--
  ((getAcc goalStmt) :+: S "4", Flat (S "Given the physical" +:+
  S "properties, initial linear and angular" +:+ addS (position ^. term) +:+
  S "and" +:+ irregPlur (vel ^. term) `sC` S "determine the new" +:+
  addS (position ^. term) +:+ S "and" +:+ irregPlur (vel ^. term) +:+
  S "over a period of" +:+ (time ^. term) +:+ S "of" +:+
  irregPlur (rigidBody ^. term) +:+ S "that have undergone a" +:+. (collision ^. term)))
  ])

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

s4_2 :: Section

s4_2 = Section (S "Solution Characteristics Specification") [Sub s4_2_1,
  Sub s4_2_2, Sub s4_2_3, Sub s4_2_4, Sub s4_2_5, Sub s4_2_6]

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

s4_2_1 :: Section
s4_2_1_intro, s4_2_1_list :: Contents

s4_2_1 = Section (addS $ assumption ^. term) [Con s4_2_1_intro,
  Con s4_2_1_list]

-- TODO: Add assumption references in the original and this SRS. --
s4_2_1_intro = Paragraph $ S "This section simplifies the original problem" +:+
  S "and helps in developing the theoretical model by filling in the" +:+
  S "missing information for the physical system. The numbers given in" +:+
  S "the square brackets refer to the" +:+ foldr1 sC 
  (map (\ch -> (sLower (ch ^. term)) +:+ (bterm ch)) 
  [thModel, genDefn, dataDefn, inModel]) `sC` S "or" +:+ 
  (sLower $ likelyChg ^. term) +:+ (bterm likelyChg) `sC` 
  S "in which the respective" +:+ (sLower $ assumption ^. term) +:+. S "is used"
  where bterm chunk = S "[" :+: (getAcc chunk) :+: S "]"

s4_2_1_list = Enumeration (Simple [
  ((getAcc assumption) :+: S "1", Flat (S "All objects are" +:+.
  irregPlur (rigidBody ^. term))),
  ((getAcc assumption) :+: S "2", Flat (S "All objects are" +:+.
  (getAcc twoD))),
  ((getAcc assumption) :+: S "3", Flat (S "The library uses a" +:+
  (cartesian ^. term) +:+. S "system")),
  ((getAcc assumption) :+: S "4", Flat (S "The axes are defined using" +:+.
  (rightHand ^. term))),
  ((getAcc assumption) :+: S "5", Flat (S "All" +:+
  irregPlur (rigidBody ^. term) +:+ addS (collision ^. term) +:+
  S "are vertex-to-edge" +:+. (addS (collision ^. term)))),
  ((getAcc assumption) :+: S "6", Flat (S "There is no damping" +:+.
  S "involved throughout the simulation")),
  ((getAcc assumption) :+: S "7", Flat (S "There are no constraints" +:+.
  S "and joints involved throughout the simulation"))])

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

s4_2_2 :: Section
s4_2_2_intro :: Contents
s4_2_2_TMods :: [Contents]

s4_2_2 = Section (addS (thModel ^. term)) ([Con s4_2_2_intro] ++
  (map Con s4_2_2_TMods))

s4_2_2_intro = Paragraph $ S "This section focuses on the general equations" +:+
  S "the" +:+ (physLib ^. term) +:+. S "is based on"

s4_2_2_TMods = map Definition (map Theory cpTMods)

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

s4_2_3 :: Section
s4_2_3_intro :: Contents
-- s4_2_3_GDefs :: [Contents]

s4_2_3 = Section ( addS (genDefn ^. term)) ([Con s4_2_3_intro] {- ++
  (map Con s4_2_3_GDefs)-})

s4_2_3_intro = Paragraph $ S "This section collects the laws and equations" +:+
  S "that will be used in deriving the" +:+ addS (sLower (dataDefn ^. term)) `sC`
  S "which in turn will be used to build the" +:+.
  (addS (sLower (inModel ^. term)))

-- GDefs not yet implemented --
{-
s4_2_3_GDefs :: [Contents]
s4_2_3_GDefs = map Definition (map General gDefs)
-}

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

s4_2_4 :: Section
s4_2_4_intro :: Contents
s4_2_4_DDefs :: [Contents]

s4_2_4 = Section (addS (dataDefn ^. term)) ([Con s4_2_4_intro] ++
  (map Con s4_2_4_DDefs))

s4_2_4_intro = Paragraph $ S "This section collects and defines all the" +:+
  S "data needed to build the" +:+ (inModel ^. term) :+: S "s. The" +:+.
  S "dimension of each quantity is also given"

s4_2_4_DDefs = map Definition (map Data cpDDefs)

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5 :: Section
s4_2_5_intro :: Contents
-- s4_2_5_IMods :: [Contents]

s4_2_5 = Section (addS (inModel ^. term)) ([Con s4_2_5_intro] {- ++
  (map Con s4_2_5_IMods)-})

s4_2_5_intro = Paragraph $ S "This section transforms the problem defined" +:+
  S "in" +:+ (makeRef s4_1) +:+ S "into one expressed in mathematical" +:+
  S "terms. It uses concrete symbols defined in" +:+ (makeRef s4_2_4) +:+
  S "to replace the abstract symbols in the models identified in" +:+
  (makeRef s4_2_2) +:+ S "and" +:+. (makeRef s4_2_3)

-- Instance models not yet implemented --
-- s4_2_5_IMods = ?

------------------------------
-- 4.2.6 : Data Constraints --
------------------------------

s4_2_6 :: Section
s4_2_6_intro, s4_2_6_table1, s4_2_6_table2 :: Contents

s4_2_6 = Section (S "Data Constraints") [Con s4_2_6_intro, Con s4_2_6_table1,
  Con s4_2_6_table2]

s4_2_6_intro = Paragraph $ S "Table 1 and 2 show the data constraints on" +:+
  S "the input and output variables, respectively. The" +:+
  (Quote $ S "Physical Constraints") +:+ S "column gives the physical" +:+
  S "limitations on the range of values that can be taken by the" +:+
  S "variable. The constraints are conservative, to give the user of the" +:+
  S "model the flexibility to experiment with unusual situations. The" +:+
  S "column of typical values is intended to provide a feel for a" +:+.
  S "common scenario"

-- Currently unable to write relations in sentences, so verbal explanations
-- will do for now.
-- How do I write 2pi in constraints?
s4_2_6_table1 = Table [S "Var", S "Physical Constraints", S "Typical Value"]
  (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] [
  [P (len ^. symbol), P (len ^. symbol) +:+ S "is G/E to 0", S "44.2" +:+
  Sy (len ^. unit)],
  [P (mass ^. symbol), P (mass ^. symbol) +:+ S "is greater than 0",
  S "56.2" +:+ Sy (mass ^. unit)],
  [P (momtInert ^. symbol), P (momtInert ^. symbol) +:+ S "is G/E to 0",
  S "74.5" +:+ Sy (momtInert ^. unit)],
  [P (gravAccel ^. symbol), S "None", S "9.8" +:+ Sy (gravAccel ^. unit)],
  [P (position ^. symbol), S "None", S "(0.412, 0.502)" +:+
  Sy (position ^. unit)],
  [P (vel ^. symbol), S "None", S "2.51" +:+ Sy (vel ^. unit)],
  [P (restitutionCoef ^. symbol), P (restitutionCoef ^. symbol) +:+ S "G/E to 0 and" +:+
  P (restitutionCoef ^. symbol) +:+ S "less than 1", S "0.8"],
  [P (orientation ^. symbol), P (orientation ^. symbol) +:+ S "G/E to 0" +:+
  S "and" +:+ P (orientation ^. symbol) +:+ S "less than 2pi", S "pi/2" +:+
  Sy (orientation ^. unit)],
  [P (angVel ^. symbol), S "None", S "2.1" +:+ Sy (angVel ^. unit)],
  [P (force ^. symbol), S "None", S "98.1" +:+ Sy (force ^. unit)],
  [P (torque ^. symbol), S "None", S "200" +:+ Sy (torque ^. unit)]
  ]) (S "Table 1: Input Variables") True

s4_2_6_table2 = Table [S "Var", S "Physical Constraints"]
  (mkTable [(\x -> x!!0), (\x -> x!!1)] [
  [P (position ^. symbol), S "None"],
  [P (vel ^. symbol), S "None"],
  [P (orientation ^. symbol), P (orientation ^. symbol) +:+ S "G/E to 0" +:+
  S "and" +:+ P (orientation ^. symbol) +:+ S "less than 2pi"],
  [P (angVel ^. symbol), S "None"]
  ]) (S "Table 2: Output Variables") True

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

s5 :: Section
s5_intro :: Contents

s5 = Section (addS $ requirement ^. term) [Con s5_intro, Sub s5_1,
  Sub s5_2]

s5_intro = Paragraph $ S "This section provides the functional" +:+
  addS (sLower (requirement ^. term)) `sC` S "the business" +:+
  S "tasks that the software is expected to complete, and the" +:+
  S "nonfunctional" +:+. (addS (sLower (requirement ^. term)) `sC`
  S "the qualities that the software is expected to exhibit")

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1_list :: Contents

s5_1 = Section (S "Functional" +:+ addS (requirement ^. term))
  [Con s5_1_list]

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
s5_1_list = Enumeration (Simple [
  ((getAcc requirement) :+: S "1", Flat (S "Create a" +:+
  (space ^. term) +:+ S "for all of the" +:+ irregPlur (rigidBody ^. term) +:+.
  S "in the physical simulation to interact in")),
--
  ((getAcc requirement) :+: S "2", Flat (S "Input the initial" +:+
  addES (mass ^. term) `sC` irregPlur (vel ^. term) `sC` addS (orientation ^. term) `sC`
  irregPlur (angVel ^. term) +:+ S "of" `sC` S "and" +:+ addS (force ^. term) +:+ 
  S "applied on" +:+. irregPlur (rigidBody ^. term))),
--
  ((getAcc requirement) :+: S "3", Flat (S "Input the" +:+ 
  (surface ^. term) +:+ S "properties of the bodies, such as" +:+ 
  (friction ^. term) +:+ S "or" +:+. (elasticity ^. term))),
--
  ((getAcc requirement) :+: S "4", Flat (S "Verify that the inputs" +:+.
  S "satisfy the required physical constraints")),
--
  ((getAcc requirement) :+: S "5", Flat (S "Determine the" +:+
  addS (position ^. term) +:+ S "and" +:+ irregPlur (vel ^. term) +:+ S "over a" +:+
  S "period of" +:+ (time ^. term) +:+ S "of the" +:+ (getAcc twoD) +:+ 
  irregPlur (rigidBody ^. term) +:+ S "acted upon by a" +:+. (force ^. term))),
--
  ((getAcc requirement) :+: S "6", Flat (S "Determine the" +:+
  addS (orientation ^. term) +:+ S "and" +:+ irregPlur (angVel ^. term) :+:
  S " over a period of" +:+ (time ^. term) +:+ S "of the" +:+
  (getAcc twoD) +:+. irregPlur (rigidBody ^. term))),
--
  ((getAcc requirement) :+: S "7", Flat (S "Determine if any of the" +:+
  irregPlur (rigidBody ^. term) +:+ S "in the" +:+ (space ^. term) +:+.
  S "have collided")),
--
  ((getAcc requirement) :+: S "8", Flat (S "Determine the" +:+
  addS (position ^. term) +:+ S "and" +:+ irregPlur (vel ^. term) +:+ S "over a" +:+
  S "period of" +:+ (time ^. term) +:+ S "of the" +:+ (getAcc twoD) +:+ 
  irregPlur (rigidBody ^. term) +:+ S "that have undergone a" +:+. (collision ^. term)))
  ])

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

s5_2 :: Section
s5_2_intro :: Contents

s5_2 = Section (S "Nonfunctional" +:+ addS (requirement ^. term))
  [Con s5_2_intro]

s5_2_intro = Paragraph $ S "Games are resource intensive, so performance" +:+
  S "is a high priority. Other non-functional requirements that are a" +:+
  S "priority are: correctness, understandability, portability," +:+.
  S "reliability, and maintainability"

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6_intro, s6_list :: Contents

s6 = Section (addS (likelyChg ^. term)) [Con s6_intro, Con s6_list]

s6_intro = Paragraph $ S "This section lists the" +:+. (addS (sLower
  (likelyChg ^. term)) +:+ S "to be made to the physics game library")

s6_list = Enumeration (Simple [
  ((getAcc likelyChg) :+: S "1", Flat (S "The internal" +:+
  (getAcc ode) :+: S "-solving algorithm used by the library may" +:+.
  S "change in the future")),
  ((getAcc likelyChg) :+: S "2", Flat (S "The library may be" +:+
  S "expanded to deal with edge-to-edge and vertex-to-vertex" +:+.
  (addS (collision ^. term)))),
  ((getAcc likelyChg) :+: S "3", Flat (S "The library may be" +:+.
  S "expanded to include motion with damping")),
  ((getAcc likelyChg) :+: S "4", Flat (S "The library may be" +:+.
  S "expanded to include joints and constraints"))])

-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

s7 :: Section
s7_intro, s7_2dlist, s7_mid, s7_3dlist :: Contents

s7 = Section (S "Off-the-Shelf Solutions") [Con s7_intro, Con s7_2dlist,
  Con s7_mid, Con s7_3dlist]

s7_intro = Paragraph $ S "As mentioned in" +:+. ((makeRef s4_1) `sC`
  S "there already exist free open source game physics libraries") +:+
  S "Similar" +:+ (getAcc twoD) +:+ S "physics libraries are:"

s7_2dlist = Enumeration (Bullet [
  Flat (S "Box2D: http://box2d.org/"),
  Flat (S "Nape Physics Engine: http://napephys.com/")])

s7_mid = Paragraph $ S "Free open source 3D game physics libraries include:"

s7_3dlist = Enumeration (Bullet [
  Flat (S "Bullet: http://bulletphysics.org/"),
  Flat (S "Open Dynamics Engine: http://www.ode.org/"),
  Flat (S "Newton Game Dynamics: http://newtondynamics.com/")])

----------------
-- REFERENCES --
----------------

-- To be added --
