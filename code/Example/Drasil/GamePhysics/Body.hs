module Drasil.GamePhysics.Body where

import Drasil.Template.MG 
import Drasil.Template.DD
import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.SI_Units


import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Physics (rigidBody, elasticity, cartesian, friction, 
                   rightHand, collision, space)
import Data.Drasil.Concepts.PhysicalProperties (ctrOfMass)
import Data.Drasil.Concepts.Math
import Data.Drasil.Utils (foldle, foldlSent, mkEnumAbbrevList)
import Data.Drasil.Quantities.Physics (restitutionCoef, time)
import Data.Drasil.Quantities.PhysicalProperties (mass, len)

import Drasil.SpecificSystemDescription
import Drasil.OrganizationOfSRS
import qualified Drasil.SRS as SRS
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

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc' mkSRS for' chipmunkSysInfo

mkSRS :: DocDesc
mkSRS = RefSec (RefProg RM.intro [TUnits, tsymb tableOfSymbols, TAandA ]) : 
  map Verbatim [s2, s3, s4, s5, s6, s7]
  where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder]

    --FIXME: Need to be able to print defn for gravitational constant.

chipmunkSysInfo :: SystemInformation
chipmunkSysInfo = SI chipmunk srs authors chipUnits cpSymbols ([] :: [CQSWrapper])
  cpAcronyms --FIXME: All named ideas, not just acronyms.

chipUnits :: [UnitDefn]
chipUnits = map UU [metre, kilogram, second] ++ map UU [newton, radians]

chipmunkMG :: Document
chipmunkMG = mgDoc' chipmunk auths mgBod

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

------------------------------
-- Section : INTRODUCTION --
------------------------------

s2 :: Section
s2_intro :: [Contents]

s2 = SRS.intro ((map Con s2_intro)++[Sub s2_1, Sub s2_2, Sub s2_3])

para1_s2_intro :: Contents
para1_s2_intro = Paragraph $ foldlSent
  [S "Due to the rising cost of developing", (plural videogame) :+: S ",", 
  S "developers are looking for ways to save time and money for",
  S "their projects. Using an open source", (phrase $ physLib ^. term),
  S "that is reliable and free will cut down development costs and lead",
  S "to better quality products"]

para2_s2_intro :: Contents
para2_s2_intro = Paragraph $ foldlSent 
  [S "The following section provides an overview of the",
  titleize srs, (sParen $ getAcc srs), S "for",
  (short chipmunk) `sC` S "an open source", (getAcc twoD), 
  (phrase $ rigidBody ^. term) +:+. (phrase $ physLib ^. term),
  S "This section explains the purpose of this document, the scope",
  S "of the system, and the organization of the document"]
        
s2_intro = [para1_s2_intro, para2_s2_intro]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

s2_1 :: Section
s2_1_intro :: [Contents]

s2_1 = section (titleize prpsOfDoc) (s2_1_intro) []

para1_s2_1_intro :: Contents
para1_s2_1_intro = Paragraph $ foldlSent 
  [S "This document descibes the modeling of an",
  S "open source", getAcc twoD, (phrase $ rigidBody ^. term), 
  (phrase $ physLib ^. term), S "used for games. The", 
  plural goalStmt, S "and", plural thModel, S "used in",
  short chipmunk, S "are provided. This",
  S "document is intended to be used as a reference to provide all",
  S "necessary information to understand and verify the model"]

para2_s2_1_intro :: Contents
para2_s2_1_intro = Paragraph $ foldlSent 
  [S "This document will be used as a starting point for",
  S "subsequent development phases, including writing the design",
  S "specification and the software verification and validation plan.",
  S "The design document will show how the", plural requirement, 
  S "are to be realized.", 
  S "The verification and validation plan will show the steps",
  S "that will be used to increase confidence in the software",
  S "documentation and the implementation"]

s2_1_intro = [para1_s2_1_intro, para2_s2_1_intro]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

s2_2 :: Section
s2_2_intro :: Contents

s2_2 = section (titleize scpOfReq) [s2_2_intro] []

s2_2_intro = Paragraph $ foldlSent 
  [S "The scope of the", plural requirement, S "includes the",
  (phrase $ physicalSim),  S "of", (getAcc twoD), (plural $ rigidBody ^. term),
  S "acted on by forces. Given", (getAcc twoD), 
  (plural $ rigidBody ^. term) `sC` (short chipmunk), 
  S "is intended to simulate how these", (plural $ rigidBody ^. term), 
  S "interact with one another"]

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

s2_3 :: Section
s2_3_intro :: Sentence

s2_3 = orgSec s2_3_intro inModel s4_2_5

-- FIXME: Citations.
-- FIXME: This can probably be completely pulled out is we decide on the 
--  right convention for the intro across examples.
s2_3_intro = foldlSent 
  [S "The organization of this document follows the",
  S "template for an", (getAcc srs), S "for scientific",
  S "computing software proposed by [1] and [2]"]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

s3 :: Section
s3_intro :: Contents

s3 = section (titleize generalSystemDescription) [s3_intro] [s3_1, s3_2]

--FIXME: This can be generalized to use more chunks
s3_intro = Paragraph $ foldlSent 
  [S "This section provides general information",
  S "about the system, identifies the interfaces between the system and",
  S "its environment, and describes the user characteristics and the",
  S "system constraints"]

--------------------------------
-- 3.1 : User Characteristics --
--------------------------------

s3_1 :: Section
s3_1_intro :: Contents

s3_1 = section (titleize' userCharacteristic) [s3_1_intro] []

s3_1_intro = Paragraph $ foldlSent 
  [S "The end user of", (short chipmunk),
  S "should have an understanding of first year programming concepts",
  S "and an understanding of high school physics"]

-------------------------------
-- 3.2 : System Constraints  --
-------------------------------

s3_2 :: Section
s3_2_intro :: Contents

s3_2 = section (titleize' systemConstraint) [s3_2_intro] []

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

s4_1 = section (titleize problemDescription) [s4_1_intro] [s4_1_1, s4_1_2]

s4_1_intro = Paragraph $ foldlSent 
  [S "Creating a gaming", (phrase $ physLib ^. term),
  S "is a difficult task. Games need physics libraries that simulate",
  S "objects acting under various physical conditions, while",
  S "simultaneously being fast and efficient enough to work in soft",
  S "real-time during the game. Developing a", (phrase $ physLib ^. term),
  S "from scratch takes a long period of time and is very costly" `sC`
  S "presenting barriers of entry which make it difficult for game",
  S "developers to include physics in their products. There are a few",
  S "free, open source and high quality physics libraries available to",
  S "be used for consumer products" +:+. (sParen $ makeRef s7),
  S "By creating a simple, lightweight, fast and portable",
  (getAcc twoD), (phrase $ rigidBody ^. term), (phrase $ physLib ^. term) `sC`
  S "game development will be more accessible",
  S "to the masses and higher quality products will be produced"]

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1 :: Section
s4_1_1_intro, s4_1_1_bullets :: Contents

s4_1_1 = section (titleize' (terminology `and_'`  definition))
 [s4_1_1_intro, s4_1_1_bullets] []

s4_1_1_intro = Paragraph $ foldle (+:+) (:+:) (EmptyS) 
  [S "This subsection provides a list of terms",
  S "that are used in subsequent sections and their meaning, with the",
  S "purpose of reducing ambiguity and making it easier to correctly",
  S "understand the", plural requirement, S ":"]

--FIXME: Handle plurals properly. This is a really bad hack.
s4_1_1_bullets = Enumeration (Bullet $ map (termDefn)
  [rigidBody, elasticity, ctrOfMass] ++ [termDefns cartesian] ++ 
  map termDefn [rightHand])
  where termDefn t = Flat (
          (sMap capitalize (phrase $ t ^. term)) :+:
          S ":" +:+ (t ^. defn))
        termDefns t = Flat (
          (sMap capitalize (phrase $ t ^. term)) :+:
          S "s:" +:+ (t ^. defn))

-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

s4_1_2 :: Section
s4_1_2_list :: Contents

s4_1_2 = section (titleize' goalStmt) [s4_1_2_list] []

s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, s4_1_2_stmt4 :: Sentence
s4_1_2_stmt1 = foldlSent 
  [S "Given the physical", S "properties, initial", 
  (plural $ position ^. term), S "and",
  (plural $ vel ^. term) `sC` S "and", (plural $ force ^. term),
  S "applied on a set of", (plural $ rigidBody ^. term) `sC`
  S "determine their new", (plural $ position ^. term), S "and",
  (plural $ vel ^. term), S "over a period of", (phrase $ time ^. term)]

s4_1_2_stmt2 = foldlSent 
  [S "Given the physical", S "properties, initial", 
  (plural $ orientation ^. term), S "and", (plural $ angVel ^. term) `sC`
  S "and", (plural $ force ^. term), S "applied on a set of", 
  (plural $ rigidBody ^. term) `sC` S "determine their new",
  (plural $ orientation ^. term), S "and", (plural $ angVel ^. term), 
  S "over a period of", (phrase $ time ^. term)]

s4_1_2_stmt3 = foldlSent 
  [S "Given the initial", (plural $ position ^. term), S "and", 
  (plural $ vel ^. term), S "of a", S "set of", 
  (plural $ rigidBody ^. term) `sC` S "determine if any of",
  S "them will collide with one another over a period of", 
  (phrase $ time ^. term)]

s4_1_2_stmt4 = foldlSent 
  [S "Given the physical", S "properties, initial linear and angular", 
  (plural $ position ^. term), 
  S "and", (plural $ vel ^. term) `sC` S "determine the new",
  (plural $ position ^. term), S "and", (plural $ vel ^. term),
  S "over a period of", (phrase $ time ^. term), S "of",
  (plural $ rigidBody ^. term), S "that have undergone a", 
  (phrase $ collision ^. term)]

s4_1_2_list' :: [Sentence]
s4_1_2_list' = [s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, s4_1_2_stmt4]

s4_1_2_list = Enumeration (Simple $ mkEnumAbbrevList goalStmt s4_1_2_list')

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

s4_2 :: Section

s4_2 = section (titleize solutionCharSpec) []
 [s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5, s4_2_6]

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

s4_2_1 :: Section
s4_2_1_intro, s4_2_1_list :: Contents

s4_2_1 = section (titleize' assumption) [s4_2_1_intro, s4_2_1_list] []

-- TODO: Add assumption references in the original and this SRS. --
s4_2_1_intro = Paragraph $ foldlSent 
  [S "This section simplifies the original problem",
  S "and helps in developing the theoretical model by filling in the",
  S "missing information for the physical system. The numbers given in",
  S "the square brackets refer to the", 
  foldr1 sC (map (\ch -> (phrase ch) +:+ (bterm ch)) 
  [thModel, genDefn, dataDefn, inModel]) `sC` S "or", 
  phrase likelyChg, (bterm likelyChg) `sC` S "in which the respective",
  (phrase assumption), S "is used"]
  where bterm chunk = S "[" :+: (getAcc chunk) :+: S "]"

s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4, s4_2_1_assum5, 
  s4_2_1_assum6, s4_2_1_assum7 :: Sentence

s4_2_1_assum1 = foldlSent [S "All objects are", (plural $ rigidBody ^. term)]
s4_2_1_assum2 = foldlSent [S "All objects are", (getAcc twoD)]
s4_2_1_assum3 = foldlSent [S "The library uses a", (phrase $ cartesian ^. term), 
  S "system"]
s4_2_1_assum4 = foldlSent [S "The axes are defined using", 
  (phrase $ rightHand ^. term)]
s4_2_1_assum5 = foldlSent [S "All", (plural $ rigidBody ^. term), 
  (plural $ collision ^. term), S "are vertex-to-edge", 
  (plural $ collision ^. term)]
s4_2_1_assum6 = foldlSent [S "There is no damping", 
  S "involved throughout the simulation"]
s4_2_1_assum7 = foldlSent [S "There are no constraints",
  S "and joints involved throughout the simulation"]

s4_2_1_list' :: [Sentence]
s4_2_1_list' = [s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4,
               s4_2_1_assum5, s4_2_1_assum6, s4_2_1_assum7]

s4_2_1_list = Enumeration (Simple $ mkEnumAbbrevList assumption s4_2_1_list')


--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

s4_2_2 :: Section
s4_2_2_intro :: Contents
s4_2_2_TMods :: [Contents]

s4_2_2 = section (titleize' thModel) ([s4_2_2_intro] ++ (s4_2_2_TMods)) []

s4_2_2_intro = Paragraph $ foldlSent 
  [S "This section focuses on the general equations",
  S "the", (phrase $ physLib ^. term), S "is based on"]

s4_2_2_TMods = map Definition (map Theory cpTMods)

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

s4_2_3 :: Section
s4_2_3_intro :: Contents
-- s4_2_3_GDefs :: [Contents]

s4_2_3 = section (titleize' genDefn) ([s4_2_3_intro] {- ++
  (map Con s4_2_3_GDefs)-}) []

s4_2_3_intro = Paragraph $ foldlSent 
  [S "This section collects the laws and equations",
  S "that will be used in deriving the", (plural dataDefn) `sC`
  S "which in turn will be used to build the", (plural inModel)]

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

s4_2_4 = section (titleize' dataDefn) ([s4_2_4_intro] ++
  (s4_2_4_DDefs)) []

s4_2_4_intro = Paragraph $ foldlSent [S "This section collects and defines all the",
  S "data needed to build the" +:+. titleize' inModel,
  S "The dimension of each quantity is also given"]

s4_2_4_DDefs = map Definition (map Data cpDDefs)

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5 :: Section
s4_2_5_intro :: Contents
-- s4_2_5_IMods :: [Contents]

s4_2_5 = section (titleize' inModel) ([s4_2_5_intro] {- ++
  (map Con s4_2_5_IMods)-}) []

s4_2_5_intro = Paragraph $ foldlSent 
  [S "This section transforms the problem defined",
  S "in", (makeRef s4_1), S "into one expressed in mathematical",
  S "terms. It uses concrete symbols defined in", (makeRef s4_2_4),
  S "to replace the abstract symbols in the models identified in",
  (makeRef s4_2_2), S "and", (makeRef s4_2_3)]

-- Instance models not yet implemented --
-- s4_2_5_IMods = ?

------------------------------
-- 4.2.6 : Data Constraints --
------------------------------

s4_2_6 :: Section
s4_2_6_intro, s4_2_6_table1, s4_2_6_table2 :: Contents

s4_2_6 = section (titleize' datumConstraint) [s4_2_6_intro, s4_2_6_table1,
  s4_2_6_table2] []

s4_2_6_intro = Paragraph $ foldlSent 
  [S "Table 1 and 2 show the data constraints on",
  S "the input and output variables, respectively. The",
  (Quote $ titleize' physicalConstraint), S "column gives the physical",
  S "limitations on the range of values that can be taken by the",
  S "variable. The constraints are conservative, to give the user of the",
  S "model the flexibility to experiment with unusual situations. The",
  S "column of typical values is intended to provide a feel for a",
  S "common scenario"]

-- Currently unable to write relations in sentences, so verbal explanations
-- will do for now.
-- How do I write 2pi in constraints?
s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, S "Typical Value"]
  (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] [
  [(P $ len ^. symbol), (P $ len ^. symbol) +:+ S "is G/E to 0", S "44.2" +:+
  (Sy $ unit_symb len)],
  [(P $ mass ^. symbol), (P $ mass ^. symbol) +:+ S "is greater than 0",
  S "56.2" +:+ (Sy $ unit_symb mass)],
  [(P $ momtInert ^. symbol), (P $ momtInert ^. symbol) +:+ S "is G/E to 0",
  S "74.5" +:+ (Sy $ unit_symb momtInert)],
  [(P $ gravAccel ^. symbol), S "None", S "9.8" +:+ (Sy $ unit_symb gravAccel)],
  [(P $ position ^. symbol), S "None", S "(0.412, 0.502)" +:+
  (Sy $ unit_symb position)],
  [(P $ vel ^. symbol), S "None", S "2.51" +:+ (Sy $ unit_symb vel)],
  [(P $ restitutionCoef ^. symbol), (P $ restitutionCoef ^. symbol) +:+ S "G/E to 0 and" +:+
  (P $ restitutionCoef ^. symbol) +:+ S "less than 1", S "0.8"],
  [(P $ orientation ^. symbol), (P $ orientation ^. symbol) +:+ S "G/E to 0" +:+
  S "and" +:+ (P $ orientation ^. symbol) +:+ S "less than 2pi", S "pi/2" +:+
  Sy (unit_symb orientation)],
  [(P $ angVel ^. symbol), S "None", S "2.1" +:+ (Sy $ unit_symb angVel)],
  [(P $ force ^. symbol), S "None", S "98.1" +:+ (Sy $ unit_symb force)],
  [(P $ torque ^. symbol), S "None", S "200" +:+ (Sy $ unit_symb torque)]
  ]) (S "Table 1: Input Variables") True

s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint]
  (mkTable [(\x -> x!!0), (\x -> x!!1)] [
  [(P $ position ^. symbol), S "None"],
  [(P $ vel ^. symbol), S "None"],
  [(P $ orientation ^. symbol), (P $ orientation ^. symbol) +:+ S "G/E to 0" +:+
  S "and" +:+ (P $ orientation ^. symbol) +:+ S "less than 2pi"],
  [(P $ angVel ^. symbol), S "None"]
  ]) (S "Table 2: Output Variables") True

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

s5 :: Section
s5_intro :: Contents

s5 = section (titleize' requirement) [s5_intro] [s5_1, s5_2]

s5_intro = Paragraph $ foldlSent 
  [S "This section provides the functional",
  plural requirement `sC` S "the business",
  S "tasks that the software is expected to complete, and the",
  S "nonfunctional", (plural requirement `sC` 
  S "the qualities that the software is expected to exhibit")]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1_list :: Contents

s5_1 = section (titleize' functionalRequirement)
  [s5_1_list] []

s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
  s5_1_req7, s5_1_req8 :: Sentence

s5_1_req1 = foldlSent [S "Create a", (phrase $ space ^. term), S "for all of the",
  (plural $ rigidBody ^. term), S "in the physical simulation to interact in"]

s5_1_req2 = foldlSent [S "Input the initial", 
  (plural $ mass ^. term) `sC` (plural $ vel ^. term) `sC` 
  (plural $ orientation ^. term) `sC` (plural $ angVel ^. term), 
  S "of" `sC` S "and", (plural $ force ^. term), S "applied on", 
  (plural $ rigidBody ^. term)]

s5_1_req3 = foldlSent [S "Input the", (phrase $ surface ^. term), 
  S "properties of the bodies, such as", (phrase $ friction ^. term), 
  S "or", (phrase $ elasticity ^. term)]

s5_1_req4 = foldlSent [S "Verify that the inputs", 
  S "satisfy the required", plural physicalConstraint]

s5_1_req5 = foldlSent 
  [S "Determine the", (plural $ position ^. term), S "and", (plural $ vel ^. term), 
  S "over a", S "period of", (phrase $ time ^. term), S "of the", (getAcc twoD),
  (plural $ rigidBody ^. term), S "acted upon by a", (phrase $ force ^. term)]

s5_1_req6 = foldlSent
  [S "Determine the", (plural $ orientation ^. term), S "and", 
  (plural $ angVel ^. term), S "over a period of", (phrase $ time ^. term),
   S "of the", (getAcc twoD), (plural $ rigidBody ^. term)]

s5_1_req7 = foldlSent [S "Determine if any of the", (plural $ rigidBody ^. term), 
  S "in the", (phrase $ space ^. term), S "have collided"]

s5_1_req8 = foldlSent
  [S "Determine the", (plural $ position ^. term), S "and", (plural $ vel ^. term), 
  S "over a", S "period of", (phrase $ time ^. term), S "of the", (getAcc twoD), 
  (plural $ rigidBody ^. term), S "that have undergone a", 
  (phrase $ collision ^. term)]

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
s5_1_list' :: [Sentence]
s5_1_list' = [s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
            s5_1_req7, s5_1_req8]

s5_1_list = Enumeration (Simple $ mkEnumAbbrevList requirement s5_1_list')

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

s5_2 :: Section
s5_2_intro :: Contents

s5_2 = section (titleize' nonfunctionalRequirement) [s5_2_intro] []

s5_2_intro = Paragraph $ foldlSent 
  [S "Games are resource intensive, so performance",
  S "is a high priority. Other non-functional", plural requirement,
  S "that are a",
  S "priority are: correctness, understandability, portability,",
  S "reliability, and maintainability"]

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6_intro, s6_list :: Contents

s6 = section (titleize' likelyChg) [s6_intro, s6_list] []

s6_intro = Paragraph $ S "This section lists the" +:+. 
  ((plural likelyChg) +:+ S "to be made to the physics game library")

s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3, 
  s6_likelyChg_stmt4 :: Sentence

s6_likelyChg_stmt1 = S "The internal" +:+ (getAcc ode) :+: 
  S "-solving algorithm used by the library may" +:+. S "change in the future"

s6_likelyChg_stmt2 = S "The library may be" +:+
  S "expanded to deal with edge-to-edge and vertex-to-vertex" +:+.
  (plural (collision ^. term))

s6_likelyChg_stmt3 = S "The library may be" +:+. 
  S "expanded to include motion with damping"

s6_likelyChg_stmt4 = S "The library may be" +:+.
  S "expanded to include joints and constraints"

s6_list' :: [Sentence]
s6_list' = [s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3,
                    s6_likelyChg_stmt4]

s6_list = Enumeration (Simple $ mkEnumAbbrevList likelyChg s6_list')


-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

s7 :: Section
s7_intro, s7_2dlist, s7_mid, s7_3dlist :: Contents

s7 = section (titleize' offShelfSolution) [s7_intro, s7_2dlist,
  s7_mid, s7_3dlist] []

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
