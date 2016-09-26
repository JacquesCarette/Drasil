module Drasil.GamePhysics.ChipmunkBody where

import Data.Char (toLower, toUpper)
import Data.List (intersperse)
import Control.Lens ((^.))

import Language.Drasil

import Drasil.Units

import Drasil.TableOfSymbols

import Drasil.GamePhysics.ChipmunkUnits
import Drasil.GamePhysics.ChipmunkUnitals
import Drasil.GamePhysics.ChipmunkConcepts
import Drasil.GamePhysics.ChipmunkTMods
import Drasil.GamePhysics.ChipmunkDataDefs

import Drasil.GamePhysics.ChipmunkModules
import Drasil.GamePhysics.ChipmunkChanges
import Drasil.GamePhysics.ChipmunkReqs

chipmunkSRS :: Document
chipmunkSRS = Document
    (S "Software Requirements Specification for " :+: S (chipmunk ^. name))
    (S "Alex Halliwushka and Luthfi Mawarid")
    [s1, s2, s3, s4, s5, s6, s7]

chipmunkMG :: Document
chipmunkMG = Document (S "Module Guide for " :+: S (chipmunk ^. name))
    (S "Alex Halliwushka and Luthfi Mawarid") (mgBod)

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------------
-- Section : REFERENCE MATERIAL --
------------------------------------

s1 :: Section
s1_intro :: Contents

s1 = Section (S "Reference Material") [Con s1_intro, Sub s1_1, Sub s1_2]

s1_intro = Paragraph $ S "This section records information for easy reference."

--------------------------
-- 1.1 : Table of Units --
--------------------------

s1_1 :: Section
s1_1 = table_of_units cpSIUnits

----------------------------
-- 1.2 : Table of Symbols --
----------------------------

s1_2 :: Section
s1_2_intro, s1_2_table :: Contents

s1_2 = Section (S "Table of Symbols") [Con s1_2_intro, Con s1_2_table]

s1_2_intro = Paragraph $
    S "The table that follows summarizes the symbols used in this " :+:
    S "document along with their units.  More specific instances of these " :+:
    S "symbols will be described in their respective sections. Throughout " :+:
    S "the document, symbols in bold will represent vectors, and scalars " :+:
    S "otherwise. The symbols are listed in alphabetical order."

s1_2_table = table cpSymbols

--------------------------------------
-- 1.3 : Abbreviations and Acronyms --
--------------------------------------

s1_3 :: Section
s1_3_table :: Contents

s1_3 = Section (S "Abbreviations and Acronyms") [Con s1_3_table]

s1_3_table = Table [S "Symbol", S "Description"] (mkTable
    [(\ch -> S $ ch ^. name),
    (\ch -> ch ^. descr)]
    cpAcronyms)
    (S "Abbreviations and Acronyms") False

------------------------------
-- Section : INTRODUCTION --
------------------------------

s2 :: Section
s2_intro :: [Contents]

s2 = Section (S "Introduction") ((map Con s2_intro)++[Sub s2_1, Sub s2_2,
    Sub s2_3])

s2_intro = [Paragraph (S "Due to the rising cost of developing video " :+:
    S "games, developers are looking for ways to save time and money for " :+:
    S "their projects. Using an open source " :+: S (physLib ^. name) :+:
    S " that is reliable and free will cut down development costs and lead " :+:
    S "to better quality products."),
    Paragraph (S "The following section provides an overview of the " :+:
    (srs ^. descr) :+: S " (" :+: S (srs ^. name) :+: S ") for " :+:
    S (chipmunk ^. name) :+: S ", an open source " :+: S (twoD ^. name) :+:
    S " " :+: S (rigidBody ^. name) :+: S " " :+: S (physLib ^. name) :+:
    S ". This section explains the purpose of this document, the scope " :+:
    S "of the system, and the organization of the document.")]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

s2_1 :: Section
s2_1_intro :: [Contents]

s2_1 = Section (S "Purpose of Document") (map Con s2_1_intro)

s2_1_intro = [Paragraph (S "This document descibes the modeling of an " :+:
    S "open source " :+: S (twoD ^. name) :+: S " " :+:
    S (rigidBody ^. name) :+: S " " :+: S (physLib ^. name) :+:
    S " used for games. The " :+: (sMap (map toLower) (goalStmt ^. descr)) :+:
    S "s and " :+: (sMap (map toLower) (theoMod ^. descr)) :+:
    S "s used in " :+: S (chipmunk ^. name) :+: S " are provided. This " :+:
    S "document is intended to be used as a reference to provide all " :+:
    S "necessary information to understand and verify the model."),
    Paragraph (S "This document will be used as a starting point for " :+:
    S "subsequent development phases, including writing the design " :+:
    S "specification and the software verification and validation plan. " :+:
    S "The design document will show how the " :+:
    (sMap (map toLower) (requirement ^. descr)) :+: S "s are to be " :+:
    S "realized. The verification and validation plan will show the steps " :+:
    S "that will be used to increase confidence in the software " :+:
    S "documentation and the implementation.")]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

s2_2 :: Section
s2_2_intro :: Contents

s2_2 = Section (S "Scope of " :+: (requirement ^. descr) :+: S "s")
    [Con s2_2_intro]

s2_2_intro = Paragraph $ S "The scope of the " :+:
    (sMap (map toLower) (requirement ^. descr)) :+: S "s includes the " :+:
    S "physical simulation of " :+: S (twoD ^. name) :+: S " " :+:
    S (rigidBodies ^. name) :+: S " acted on by forces. Given " :+:
    S (twoD ^. name) :+: S " " :+: S (rigidBodies ^. name) :+:
    S ", " :+: S (chipmunk ^. name) :+: S " is intended to simulate how " :+:
    S "these " :+: S (rigidBodies ^. name) :+: S " interact with one another."

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

s2_3 :: Section
s2_3_intro :: [Contents]

s2_3 = Section (S "Organization of Document") (map Con s2_3_intro)

-- NOTE: References pending --
s2_3_intro = [Paragraph (S "The organization of this document follows the " :+:
    S "template for an " :+: S (srs ^. name) :+: S " for scientific " :+:
    S "computing software proposed by [1] and [2]. The presentation " :+:
    S "follows the standard pattern of presenting goals, theories, " :+:
    S "definitions, and assumptions. For readers that would like a more " :+:
    S "bottom up approach, they can start reading the " :+:
    (sMap (map toLower) (instMod ^. descr)) :+: S "s in " :+:
    (makeRef s4_2_5) :+: S " and trace back to any additional information " :+:
    S "they require."),
    Paragraph (S "The " :+: (sMap (map toLower) (goalStmt ^. descr)) :+:
    S "s are refined to the " :+: (sMap (map toLower) (theoMod ^. descr)) :+:
    S "s, and the " :+: (sMap (map toLower) (theoMod ^. descr)) :+:
    S "s to the " :+: (sMap (map toLower) (instMod ^. descr)) :+: S "s.")]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

s3 :: Section
s3_intro :: Contents

s3 = Section (S "General System Description") [Con s3_intro, Sub s3_1,
    Sub s3_2]

s3_intro = Paragraph $ S "This section provides general information " :+:
    S "about the system, identifies the interfaces between the system and " :+:
    S "its environment, and describes the user characteristics and the " :+:
    S "system constraints."

--------------------------------
-- 3.1 : User Characteristics --
--------------------------------

s3_1 :: Section
s3_1_intro :: Contents

s3_1 = Section (S "User Characteristics") [Con s3_1_intro]

s3_1_intro = Paragraph $ S "The end user of " :+: S (chipmunk ^. name) :+:
    S " should have an understanding of first year programming concepts " :+:
    S "and an understanding of high school physics."

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
s4_intro :: Contents

s4 = Section (S "Specific System Description") [Con s4_intro, Sub s4_1,
    Sub s4_2]

s4_intro = Paragraph $ S "This section first presents the problem " :+:
    S "description, which gives a high-level view of the problem to be " :+:
    S "solved. This is followed by the solution characteristics " :+:
    S "specification, which presents the " :+: (sMap (map toLower)
    (assumption ^. descr)) :+: S "s, theories, and definitions that are " :+:
    S "used for the " :+: S (physLib ^. name) :+: S"."

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1 :: Section
s4_1_intro :: Contents

s4_1 = Section (S "Problem Description") [Con s4_1_intro, Sub s4_1_1,
    Sub s4_1_2]

s4_1_intro = Paragraph $ S "Creating a gaming " :+: S (physLib ^. name) :+:
    S " is a difficult task. Games need physics libraries that simulate " :+:
    S "objects acting under various physical conditions, while " :+:
    S "simultaneously being fast and efficient enough to work in soft " :+:
    S "real-time during the game. Developing a " :+: S (physLib ^. name) :+:
    S " from scratch takes a long period of time and is very costly, " :+:
    S "presenting barriers of entry which make it difficult for game " :+:
    S "developers to include physics in their products. There are a few " :+:
    S "free, open source and high quality physics libraries available to " :+:
    S "be used for consumer products (" :+: (makeRef s7) :+:
    S "). By creating a simple, lightweight, fast and portable " :+:
    S (twoD ^. name) :+: S " " :+: S (rigidBody ^. name) :+: S " " :+:
    S (physLib ^. name) :+: S ", game development will be more accessible " :+:
    S "to the masses and higher quality products will be produced."

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1 :: Section
s4_1_1_intro, s4_1_1_bullets :: Contents

s4_1_1 = Section (S "Terminology and Definitions") [Con s4_1_1_intro,
    Con s4_1_1_bullets]

s4_1_1_intro = Paragraph $ S "This subsection provides a list of terms " :+:
    S "that are used in subsequent sections and their meaning, with the " :+:
    S "purpose of reducing ambiguity and making it easier to correctly " :+:
    S "understand the requirements:"

s4_1_1_bullets = Enumeration (Bullet $ map (\term -> Flat (
    S ((\word -> (toUpper . head $ word) : (tail word)) (term ^. name)) :+:
    S ": " :+: (term ^. descr)))
    [rigidBody, elast, ctrOfMass, cartesian, rightHand])

-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

s4_1_2 :: Section
s4_1_2_list :: Contents

s4_1_2 = Section ((goalStmt ^. descr) :+: S "s") [Con s4_1_2_list]

s4_1_2_list = Enumeration (Simple [
    (S (goalStmt ^. name) :+: S "1", Flat (S "Given the physical " :+:
    S "properties, initial " :+: (position ^. descr) :+: S "s and " :+:
    S (vels ^. name) :+: S ", and " :+: (force ^. descr) :+:
    S "s applied on a set of " :+: S (rigidBodies ^. name) :+:
    S ", determine their new " :+: (position ^. descr) :+: S "s and " :+:
    S (vels ^. name) :+: S " over a period of " :+: (time ^. descr) :+: S ".")),
    (S (goalStmt ^. name) :+: S "2", Flat (S "Given the physical " :+:
    S "properties, initial " :+: (orientation ^. descr) :+: S "s and " :+:
    S (angularVels ^. name) :+: S ", and " :+: (force ^. descr) :+: S "s " :+:
    S "applied on a set of " :+: S (rigidBodies ^. name) :+: S ", " :+:
    S "determine their new " :+: (orientation ^. descr) :+: S "s and " :+:
    S (angularVels ^. name) :+: S " over a period of " :+: (time ^. descr) :+:
    S ".")),
    (S (goalStmt ^. name) :+: S "3", Flat (S "Given the initial " :+:
    (position ^. descr) :+: S "s and " :+: S (vels ^. name) :+: S " of a " :+:
    S "set of " :+: S (rigidBodies ^. name) :+: S ", determine if any of " :+:
    S "them will collide with one another over a period of " :+:
    (time ^. descr) :+: S ".")),
    (S (goalStmt ^. name) :+: S "4", Flat (S "Given the physical " :+:
    S "properties, initial linear and angular " :+: (position ^. descr) :+:
    S "s and " :+: S (vels ^. name) :+: S ", determine the new " :+:
    (position ^. descr) :+: S "s and " :+: S (vels ^. name) :+:
    S " over a period of " :+: (time ^. descr) :+: S " of " :+:
    S (rigidBodies ^. name) :+: S " that have undergone a " :+:
    S (coll ^. name) :+: S "."))])

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

s4_2_1 = Section (assumption ^. descr :+: S "s") [Con s4_2_1_intro,
    Con s4_2_1_list]

-- TODO: Add assumption references in the original and this SRS. --
s4_2_1_intro = Paragraph $ S "This section simplifies the original problem " :+:
    S "and helps in developing the theoretical model by filling in the " :+:
    S "missing information for the physical system. The numbers given in " :+:
    S "the square brackets refer to the " :+: foldr1 (:+:) (intersperse (S ", ")
    (map (\ch -> (sMap (map toLower) (ch ^. descr)) :+: S (" " ++
    sqbrac (ch ^. name))) [theoMod, genDefn, dataDefn, instMod])) :+:
    S ", or " :+: (sMap (map toLower) $ likelyChange ^. descr) :+: S (" " ++
    sqbrac (likelyChange ^. name)) :+: S ", in which the respective " :+:
    (sMap (map toLower) $ assumption ^. descr) :+: S " is used."

s4_2_1_list = Enumeration (Simple [
    (S (assumption ^. name) :+: S "1", Flat (S "All objects are " :+:
    S (rigidBodies ^. name) :+: S ".")),
    (S (assumption ^. name) :+: S "2", Flat (S "All objects are " :+:
    S (twoD ^. name) :+: S ".")),
    (S (assumption ^. name) :+: S "3", Flat (S "The library uses a " :+:
    S (init (cartesian ^. name)) :+: S " system.")),
    (S (assumption ^. name) :+: S "4", Flat (S "The axes are defined using " :+:
    S (rightHand ^. name) :+: S ".")),
    (S (assumption ^. name) :+: S "5", Flat (S "All " :+:
    S (rigidBodies ^. name) :+: S " " :+: S (coll ^. name) :+:
    S "s are vertex-to-edge " :+: S (coll ^. name) :+: S "s.")),
    (S (assumption ^. name) :+: S "6", Flat (S "There is no damping " :+:
    S "involved throughout the simulation.")),
    (S (assumption ^. name) :+: S "7", Flat (S "There are no constraints " :+:
    S "and joints involved throughout the simulation."))])

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

s4_2_2 :: Section
s4_2_2_intro :: Contents
s4_2_2_TMods :: [Contents]

s4_2_2 = Section ((theoMod ^. descr) :+: S "s") ([Con s4_2_2_intro] ++
    (map Con s4_2_2_TMods))

s4_2_2_intro = Paragraph $ S "This section focuses on the general equations ":+:
    S "the " :+: S (physLib ^. name) :+: S " is based on."

s4_2_2_TMods = map Definition (map Theory cpTMods)

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

s4_2_3 :: Section
s4_2_3_intro :: Contents
-- s4_2_3_GDefs :: [Contents]

s4_2_3 = Section ((genDefn ^. descr) :+: S "s") ([Con s4_2_3_intro] {- ++
  (map Con s4_2_3_GDefs)-})

s4_2_3_intro = Paragraph $ S "This section collects the laws and equations " :+:
  S "that will be used in deriving the " :+: (sMap (map toLower)
  (dataDefn ^. descr)) :+: S "s, which in turn will be used to build the " :+:
  (sMap (map toLower) (instMod ^. descr)) :+: S "s."

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

s4_2_4 = Section ((dataDefn ^. descr) :+: S "s") ([Con s4_2_4_intro] ++
    (map Con s4_2_4_DDefs))

s4_2_4_intro = Paragraph $ S "This section collects and defines all the " :+:
    S "data needed to build the " :+: (instMod ^. descr) :+: S "s. The " :+:
    S "dimension of each quantity is also given."

s4_2_4_DDefs = map Definition (map Data cpDDefs)

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5 :: Section
s4_2_5_intro :: Contents
-- s4_2_5_IMods :: [Contents]

s4_2_5 = Section ((instMod ^. descr) :+: S "s") ([Con s4_2_5_intro] {- ++
    (map Con s4_2_5_IMods)-})

s4_2_5_intro = Paragraph $ S "This section transforms the problem defined " :+:
    S "in " :+: (makeRef s4_1) :+: S " into one expressed in mathematical " :+:
    S "terms. It uses concrete symbols defined in " :+: (makeRef s4_2_4) :+:
    S " to replace the abstract symbols in the models identified in " :+:
    (makeRef s4_2_2) :+: S " and " :+: (makeRef s4_2_3) :+: S "."

-- Instance models not yet implemented --
-- s4_2_5_IMods = ?

------------------------------
-- 4.2.6 : Data Constraints --
------------------------------

s4_2_6 :: Section
s4_2_6_intro, s4_2_6_table1, s4_2_6_table2 :: Contents

s4_2_6 = Section (S "Data Constraints") [Con s4_2_6_intro, Con s4_2_6_table1,
    Con s4_2_6_table2]

s4_2_6_intro = Paragraph $ S "Table 1 and 2 show the data constraints on " :+:
    S "the input and output variables, respectively. The " :+:
    (Quote $ S "Physical Constraints") :+: S " column gives the physical " :+:
    S "limitations on the range of values that can be taken by the " :+:
    S "variable. The constraints are conservative, to give the user of the " :+:
    S "model the flexibility to experiment with unusual situations. The " :+:
    S "column of typical values is intended to provide a feel for a " :+:
    S "common scenario."

-- Currently unable to write relations in sentences, so verbal explanations
-- will do for now.
-- How do I write 2pi in constraints?
s4_2_6_table1 = Table [S "Var", S "Physical Constraints", S "Typical Value"]
    (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] [
    [P (len ^. symbol), P (len ^. symbol) :+: S " is G/E to 0", S "44.2 " :+:
    Sy (len ^. unit)],
    [P (mass ^. symbol), P (mass ^. symbol) :+: S " is greater than 0",
    S "56.2 " :+: Sy (mass ^. unit)],
    [P (momtInert ^. symbol), P (momtInert ^. symbol) :+: S " is G/E to 0",
    S "74.5 " :+: Sy (momtInert ^. unit)],
    [P (gravAccel ^. symbol), S "None", S "9.8 " :+: Sy (gravAccel ^. unit)],
    [P (position ^. symbol), S "None", S "(0.412, 0.502) " :+:
    Sy (position ^. unit)],
    [P (vel ^. symbol), S "None", S "2.51 " :+: Sy (vel ^. unit)],
    [P (restCoef ^. symbol), P (restCoef ^. symbol) :+: S " G/E to 0 and " :+:
    P (restCoef ^. symbol) :+: S " less than 1", S "0.8"],
    [P (orientation ^. symbol), P (orientation ^. symbol) :+: S " G/E to 0 " :+:
    S "and " :+: P (orientation ^. symbol) :+: S " less than 2pi", S "pi/2 " :+:
    Sy (orientation ^. unit)],
    [P (angVel ^. symbol), S "None", S "2.1 " :+: Sy (angVel ^. unit)],
    [P (force ^. symbol), S "None", S "98.1 " :+: Sy (force ^. unit)],
    [P (torque ^. symbol), S "None", S "200 " :+: Sy (torque ^. unit)]
    ]) (S "Table 1: Input Variables") True

s4_2_6_table2 = Table [S "Var", S "Physical Constraints"]
    (mkTable [(\x -> x!!0), (\x -> x!!1)] [
    [P (position ^. symbol), S "None"],
    [P (vel ^. symbol), S "None"],
    [P (orientation ^. symbol), P (orientation ^. symbol) :+: S " G/E to 0 " :+:
    S "and " :+: P (orientation ^. symbol) :+: S " less than 2pi"],
    [P (angVel ^. symbol), S "None"]
    ]) (S "Table 2: Output Variables") True

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

s5 :: Section
s5_intro :: Contents

s5 = Section (requirement ^. descr :+: S "s") [Con s5_intro, Sub s5_1,
    Sub s5_2]

s5_intro = Paragraph $ S "This section provides the functional " :+:
    (sMap (map toLower) (requirement ^. descr)) :+: S "s, the business " :+:
    S "tasks that the software is expected to complete, and the " :+:
    S "nonfunctional " :+: (sMap (map toLower) (requirement ^. descr)) :+:
    S "s, the qualities that the software is expected to exhibit."

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1_list :: Contents

s5_1 = Section (S "Functional " :+: (requirement ^. descr) :+: S "s")
    [Con s5_1_list]

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
s5_1_list = Enumeration (Simple [
    (S (requirement ^. name) :+: S "1", Flat (S "Create a " :+:
    S (space ^. name) :+: S " for all of the " :+: S (rigidBodies ^. name) :+:
    S " in the physical simulation to interact in.")),
    (S (requirement ^. name) :+: S "2", Flat (S "Input the initial " :+:
    (mass ^. descr) :+: S "es, " :+: S (vels ^. name) :+: S ", " :+:
    (orientation ^. descr) :+: S "s, " :+: S (angularVels ^. name) :+:
    S " of, " :+: S "and " :+: (force ^. descr) :+: S "s applied on " :+:
    S (rigidBodies ^. name) :+: S ".")),
    (S (requirement ^. name) :+: S "3", Flat (S "Input the surface " :+:
    S "properties of the bodies, such as " :+: S (fric ^. name) :+: S " or " :+:
    S (elast ^. name) :+: S ".")),
    (S (requirement ^. name) :+: S "4", Flat (S "Verify that the inputs " :+:
    S "satisfy the required physical constraints.")),
    (S (requirement ^. name) :+: S "5", Flat (S "Determine the " :+:
    (position ^. descr) :+: S "s and " :+: S (vels ^. name) :+: S " over a " :+:
    S "period of " :+: (time ^. descr) :+: S " of the " :+: S (twoD ^. name) :+:
    S " " :+: S (rigidBodies ^. name) :+: S " acted upon by a " :+:
    (force ^. descr) :+: S ".")),
    (S (requirement ^. name) :+: S "6", Flat (S "Determine the " :+:
    (orientation ^. descr) :+: S "s and " :+: S (angularVels ^. name) :+:
    S " over a period of " :+: (time ^. descr) :+: S " of the " :+:
    S (twoD ^. name) :+: S " " :+: S (rigidBodies ^. name) :+: S ".")),
    (S (requirement ^. name) :+: S "7", Flat (S "Determine if any of the " :+:
    S (rigidBodies ^. name) :+: S " in the " :+: S (space ^. name) :+:
    S " have collided.")),
    (S (requirement ^. name) :+: S "8", Flat (S "Determine the " :+:
    (position ^. descr) :+: S "s and " :+: S (vels ^. name) :+: S " over a " :+:
    S "period of " :+: (time ^. descr) :+: S " of the " :+: S (twoD ^. name) :+:
    S " " :+: S (rigidBodies ^. name) :+: S " that have undergone a " :+:
    S (coll ^. name) :+: S "."))])

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

s5_2 :: Section
s5_2_intro :: Contents

s5_2 = Section (S "Nonfunctional " :+: (requirement ^. descr) :+: S "s")
    [Con s5_2_intro]

s5_2_intro = Paragraph $ S "Games are resource intensive, so performance " :+:
    S "is a high priority. Other non-functional requirements that are a " :+:
    S "priority are: correctness, understandability, portability, " :+:
    S "reliability, and maintainability."

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6_intro, s6_list :: Contents

s6 = Section ((likelyChange ^. descr) :+: S "s") [Con s6_intro, Con s6_list]

s6_intro = Paragraph $ S "This section lists the " :+: (sMap (map toLower)
    (likelyChange ^. descr)) :+: S "s to be made to the physics game library."

s6_list = Enumeration (Simple [
    (S (likelyChange ^. name) :+: S "1", Flat (S "The internal " :+:
    S (ode ^. name) :+: S "-solving algorithm used by the library may " :+:
    S "change in the future.")),
    (S (likelyChange ^. name) :+: S "2", Flat (S "The library may be " :+:
    S "expanded to deal with edge-to-edge and vertex-to-vertex " :+:
    S (coll ^. name) :+: S "s.")),
    (S (likelyChange ^. name) :+: S "3", Flat (S "The library may be " :+:
    S "expanded to include motion with damping.")),
    (S (likelyChange ^. name) :+: S "4", Flat (S "The library may be " :+:
    S "expanded to include joints and constraints."))])

-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

s7 :: Section
s7_intro, s7_2dlist, s7_mid, s7_3dlist :: Contents

s7 = Section (S "Off-the-Shelf Solutions") [Con s7_intro, Con s7_2dlist,
    Con s7_mid, Con s7_3dlist]

s7_intro = Paragraph $ S "As mentioned in " :+: (makeRef s4_1) :+:
    S ", there already exist free open source game physics libraries. " :+:
    S "Similar " :+: S (twoD ^. name) :+: S " physics libraries are:"

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
