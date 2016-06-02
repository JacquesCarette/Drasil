{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Example.Drasil.GamePhysics.ChipBody where

import Data.Char (toLower)
import Data.List (intersperse)
import Control.Lens ((^.))

import Example.Drasil.GamePhysics.ChipExample
import Example.Drasil.GamePhysics.ChipUnits

import Language.Drasil
import Language.Drasil.SI_Units


this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [newton, radians]

s1, s1_intro, s1_1, s1_1_intro, s1_1_table, s1_2, s1_2_intro, s1_2_table, s1_3, s1_3_table, s2, s2_intro, s2_1, s2_1_intro, s2_2, s2_2_intro, s2_3, s2_3_intro, s3, s3_intro, s3_1, s3_1_intro, s3_2, s3_2_intro, s4, s4_intro, s4_1, s4_1_intro, s4_1_1, s4_1_1_intro, s4_1_1_bullets, s4_1_2, s4_1_2_intro, s4_1_3, s4_1_3_list, s4_2, s4_2_1, s4_2_1_intro, s4_2_1_list, s4_2_2, s4_2_2_intro, s5, s5_intro, s5_1, s5_1_list, s5_2, s5_2_intro, s6, s6_intro, s7, s7_intro, s7_2dlist, s7_mid, s7_3dlist :: LayoutObj

chip_srs :: Document
chip_srs = Document (S "Software Requirements Specification for Game " :+:
          S "Physics Library")
          (S "Luthfi Mawarid and Alex Halliwushka") [s1, s2, s3, s4, s5, s6, s7]

{-TODO: Split more keywords and definitions into chunks-}

-- SECTION 1 : REFERENCE MATERIAL --

s1 = Section 0 (S "Reference Material") [s1_intro, s1_1, s1_2, s1_3]

s1_intro = Paragraph (S "This section records information for easy reference.")

s1_1 = Section 1 (S "Table of Units") [s1_1_intro, s1_1_table]

s1_1_intro = Paragraph (S "Throughout this document SI (Syst" :+:
           (F Grave 'e') :+: S "me International d'Unit" :+:
           (F Acute 'e') :+: S "s) is employed as the unit system." :+:
           S " In addition to the basic units, several derived units are" :+:
           S " employed as described below. For each unit, the symbol is" :+:
           S " given followed by a description of the unit followed by " :+:
           S "the SI name.")

s1_1_table = Table [S "Symbol", S "Description", S "Name"] (mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> (x ^. descr)),
   (\x -> S (x ^. name))
  ] this_si)
  (S "Table of Units") True

s1_2 = Section 1 (S "Table of Symbols") [s1_2_intro, s1_2_table]

s1_2_intro = Paragraph $
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The symbols are listed in " :+:
  S "alphabetical order."

s1_2_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
  [(\ch -> U (ch ^. symbol)), -- (\ch -> N (ch ^. symbol)) ,
   (\ch -> Sy $ ch ^. unit),
   (\ch -> ch ^. descr)
   ]
  chipSymbols)
  (S "Table of Symbols") False

s1_3 = Section 1 (S "Abbreviations and Acronyms") [s1_3_table]

s1_3_table = Table [S "Symbol", S "Description"] (mkTable
  [(\ch -> S $ ch ^. name),
   (\ch -> ch ^. descr)]
  acronyms)
  (S "Abbreviations and Acronyms") False

-- SECTION 2 : INTRODUCTION --

s2 = Section 0 (S "Introduction") [s2_intro, s2_1, s2_2, s2_3]

s2_intro = Paragraph $ S "Due to the rising cost of developing video " :+:
  S "games, developers are looking for ways to save time and money for " :+:
  S "their projects. Using an open source physics library that is " :+:
  S "reliable and free will cut down development costs and lead to better " :+:
  S "quality products." :+:
  S "The following section provides an overfiew of the " :+:
  (softwareRS ^. descr) :+:
  S " (SRS) for Chipmunk2D, an open source 2D rigid body physics library. " :+:
  S "This section explains the purpose of this document, the scope of the " :+:
  S "system, and the organization of the document."

s2_1 = Section 1 (S "Purpose of Document") [s2_1_intro]

s2_1_intro = Paragraph $ S "This document descibes the modeling of an open " :+:
  S "source 2D rigid body physics library used for games. The goals and " :+:
  S "the theoretical models used in Chipmunk2D are provided. This document " :+:
  S "is intended to be used as a reference to provide all necessary " :+:
  S "information to understand and verify the model." :+:
  S "This document will be used as a starting point for subsequent " :+:
  S "development phases, including writing the design specification and " :+:
  S "the software verification and validation plan. The design document " :+:
  S "will show how the requirements are to be realized. The verification " :+:
  S "and validation plan will show the steps that will be used to increase " :+:
  S "confidence in the software documentation and the implementation."

s2_2 = Section 1 (S "Scope of " :+: (requirement ^. descr) :+: S "s")
  [s2_2_intro]

s2_2_intro = Paragraph $ S "The scope of the requirements includes the " :+:
  S "physical simulation of 2D rigid bodies acted on by forces. Given 2D " :+:
  S "rigid bodies, Chipmunk2D is intended to simulate how these rigid " :+:
  S "bodies interact with one another."

s2_3 = Section 1 (S "Organization of Document") [s2_3_intro]

--TODO: insert references in the following brackets

s2_3_intro = Paragraph $ S "The organization of this document follows the " :+:
  S "template for an SRS for scientific computing software proposed by [1] " :+:
  S "and [2]. The presentation follows the standard pattern of presenting " :+:
  S "goals, theories, definitions, and assumptions. For readers that would " :+:
  S "like a more bottom up approach, they can start reading the instance " :+:
  S "models in Section 4.2.5 and trace back to any additional information " :+:
  S "they require." :+:
  S "The goal statements are refined to the theoretical models, and the " :+:
  S "theoretical models to the instance models."

-- SECTION 3 : GENERAL SYSTEM DESCRIPTION --

s3 = Section 0 (S "General System Description") [s3_intro, s3_1, s3_2]

s3_intro = Paragraph $ S "This section provides general information " :+:
  S "about the system, identifies the interfaces between the system and " :+:
  S "its environment, and describes the user characteristics and the " :+:
  S "system constraints."

s3_1 = Section 1 (S "User Characteristics") [s3_1_intro]

s3_1_intro = Paragraph $ S "The end user of Chipmunk2D should have an " :+:
  S "understanding of first year programming concepts and an " :+:
  S "understanding of high school physics."

s3_2 = Section 1 (S "System Constraints") [s3_2_intro]

s3_2_intro = Paragraph $ S "There are no system constraints."

-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --

-- NOTE: Section 4 remains incomplete. General definitions, data definitions
-- and instance models have not been encoded.
-- Some require the summation and integral symbols which have yet to be
-- implemented.

s4 = Section 0 (S "Specific System Description") [s4_intro, s4_1, s4_2]

s4_intro = Paragraph $ S "This section first presents the problem " :+:
  S "description, which gives a high-level view of the problem to be solved" :+:
  S ". This is followed by the solution characteristics specification, " :+:
  S "which presents the assumptions, theories, definitions that are used " :+:
  S "for the physics library."

s4_1 = Section 1 (S "Problem Description") [s4_1_intro, s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = Paragraph $ S "Creating a gaming physics library is a " :+:
  S "difficult task. Games need physics libraries that simulate objects " :+:
  S "acting under physics conditions, while at the same time they need to " :+:
  S "be efficient and fast enough to work in soft real-time during the " :+:
  S "game. Developing a physics library from scratch takes a long period " :+:
  S "time and is very costly. These barriers of entry make it difficult " :+:
  S "for game developers to include physics in their products. There are a " :+:
  S "few free, open source and high quality physics libraries available to " :+:
  S "be used for consumer products (" :+: (makeRef s7) :+:
  S "). By creating a simple, lightweight, fast and portable 2D rigid body " :+:
  S "physics library, game development will be more accessible to the " :+:
  S "masses and higher quality products will be produced."

s4_1_1 = Section 2 (S "Terminology and Definitions") [s4_1_1_intro, s4_1_1_bullets]

s4_1_1_intro = Paragraph $ S "This subsection provides a list of terms that" :+:
  S " are used in subsequent sections and their meaning, with the purpose " :+:
  S "of reducing ambiguity and making it easier to correctly understand the" :+:
  S " requirements:"

s4_1_1_bullets = BulletList $ [
  S "Rigid Body: a solid body in which deformation is neglected.",
  S "Elasticity: ratio of the velocities of the two colliding objects " :+:
  S "after and before the collision.",
  S "Centre of mass: the mean location of the distribution of mass of the " :+:
  S "object.",
  S "Cartesian coordinates: a coordinate system that specifies each point " :+:
  S "uniquely in a plane by a pair of numerical coordinates.",
  S "Right-handed coordinate system: A coordinate system where the " :+:
  S "positive z-axis comes out of the screen."]

s4_1_2 = Section 2 (physSysDescr ^. descr) [s4_1_2_intro]

s4_1_2_intro = Paragraph $ S "N/A."

s4_1_3 = Section 2 ((goalStmt ^. descr) :+: S "s") [s4_1_3_list]

s4_1_3_list = SimpleList $ [
  (S "GS1", S "given the " :+: (initPos ^. descr) :+: S " and velocity, " :+:
   S "determine the new positions and velocities over a period of " :+:
   (time ^. descr) :+: S " a 2D rigid body acted upon by a" :+: (force ^. descr)),
  (S "GS2", S "given the " :+: (initPos ^. descr) :+: S "s and velocities, " :+:
   S "determine if any of the rigid bodies will collide with one another " :+:
   S "over a period of" :+: (time ^. descr) :+:  S "."),
  (S "GS3", S "given the " :+: (initPos ^. descr) :+: S "s and velocities, " :+:
   S "determine the new positions and velocities"),
  (S "GS4", S "given the initial " :+: (orientation ^. descr) :+: S " and " :+:
   (angVel ^. descr) :+: S ", determine the new " :+: (orientation ^. descr) :+:
   S " and " :+: (angVel ^. descr) :+: S " over a period of time of a 2D " :+:
   S "rigid body acted upon by a " :+: (force ^. descr) :+: S "."),
  (S "GS5", S "given the " :+: (initPos ^. descr) :+: S "s and velocities, " :+:
   S "determine the positions and velocities over a period of " :+:
   (time ^. descr) :+: S " of 2D rigid bodies with constraints or joints " :+:
   S "between them.")]

s4_2 = Section 1 (S "Solution Characteristics Specification")
  [s4_2_1, s4_2_2]

s4_2_1 = Section 2 (assumption ^. descr :+: S "s") [s4_2_1_intro, s4_2_1_list]

s4_2_1_intro = Paragraph $ S "This section simplifies the original problem " :+:
  S "and helps in developing the theoretical model by filling in the " :+:
  S "missing information for the physical system. The numbers given in the " :+:
  S "square brackets refer to the " :+: foldr1 (:+:) (intersperse (S ", ")
  (map (\ch -> (sMap (map toLower) (ch ^. descr)) :+: S (" " ++
  sqbrac (ch ^. name))) [theoreticMod, genDefn, dataDefn, instanceMod])) :+:
  S ", or " :+: (sMap (map toLower) $ likelyChange ^. descr) :+: S (" " ++
  sqbrac (likelyChange ^. name)) :+: S ", in which the respective " :+:
  (sMap (map toLower) $ assumption ^. descr) :+: S " is used."

s4_2_1_list = SimpleList $ [
  (S "A1", S "All objects are rigid bodies."),
  (S "A2", S "All objects are 2D."),
  (S "A3", S "The damping coefficient is constant throughout the simulation."),
  (S "A4", S "The library uses a Cartesian coordinate system."),
  (S "A5", S "The axes are defined using a right hand system.")]

s4_2_2 = Section 2 ((theoreticMod ^. descr) :+: S "s")
  (s4_2_2_intro:s4_2_2_TMods)

s4_2_2_intro = Paragraph $ S "This section focuses on the general equations ":+:
  S "the physics library is based on."
-- :+: foldr1 (:+:) (map makeRef s4_2_2_TMods) :+: S" " :+: makeRef s1

s4_2_2_TMods :: [LayoutObj]
s4_2_2_TMods = map Definition (map Theory [t1newtonSndLaw, t2newtonThdLaw, t3newtonUnivGravLaw, t4hookeLaw, t5rotMotionEq])

-- General definitions, data definitions, instance models and
-- constraints to be added

-- SECTION 5 : REQUIREMENTS --

s5 = Section 0 (requirement ^. descr :+: S "s") [s5_intro, s5_1, s5_2]

s5_intro = Paragraph $ S "This section provides the functional requirements" :+:
  S ", the business tasks that the software is expected to complete, and " :+:
  S "the nonfunctional requirements, the qualities that the software is " :+:
  S "expected to exhibit."

s5_1 = Section 1 (S "Functional Requirements") [s5_1_list]

s5_1_list = SimpleList $ [
  (S "R1", S "Create a space for all rigid bodies to interact in within " :+:
   S "the physical simulation."),
  (S "R2", S "Input the initial masses, positions, velocities," :+:
   S "orientations, angular velocities, and constraints of objects."),
  (S "R3", S "Input the surface properties of objects such as friction " :+:
   S "and elasticity."),
  (S "R4", S "Input the constraints of the objects."),
  (S "R5", S "Verify that the inputs satisfy required physical constraints."),
  (S "R6", S "Determine the position and velocities over a period of " :+:
   (time ^. descr) :+: S " of the 2D rigid bodies acted upon by a " :+:
   (force ^. descr) :+: S " ."),
  (S "R7", S "Determine if any of the rigid bodies in the space have " :+:
   S "collided."),
  (S "R8", S "Determine the position and velocities over a period of " :+:
   (time ^. descr) :+: S " of 2D rigid bodies that have undergone a " :+:
   S "collision."),
  (S "R9", S "Determine the " :+: (orientation ^. descr) :+: S " and " :+:
   S "angular velocities over a period of " :+: (time ^. descr) :+:
   S " of 2D rigid bodies that have undergone a collision."),
  (S "R10", S "Determine the position and velocities over a period of " :+:
   (time ^. descr) :+: S " of 2D rigid bodies with constraints or joints " :+:
   S "between them."),
  (S "R11", S "Allow the user to query the space and return information " :+:
   S "about the rigid bodies.")]

s5_2 = Section 1 (S "Nonfunctional Requirements") [s5_2_intro]

s5_2_intro = Paragraph $ S "Games are resource intensive, so performance " :+:
   S "is a high priority. Other non-functional requirements that are a " :+:
   S "priority are: correctness, understandability, portability, " :+:
   S "reliability, and maintainability."

-- SECTION 6 : LIKELY CHANGES --

s6 = Section 0 ((likelyChange ^. descr) :+: S "s") [s6_intro]

s6_intro = Paragraph $ S "N/A."

-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --

s7 = Section 0 (S "Off-the-Shelf Solutions") [s7_intro, s7_2dlist, s7_mid,
  s7_3dlist]

s7_intro = Paragraph $ S "As mentioned in " :+: (makeRef s4_1) :+:
  S ", there already exists free open source game physics libraries. " :+:
  S "Similar 2D physics libraries are:"

s7_2dlist = BulletList $ [
  S "Box2D http://box2d.org/",
  S "Nape Physics Engine http://napephys.com/"]

s7_mid = Paragraph $ S "Free open source 3D game physics libraries include:"

s7_3dlist = BulletList $ [
  S "Bullet http://bulletphysics.org/",
  S "Open Dynamics Engine http://www.ode.org/",
  S "Newton Game Dynamics http://newtondynamics.com/"]
