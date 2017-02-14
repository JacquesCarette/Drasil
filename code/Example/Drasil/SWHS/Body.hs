module Drasil.SWHS.Body where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Data.Drasil.SI_Units 
import Data.Drasil.Authors

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.PhysicalProperties hiding (mass)
import Data.Drasil.Concepts.Thermodynamics

import Data.Drasil.Quantities.Physics (surface)
import Data.Drasil.Quantities.Math (gradient, norm_vect)

import Drasil.SWHS.Unitals
import Drasil.SWHS.Concepts
import Drasil.SWHS.TModel1
import Drasil.SWHS.TModel2
import Drasil.SWHS.TModel3
import Drasil.SWHS.DataDefs
import Drasil.SWHS.Modules
import Drasil.SWHS.Changes
import Drasil.SWHS.Reqs

import Drasil.TableOfUnits
import Drasil.TableOfSymbols
import Drasil.TableOfAbbAndAcronyms
import Drasil.OrganizationOfSRS
import Drasil.SRS
import Drasil.ReferenceMaterial
import Drasil.DocumentLanguage

acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ordDiffEq,
  phsChgMtrl,physSyst,requirement,rightSide,srs,progName,thModel]

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

--Will there be a table of contents?

s1, s1_1, s1_2, s1_3, s2, s2_1, s2_2, s2_3, s3, s3_1, s3_2, s4, s4_1,
  s4_1_1, s4_1_2, s4_1_3, s4_2, s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5,
  s4_2_6, s4_2_7, s5, s5_1, s5_2, s6, s7 :: Section

s1_2_intro, s1_2_table,
  s2_2_contents, s3_intro, s3_1_contents, s3_2_contents, s4_intro, 
  s4_1_intro, s4_1_1_intro, s4_1_1_bullets, s4_1_2_intro, s4_1_2_list,
  fig_tank, s4_1_3_intro, s4_1_3_list, s4_2_intro, s4_2_1_intro, 
  s4_2_1_list, s4_2_2_intro, s4_2_3_intro, s4_2_4_intro, s4_2_6_intro, 
  s5_intro, s5_2_contents, s6_list, s7_intro1, s7_table1, s7_table2,
  s7_table3, s7_fig1, s7_fig2 :: Contents
  
s2_intro, s2_1_contents, s2_3_contents, s4_2_3_deriv, s4_2_5_intro, 
  s4_2_5_deriv1, s4_2_5_deriv2, s4_2_7_deriv, s5_1_list, s7_intro2 :: [Contents]

authors :: Sentence
authors = manyNames [thulasi, brooks, spencerSmith]

swhs_si :: SystemInformation
swhs_si = SI swhs_pcm srs [thulasi, brooks, spencerSmith] 
  this_si swhsSymbols (swhsSymbols) --Note: The second swhsSymbols here is 
    -- Redundant b/c the unitals are not really concepts (yet). There
    -- Will still likely be a better way to do this.

{-
This table is ALMOST correct (just the normal vector is wrong because it should
be using defn).
-}
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [ TUnits, 
                                tsymb'' s1_2_intro (TermExcept [norm_vect]),
                                TVerb s1_3
                              ]
  ) : map Verbatim [s2, s3, s4, s5, s6, s7]

swhs_srs' :: Document
swhs_srs' = mkDoc mkSRS swhs_si

swhs_srs :: Document
swhs_srs = srsDoc swhsFull authors [s1, s2, s3, s4, s5, s6, s7]

-- It is sometimes hard to remember to add new sections both here and above.

-- Title could be automated as long as program name is abstracted out

-- Authors could be abstracted out (specifically, Spencer is an author for 
-- multiple examples)

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

swhs_mg :: Document
swhs_mg = mgDoc swhsFull authors mgBod

-- Again, with program name abstracted out this title could be automated.

-- As above, potentially abstract out author names.

s1 = refSec [s1_1, s1_2, s1_3]

-- This is the same between all examples (could be automated)

s1_1 = table_of_units this_si
  
-- Is it possible to make tables look nicer? I.e. \hline

s1_2 = Section (S "Table of Symbols") [Con s1_2_intro, Con s1_2_table]

s1_2_intro = Paragraph (S "The table that follows summarizes the" +:+
  S "symbols used in this document along with their units" :+:
  S ". The choice of symbols was made to be consistent" +:+
  S "with the" +:+ (sLower (heat_trans ^. term)) :+: 
  S " literature" +:+
  S "and with existing documentation for" +:+ (sLower
  (progName ^. defn)) :+: S "s. The symbols are listed in" +:+.
  S "alphabetical order")

-- "heat transfer" and program name are specific, but otherwise this paragraph 
-- is general. If it were to be automated, there is a sentence in the game 
-- physics example about how symbols for vectors are bold, which would be 
-- useful to include in the automated paragraph.

s1_2_table = table swhsSymbols (termExcept [cqs norm_vect])
  
s1_3 = table_of_abb_and_acronyms acronyms
  
-- This section name and table structure are same between all examples.
  
s2 = Section (S "Introduction") ((map Con s2_intro)++[Sub s2_1, Sub s2_2, 
  Sub s2_3])

s2_intro = [Paragraph (S "Due to increasing cost, diminishing" +:+
  S "availability, and negative environmental impact of" +:+
  S "fossil fuels, there is a higher demand for renewable" +:+.
  S "energy sources and energy storage technology" +:+
  (swhs_pcm ^. defn) +:+ S "(" :+: (phsChgMtrl ^. term) :+: 
  S ") use a renewable energy source and provide a novel way of" +:+.
  S "storing energy" +:+ (swhs_pcm ^. term) +:+ S "improve" +:+
  S "over the traditional" +:+ (sLower (progName ^. 
  defn)) :+: S "s because of their smaller size. The smaller size" +:+
  S "is possible because of the ability of" +:+ 
  (phsChgMtrl ^. term) +:+ S "to store" +:+
  (sLower (thermal_energy ^. term)) +:+ S "as" +:+
  (sLower (latent_heat ^. term)) :+: S ", which" +:+
  S "allows higher" +:+ (sLower (thermal_energy ^. 
  term)) +:+. S "storage capacity per unit weight"),
  Paragraph (S " The following section provides an overview of the" +:+ (srs ^. defn) +:+ S "(" :+: (srs ^. term) :+:
  S ") for" +:+ (swhs_pcm ^. term) :+: S ". The developed" +:+
  S "program will be referred to as" +:+ (progName ^. defn) +:+
  S "(" :+: (progName ^. term) :+: S "). This section explains" +:+
  S "the purpose of this document, the scope of the system, the" +:+
  S "organization of the document and the characteristics of" +:+.
  S "the intended readers")]

-- NamedChunks... Sometimes capitalized, sometimes not, sometimes plural, 
-- sometimes not, sometimes need to be used in different tenses. How to 
-- accomodate all this?

-- The second paragraph is general except for program name, and there is a 
-- similar paragraph in each of the other examples. It can probably be 
-- abstracted out.

s2_1 = Section (S "Purpose of Document") (map Con s2_1_contents)

s2_1_contents = [Paragraph (S "The main purpose of this document is to" +:+
  S "describe the modelling of" +:+ (swhs_pcm ^. term) :+:
  S ". The" +:+ (sLower (goalStmt ^. defn)) :+: 
  S "s and" +:+ (sLower (thModel ^. defn)) :+: 
  S "s used in the" +:+ (progName ^. term) +:+ S "code" +:+
  S "are provided, with an emphasis on explicitly identifying" +:+ 
  (sLower (assumption ^. defn)) :+: 
  S "s and unambiguous definitions. This document is intended" +:+
  S "to be used as a reference to provide ad hoc access to" +:+
  S "all information necessary to understand and verify the" +:+
  S "model. The" +:+ (srs ^. term) +:+ S "is abstract" +:+ 
  S "because the contents say what problem is being solved," +:+.
  S "but do not say how to solve it"),
  Paragraph (S "This document will be used as a starting" +:+
  S "point for subsequent development phases, including" +:+ 
  S "writing the design specification and the software" +:+
  S "verification and validation plan. The design document" +:+
  S "will show how the" +:+ (sLower (requirement ^.
  defn)) :+: S "s are to be realized, including decisions" +:+.
  S "on the numerical algorithms and programming environment" +:+
  S "The verification and validation plan will show the" +:+
  S "steps that will be used to increase confidence in the" +:+
  S "software documentation and the implementation. Although" +:+
  S "the" +:+ (srs ^. term) +:+ S "fits in a series of" +:+ 
  S "documents that follow the so-called waterfall model, the" +:+
  S "actual development process is not constrained in any" +:+
  S "way. Even when the process is not waterfall, as Parnas" +:+
  S "and Clements [citation] point out, the most logical way" +:+
  S "to present the documentation is still to" +:+
  Quote (S "fake") +:+. S "a rational design process")]

-- Besides program name, these two paragraphs are general, mostly repeated 
-- between examples, and can be abstracted out.

--How to italicize words in sentence?
--How to cite?

s2_2 = Section (S "Scope of Requirements") [Con s2_2_contents]

s2_2_contents = Paragraph (S "The scope of the requirements is limited" +:+
  S "to" +:+ (sLower (thermal_analysis ^.
  term)) +:+ S "of a single" +:+. (tank_pcm ^. term) +:+ 
  S "Given the appropriate inputs, the code for" +:+
  (progName ^. term) +:+ S "is intended to predict the" +:+
  (temp ^. term) +:+ S "and" +:+ (sLower  
  (thermal_energy ^. term)) +:+ S "histories for the" +:+
  (sLower  (water ^. term)) +:+ S "and the" +:+
  (phsChgMtrl ^. term) :+: S ". This entire document" +:+
  S "is written assuming that the substances inside the" +:+
  (sLower (tank ^. defn)) +:+ S "are" +:+
  (sLower (water ^. term)) +:+ S "and" +:+.
  (phsChgMtrl ^. term))

-- There is a similar paragraph in each example, but there's a lot of specific 
-- info here. Would need to abstract out the object of analysis (i.e. solar 
-- water heating tank incorporating PCM, 2D slope composed of homogeneous soil 
-- layers, glass slab and blast, or 2D bodies acted on by forces) and also 
-- abstract out the overall goal of the program (i.e. predict the temperature 
-- and energy histories for the water and PCM, simulate how 2D rigid bodies 
-- interact with each other, predict whether the glass slab is safe to use or 
-- not, etc.). If that is done, then this paragraph can also be abstracted out.

-- The fact that "PCM" must always be capital is especially making things 
-- difficult with concept chunks involving PCM (can't use map toLower).

s2_3 = Section (S "Organization of Document") (map Con s2_3_contents)

s2_3_contents = [Paragraph (S "The organization of this document follows" +:+
  S "the template for an" +:+ (srs ^. term) +:+ S "for" +:+
  S "scientific computing software proposed by [citation] and" +:+
  S "[citation]. The presentation follows the standard" +:+
  S "pattern for presenting" +:+ (sLower (goalStmt ^.
  defn)) :+: S "s," +:+ (sLower (thModel ^. 
  defn)) :+: S "s," +:+ (sLower (dataDefn ^.
  defn)) :+: S "s, and" +:+ (sLower (assumption ^.
  defn)) :+: S "s. For readers that would like a more bottom" :+: 
  S " up approach, they can start reading the" +:+ (sLower (inModel ^. defn)) :+: S "s in" +:+ 
  makeRef s4_2_5 +:+ S "and trace back to find any" +:+
  S "additional information they require. The" +:+
  (sLower (inModel ^. defn)) :+: S "s provide" +:+
  S "the" +:+ (ordDiffEq ^. defn) +:+ S "(" :+: (ordDiffEq ^.
  term) :+: S "s) and algebraic equations that model the" +:+.
  (swhs_pcm ^. term) +:+ (progName ^. term) +:+
  S "solves these" +:+ (ordDiffEq ^. term) :+: S "s."),
  Paragraph $ refineChain [goalStmt, thModel, inModel] +:+
  {-
  (S "The" +:+ (sLower (goalStmt ^. defn))
  :+: S "s are refined to the" +:+ (sLower 
  (thModel ^. defn)) :+: S "s, and" +:+ (sLower 
  (thModel ^. defn)) :+: S "s to the" +:+ (sLower
  (inModel ^. defn)) :+: S "s."
  -}
  S "The" +:+ addS (sLower (inModel ^. defn)) +:+ sParen (makeRef s4_2_5) +:+. 
  S "to be solved are referred to as IM1 to IM4"]

-- This paragraph is mostly general (besides program name and number of IMs), 
-- but there are some differences between the examples that I'm not sure how to 
-- account for. Specifically, the glass example references a Volere paper that 
-- is not used for the other examples. Besides that, this paragraph could 
-- probably be abstracted out with some changes (i.e. the other examples don't 
-- include the last sentence, so we might not need to know the number of IMs 
-- after all if we just leave that sentence out)

-- The swhs_pcm reference at the end of the first paragraph would be better if 
-- singular, but concept is plural.
-- IM1 to IM4 : reference later

-- how to cite/reference?

-- If all SRS have the same basic layout, is it possible to automate
-- the sectioning? This would also improve the tediousness of declaring 
-- LayoutObjs

s3 = Section (S "General System Description") [Con s3_intro, Sub s3_1, 
  Sub s3_2]

s3_intro = Paragraph (S "This section provides general information about" +:+
  S "the system, identifies the interfaces between the system and" +:+
  S "its environment, and describes the user characteristics and" +:+.
  S "the system constraints")

-- Completely general paragraph, same between examples. Easily abstracted out.

s3_1 = Section (S "User Characteristics") [Con s3_1_contents]

s3_1_contents = Paragraph (S "The end user of" +:+ (progName ^. term) :+: 
  S " should have an understanding of undergraduate Level 1" +:+.
  S "Calculus and Physics")

-- Some of these course names are repeated between examples, could potentially 
-- be abstracted out.

s3_2 = Section (S "System Constraints") [Con s3_2_contents]

s3_2_contents = Paragraph (S "There are no system constraints.")

-- This is the same for all of our examples... but there could potentially be 
-- system constraints in other projects so it can't be abstracted out as is...

s4 = Section (S "Specific System Description") [Con s4_intro, Sub s4_1, 
  Sub s4_2]

s4_intro = Paragraph (S "This section first presents the problem" +:+
  S "description, which gives a high-level view of the problem" +:+
  S "to be solved. This is followed by the solution" +:+
  S "characteristics specification, which presents the" +:+
  (sLower (assumption ^. defn)) :+: S "s," +:+ 
  (sLower (thModel ^. defn)) :+: S "s," +:+ 
  (sLower (genDefn ^. defn)) :+: S "s," +:+ 
  (sLower (dataDefn ^. defn)) :+: S "s, and finally" +:+
  S "the" +:+ (sLower (inModel ^. defn)) :+: S "s (" :+:
  (ordDiffEq ^. term) :+: S "s) that model the" +:+. 
  (swhs_pcm ^. term))

-- Completely general except for solar water heating tank (object of analysis) 
-- and similar between all examples; can be abstracted out.
 
-- The swhs_pcm reference at the end would be better if singular, but concept is
-- plural.

s4_1 = Section (S "Problem Description") [Con s4_1_intro, Sub s4_1_1, 
  Sub s4_1_2, Sub s4_1_3]

s4_1_intro = Paragraph ((progName ^. term) +:+ S "is a computer program" +:+
  S "developed to investigate the effect of employing" +:+
  (phsChgMtrl ^. term) +:+ S "within a" +:+. 
  (sLower (tank ^. defn)))

--  section is very different between all examples

s4_1_1 = Section (S "Terminology and Definitions") [Con s4_1_1_intro, 
  Con s4_1_1_bullets]

s4_1_1_intro = Paragraph (S "This subsection provides a list of terms" +:+
  S "that are used in the subsequent sections and their" +:+
  S "meaning, with the purpose of reducing ambiguity and" +:+
  S "making it easier to correctly understand the requirements:")

-- Above paragraph is repeated in all examples, can be abstracted out. (Note: 
-- GlassBR has an additional sentence with a reference at the end.)

s4_1_1_bullets = Enumeration (Bullet $ map s411_bullet_map_f [heat_flux,
   phase_change_material, specific_heat, 
   thermal_conduction, transient])

s411_bullet_map_f :: Concept c => c -> ItemType
s411_bullet_map_f c = Flat ((c ^. term) :+: S ":" +:+ (c ^. defn))
  
-- Structure of this list is same in all examples, probably can be automated.

-- Included heat flux and specific heat in NamedChunks even though they are 
-- already in SWHSUnits

s4_1_2 = Section (physSyst ^. defn) [Con s4_1_2_intro, Con s4_1_2_list, 
  Con fig_tank]

s4_1_2_intro = Paragraph (S "The physical system of" +:+ (progName ^. term)
  :+: S ", as shown in" +:+ (makeRef fig_tank) :+:
  S ", includes the following elements:")

-- Above paragraph is general except for progName and figure. However, not 
-- every example has a physical system. Also, the SSP example is different, so 
-- this paragraph can not be abstracted out as is.

s4_1_2_list = Enumeration (Simple $ [((physSyst ^. term) :+: S "1", Flat
  ((tank ^. term) +:+ S "containing" +:+. (sLower 
  ((water ^. term))))),
--
  ((physSyst ^. term) :+: S "2", Flat ((coil ^. term) :+: 
  S " at bottom of" +:+. (sLower (tank ^. term)) +:+
  sParen (P (ht_flux_C ^. symbol) +:+ S "represents the" +:+.
  (ht_flux_C ^. term)))),
--
  ((physSyst ^. term) :+: S "3", Flat ((phsChgMtrl ^. term) :+: 
  S " suspended in" +:+. (sLower (tank ^. term)) +:+
  sParen (P (ht_flux_P ^. symbol) +:+ S "represents the" +:+.
  (ht_flux_P ^. term))))])

-- Structure of list would be same between examples but content is completely 
-- different

fig_tank = Figure ((tank ^. defn) :+: S ", with" +:+ (ht_flux_C ^. term) +:+
  S "of" +:+ P (ht_flux_C ^. symbol) +:+ S "and" +:+ 
  (ht_flux_P ^. term) +:+ S "of" +:+ P (ht_flux_P ^. symbol)) 
  "Tank.png"

s4_1_3 = Section ((goalStmt ^. defn) :+: S "s") [Con s4_1_3_intro, 
  Con s4_1_3_list]

s4_1_3_intro = Paragraph (S "Given the" +:+ (temp_C ^. term) :+: S "," +:+
  S "initial conditions for the" +:+ (temp_W ^. term) +:+
  S "and the" +:+ (temp_PCM ^. term) :+: S ", and" +:+
  S "material properties, the" +:+
  (sLower (goalStmt ^. defn)) :+: S "s are:")

-- 2 examples include this paragraph, 2 don't. The "givens" would need to be 
-- abstracted out if this paragraph were to be abstracted out.

s4_1_3_list = Enumeration (Simple [((goalStmt ^. term) :+: S "1", Flat 
  (S "Predict the" +:+ (temp_W ^. term) +:+ S "over" +:+.
  (time ^. term))),
--
  ((goalStmt ^. term) :+: S "2", Flat (S "Predict the" +:+
  (temp_PCM ^. term) +:+ S "over" +:+. (time ^. term))),
--
  ((goalStmt ^. term) :+: S "3", Flat (S "Predict the" +:+
  (w_E ^. term) +:+ S "over" +:+. (time ^. term))),
--
  ((goalStmt ^. term) :+: S "4", Flat (S "Predict the" +:+
  (pcm_E ^. term) +:+ S "over" +:+. (time ^. term)))])

-- List structure is repeated between examples. (For all of these lists I am 
-- imagining the potential for something like what was done with the lists in 
-- MG, where you define goals, assumptions, physical system components, etc. in 
-- separate files, import them and pass them as arguments to some "makeSRS" 
-- function and the rest is automated.)

s4_2 = Section (S "Solution Characteristics Specification") [Con s4_2_intro, 
  Sub s4_2_1, Sub s4_2_2, Sub s4_2_3, Sub s4_2_4, Sub s4_2_5, Sub s4_2_6, 
  Sub s4_2_7]

s4_2_intro = Paragraph (S "The" +:+ (sLower (inModel ^. defn)) :+:
  S "s (" :+: (ordDiffEq ^. term) :+: S "s) that govern" +:+
  (progName ^. term) +:+ S "are" +:+ S "presented in" +:+ 
  (makeRef s4_2_5) :+: S ". The information to understand the" +:+
  S "meaning of the" +:+ (sLower (inModel ^. defn)) :+:
  S "s and their derivation is also presented, so that the" +:+
  (sLower (inModel ^. defn)) :+: S "s" +:+.
  S "can be verified")

-- General besides progName, repeated in only one other example but it could be 
-- used for all of them. So it can be abstracted out.

s4_2_1 = Section (assumption ^. defn :+: S "s") [Con s4_2_1_intro, 
  Con s4_2_1_list]

s4_2_1_intro = Paragraph (S "This section simplifies the original problem" +:+
  S "and helps in developing the" +:+ (sLower 
  (thModel ^. defn)) +:+ S "by filling in the missing" +:+
  S "information for the physical system. The numbers given in" +:+
  S "the square brackets refer to the" +:+ (sLower 
  (thModel ^. defn)) +:+ S "[" :+: (thModel ^. term) :+: 
  S "]," +:+ (sLower (genDefn ^. defn)) :+: 
  S " [" :+: (genDefn ^. term) :+: S "]," +:+ (sLower (dataDefn ^. defn)) +:+ S "[" :+: (dataDefn ^.
  term) :+: S "]," +:+ (sLower (inModel ^. defn)) +:+
  S "[" :+: (inModel ^. term) :+: S "], or" +:+ (sLower (likelyChg ^. defn)) +:+ S "[" :+: (likelyChg ^. 
  term) :+: S "], in which the respective" +:+
  (sLower (assumption ^. defn)) +:+. S "is used") 

-- General paragraph, repeated in every example. Can be abstracted out.

s4_2_1_list = Enumeration (Simple [((assumption ^. term) :+: S "1", Flat 
  (S "The only form of energy that is relevant for this problem" +:+
  S "is" +:+ (sLower (thermal_energy ^. term)) :+:
  S ". All other forms of energy, such as" +:+ (sLower 
  ((mech_energy ^. term))) :+: S ", are assumed to be" +:+
  S "negligible [" :+: (makeRef s4_2_2_T1) :+: S "].")),
--
  ((assumption ^. term) :+: S "2", Flat (S "All" +:+ sLower
  (heat_trans ^. term) +:+ S "coefficients are constant over" +:+
  (time ^. term) +:+. S "[GD1]")),
--
  ((assumption ^. term) :+: S "3", Flat (S "The" +:+ 
  (sLower ((water ^. term))) +:+ S "in the" +:+ 
  (sLower ((tank ^. term))) +:+ S "is fully mixed, so" +:+
  S "the" +:+ (temp_W ^. term) +:+ S "is the same throughout" +:+
  S "the entire" +:+ (sLower ((tank ^. term))) +:+
  S "[GD2" `sC` makeRef s4_2_4_DD2 :+: S "].")),
--
  ((assumption ^. term) :+: S "4", Flat (S "The" +:+ (temp_PCM ^.
  term) +:+ S "is the same throughout the" +:+ (pcm_vol ^. 
  term) +:+ S "[GD2" `sC` makeRef s4_2_4_DD2 `sC` S "LC1].")),
--
  ((assumption ^. term) :+: S "5", Flat (S "The" +:+ 
  (w_density ^. term) +:+ S "and" +:+ (pcm_density ^. term) +:+
  S "have no spatial variation; that is, they are each" +:+
  S "constant over their entire" +:+ (volume ^. term) +:+. 
  S "[GD2]")),
--
  ((assumption ^. term) :+: S "6", Flat (S "The" +:+ (htCap_W ^.
  term) :+: S "," +:+ (htCap_S_P ^. term) :+: S ", and" +:+ 
  (htCap_L_P ^. term) +:+ S "have no spatial variation; that" +:+
  S "is, they are each constant over their entire" +:+
  (volume ^. term) +:+. S "[GD2]")),
--
  ((assumption ^. term) :+: S "7", Flat ((law_conv_cooling ^.
  defn) +:+ S "applies between the" +:+ (sLower (
  (coil ^. term))) +:+ S "and the" +:+ (sLower (
  (water ^. term))) +:+ S "[" :+: makeRef s4_2_4_DD1 :+: S "].")),
--
  ((assumption ^. term) :+: S "8", Flat (S "The" +:+ (temp_C ^. 
  term) +:+ S "is constant over" +:+ (time ^. term) +:+
  S "[" :+: makeRef s4_2_4_DD1 `sC` S "LC2].")),
--
  ((assumption ^. term) :+: S "9", Flat (S "The" +:+ (temp_C ^.
  term) +:+ S "does not vary along its length [" :+:
  makeRef s4_2_4_DD1 `sC` S "LC3].")),
--
  ((assumption ^. term) :+: S "10", Flat ((law_conv_cooling ^. 
  defn) +:+ S "applies between the" +:+ (sLower (
  (water ^. term))) +:+ S "and the" +:+ (phsChgMtrl ^. term) +:+
  S "[" :+: makeRef s4_2_4_DD2 :+: S "].")),
--
  ((assumption ^. term) :+: S "11", Flat (S "The model only" +:+
  S "accounts for" +:+ (sLower (charging ^. defn)) :+:
  S ", not" +:+ (sLower ((discharging ^. term))) :+:
  S ". The" +:+ (temp_W ^. term) +:+ S "and" +:+ 
  (temp_PCM ^. term) +:+ S "can only increase, or remain" +:+
  S "constant; they do not decrease. This implies that the" +:+
  (temp_init ^. term) +:+ S "(A12) is less than (or equal)" +:+
  S "to the" +:+ (temp_C ^. term) +:+. S "[IM1, LC4]")),
--
  ((assumption ^. term) :+: S "12", Flat (S "The" +:+
  (temp_init ^. term) +:+ S "of the" +:+ (sLower ( 
  (water ^. term))) +:+ S "and the" +:+ (phsChgMtrl ^. term) +:+
  S "is the same" +:+. S "[IM1, IM2, LC5]")),
--
  ((assumption ^. term) :+: S "13", Flat (S "The simulation" +:+
  S "will start with the" +:+ (phsChgMtrl ^. term) +:+
  S "in a" +:+ (sLower (solid ^. defn)) +:+.
  S "[IM2, IM4]")),
--
  ((assumption ^. term) :+: S "14", Flat (S "The operating" +:+
  (temp ^. term) +:+ S "range of the system is" +:+ S "such" +:+
  S "that the" +:+ (sLower ((water ^. term))) +:+
  S "is always in" +:+ (liquid ^. defn) :+: S ". That is," +:+
  S "the" +:+ (temp ^. term) +:+ S "will not drop below the" +:+
  (temp_melt ^. term) +:+ S "of" +:+ (sLower 
  ((water ^. term))) :+: S ", or rise above its" +:+
  (temp_boil ^. term) +:+. S "[IM1, IM3]")),
--
  ((assumption ^. term) :+: S "15", Flat (S "The" +:+
  (sLower ((tank ^. term))) +:+ S "is" +:+ 
  (perfect_insul ^. term) +:+ S "so that there is no heat" +:+
  S "loss from the" +:+ (sLower ((tank ^. term))) +:+.
  S "[IM1, LC6]")),
--
  ((assumption ^. term) :+: S "16", Flat (S "No internal heat" +:+
  S "is generated by either the" +:+ (sLower 
  ((water ^. term))) +:+ S "or the" +:+ (phsChgMtrl ^.
  term) :+: S "; therefore, the" +:+ (vol_ht_gen ^. term) +:+.
  S "is zero [IM1, IM2]")),
--
  ((assumption ^. term) :+: S "17", Flat (S "The volume" +:+ 
  S "change of the" +:+ (phsChgMtrl ^. term) +:+ S "due to" +:+ 
  (sLower ((melting ^. term))) +:+.
  S "is negligible [IM2]")),
--
  ((assumption ^. term) :+: S "18", Flat (S "The" +:+ 
  (phsChgMtrl ^. term) +:+ S "is either in a" +:+
  (liquid ^. defn) +:+ S "or a" +:+ (solid ^. defn) +:+
  S "but not a" +:+ (gaseous ^. defn) +:+. S "[IM2, IM4]")),
--
  ((assumption ^. term) :+: S "19", Flat (S "The pressure in" +:+
  S "the" +:+ (sLower ((tank ^. term))) +:+
  S "is atmospheric, so the" +:+ (temp_melt ^. term) +:+
  S "and" +:+ (temp_boil ^. term) +:+ S "are 0" :+:
  Sy (temp ^. unit) +:+ S "and 100" :+: Sy (temp ^. unit) `sC`
  S "respectively [IM1, IM3]."))])

-- Again, list structure is same between all examples.

-- Can booktabs colored links be used? The box links completely cover nearby 
-- punctuation.

s4_2_2 = Section (thModel ^. defn :+: S "s") [Con s4_2_2_intro, 
  Con s4_2_2_T1, Con s4_2_2_T2, Con s4_2_2_T3]

s4_2_2_intro = Paragraph (S "This section focuses on the general equations" +:+
  S "and laws that" +:+ (progName ^. term) +:+.
  S "is based on")

-- General paragraph (besides progName), repeated in all examples. Can be 
-- abstracted out.

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

-- No subsubsubsections... may make things difficult for derivation sections
-- coming up

s4_2_3 = Section (genDefn ^. defn :+: S "s") 
  ((Con s4_2_3_intro):(map Con s4_2_3_deriv))

s4_2_3_intro = Paragraph (S "This section collects the laws and equations" +:+
  S "that will be used in deriving the" +:+ (sLower 
  (dataDefn ^. defn)) :+: S "s, which in turn are used to" +:+
  S "build the" +:+ (sLower (inModel ^. defn)) :+: 
  S "s. (General definitions are left out because they are not" +:+
  S "currently implemented in Drasil.)")

-- General paragraph, repeated in one other example but could be included in 
-- all. Can be abstracted out.
 
-- s4_2_3_GDs :: [LayoutObj]
-- s4_2_3_GDs = map Definition (map General [gd1NewtonCooling])

--General definitions not yet implemented

s4_2_3_deriv = [Paragraph (S "Detailed derivation of simplified rate of" +:+
  S "change of" +:+ (temp ^. term) :+: S ":"),
  Paragraph (S "Integrating" +:+ makeRef s4_2_2_T1 :+: 
  S " over a" +:+ (volume ^. term) +:+ S "(" :+:
  P (volume ^. symbol) :+: S "), we have:"),
  EqnBlock 
  ((Neg (UnaryOp (Integral (Just (Low (C volume)), Nothing)
  ((C gradient) :. (C thFluxVect)) volume))) + 
  UnaryOp (Integral (Just (Low (C volume)), Nothing) 
  (C vol_ht_gen) volume) := 
  UnaryOp (Integral (Just (Low (C volume)), Nothing) ((C density) 
  * (C htCap) * Deriv Part (C temp) (C time)) volume)),
  Paragraph (S "Applying" +:+ (gauss_div ^. term) +:+ S "to" +:+
  S "the first term over the" +:+ (surface ^. term) +:+ P (surface ^. symbol) +:+ S "of the" +:+ 
  (volume ^. term) :+: S ", with" +:+ P (thFluxVect ^. 
  symbol) +:+ S "as the" +:+ (thFluxVect ^. term) +:+
  S "for the" +:+ (surface ^. term) +:+ S "and" +:+
  P (norm_vect ^. symbol) +:+ S "as a" +:+ (norm_vect ^.
  defn) :+: S ":"),
  EqnBlock 
  ((Neg (UnaryOp (Integral (Just (Low (C surface)), Nothing) 
  ((C thFluxVect) :. (C norm_vect)) surface))) + 
  (UnaryOp (Integral (Just 
  (Low (C volume)), Nothing) (C vol_ht_gen) volume)) := 
  UnaryOp (Integral (Just (Low (C volume)), Nothing) 
  ((C density) * (C htCap) * Deriv Part (C temp) (C time)) volume)),
  Paragraph (S "We consider an arbitrary" +:+ (volume ^. 
  term) :+: S ". The" +:+ (vol_ht_gen ^. term) :+: S "is" +:+
  S "assumed constant. Then (1) can be written as:"),
  EqnBlock 
  ((C ht_flux_in) * (C in_SA) - (C ht_flux_out) * 
  (C out_SA) + (C vol_ht_gen) * (C volume) := UnaryOp (Integral 
  (Just (Low (C volume)), Nothing) ((C density) * (C htCap) *
  Deriv Part (C temp) (C time)) volume)),
  Paragraph (S "Where" +:+ P (ht_flux_in ^. symbol) :+: S "," +:+
  P (ht_flux_out ^. symbol) :+: S "," +:+ P (in_SA ^. symbol) :+:
  S ", and" +:+ P (out_SA ^. symbol) +:+ S "are explained in" +:+
  S "GD2. Assuming" +:+ P (density ^. symbol) :+: S "," +:+
  P (htCap ^. symbol) +:+ S "and" +:+ P (temp ^. symbol) +:+
  S "are constant over the" +:+ (volume ^. term) :+: S "," +:+ 
  S "which is true in our case by" +:+ (assumption ^. defn) :+:
  S "s (A3), (A4), (A5), and (A6), we have:"),
  EqnBlock 
  ((C density) * (C htCap) * (C volume) * Deriv Total (C temp) 
  (C time) := (C ht_flux_in) * (C in_SA) - (C ht_flux_out) * 
  (C out_SA) + (C vol_ht_gen) * (C volume)),
  Paragraph (S "Using the fact that" +:+ P (density ^. symbol) :+:
  S "=" :+: P (mass ^. symbol) :+: S "/" :+: 
  P (volume ^. symbol) :+: S ", (2) can be written as:"),
  EqnBlock 
  ((C mass) * (C htCap) * Deriv Total (C temp) (C time) :=
  (C ht_flux_in) * (C in_SA) - (C ht_flux_out) * (C out_SA) + 
  (C vol_ht_gen) * (C volume))]

-- Created a unitalChunk for "S"... should I add it to table of symbols?
-- Add references to above when available (assumptions, GDs)
-- Replace relevant Derivs with the regular derivative when it is available

s4_2_4 = Section (dataDefn ^. defn :+: S "s") [Con s4_2_4_intro, 
  Con s4_2_4_DD1, Con s4_2_4_DD2, Con s4_2_4_DD3]

s4_2_4_intro = Paragraph (S "This section collects and defines all the" +:+
  S "data needed to build the" +:+ (sLower (inModel ^.
  defn)) :+: S "s. The dimension of each quantity is also given.")

-- General paragraph, repeated in most examples but would work for all. Can be 
-- absracted out.

s4_2_5 = Section (inModel ^. defn :+: S "s") ((map Con s4_2_5_intro) ++ 
  (map Con s4_2_5_deriv1) ++ (map Con s4_2_5_deriv2))

s4_2_5_intro = [Paragraph (S "This section transforms the problem defined" +:+
  S "in" +:+ (makeRef s4_1) +:+ S "into one which" +:+
  S "is expressed in mathematical terms. It uses concrete" +:+
  S "symbols defined in" +:+ (makeRef s4_2_4) +:+
  S "to replace the abstract symbols in the models identified" +:+
  S "in" +:+ (makeRef s4_2_2) +:+ S "and" +:+. (makeRef s4_2_3)), 
  Paragraph (S "The goals GS1 to GS4 are solved by IM1 to IM4." +:+
  S "The solutions for IM1 and IM2 are coupled since the" +:+
  S "solution for" +:+ P (temp_W ^. symbol) +:+ S "and" +:+
  P (temp_PCM ^. symbol) +:+ S "depend on one another. IM3" +:+
  S "can be solved once IM1 has been solved. The solution of" +:+
  S "IM2 and IM4 are also coupled, since the" +:+ 
  (temp_PCM ^. term) +:+ S "and" +:+ (pcm_E ^. term) +:+
  S "depend on the" +:+ (sLower (phase_change ^. 
  term)) :+: S ". (Instance models are left out because they" +:+
  S "are not currently implemented in Drasil.)")]

-- The first paragraph is completely general and repeated in other examples. 
-- The second paragraph is very specific, and the other examples don't even 
-- include a paragraph analogous to this one.

-- Instance Models aren't implemented yet

s4_2_5_deriv1 = [Paragraph (S "Derivation of the energy balance on" +:+ 
  (sLower ((water ^. term))) :+: S ":"),
  Paragraph (S "To find the rate of change of" +:+ P (temp_W ^.
  symbol) :+: S ", we look at the energy balance on" +:+
  (sLower ((water ^. term))) :+: S ". The" +:+ 
  (volume ^. term) +:+ S "being considered is the" +:+
  (w_vol ^. term) :+: S "" +:+ P (w_vol ^. symbol) :+:
  S ", which has" +:+ (w_mass ^. term) :+: S "" +:+ 
  P (w_mass ^. symbol) +:+ S "and" +:+ (htCap_W ^. term) :+: 
  S "," +:+. P (htCap_W ^. symbol) +:+ P (ht_flux_C ^. 
  symbol) +:+ S "represents the" +:+ (ht_flux_C ^. term) +:+
  S "and" +:+ P (ht_flux_P ^. symbol) +:+ S "represents" +:+
  S "the" +:+ (ht_flux_P ^. term) :+: S ", over" +:+
  (coil_SA ^. term) +:+ S "and" +:+ (pcm_SA ^. term) +:+
  S "of" +:+ P (coil_SA ^. symbol) +:+ S "and" +:+ 
  P (pcm_SA ^. symbol) :+: S ", respectively. No" +:+ sLower
  (heat_trans ^. term) +:+ S "occurs to the outside of the" +:+
  (sLower ((tank ^. term))) :+: S ", since it" +:+
  S "has been assumed to be" +:+ (perfect_insul ^. term) :+: 
  S " (A15)." +:+ S "Assuming no" +:+ (vol_ht_gen ^. term) :+: 
  S " (A16)," +:+ P (vol_ht_gen ^. symbol) :+: S "=0." +:+
  S "Therefore, the equation for GD2 can be written as:"),
  EqnBlock 
   ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) (C time) 
   := (C ht_flux_C) * (C coil_SA) - (C ht_flux_P) * (C pcm_SA)),
  Paragraph(S "Using" +:+ makeRef s4_2_4_DD1 +:+ S "and" +:+
  makeRef s4_2_4_DD2 +:+ S "for" +:+ P (ht_flux_C ^. symbol) +:+
  S "and" +:+ P (ht_flux_P ^. symbol) +:+ S "respectively," +:+
  S "this can be written as:"),
  EqnBlock 
   ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) (C time) 
   := (C coil_HTC) * (C coil_SA) * ((C temp_C) - (C temp_W)) -
   (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM))),
  Paragraph (S "Dividing (3) by" +:+ P (w_mass ^. symbol) :+:
  P (htCap_W ^. symbol) :+: S ", we obtain:"),
  EqnBlock 
   (Deriv Total (C temp_W) (C time) := ((C coil_HTC) * 
   (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - 
   (C temp_W)) - ((C pcm_mass) * (C pcm_SA)) / ((C w_mass) *
   (C htCap_W)) * ((C temp_W) - (C temp_PCM))),
  Paragraph (S "Factoring the negative sign out of the second" +:+
  S "term of the" +:+ (rightSide ^. term) +:+ S "of" +:+
  S "Equation (4) and multiplying it by" +:+ 
  P (coil_HTC ^. symbol) :+: P (coil_SA ^. symbol) :+: S "/" :+: 
  P (coil_HTC ^. symbol) :+: P (coil_SA ^. symbol) :+: 
  S " yields:"),
  EqnBlock 
   (Deriv Total (C temp_W) (C time) := ((C coil_HTC) * 
   (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - 
   (C temp_W)) + ((C coil_HTC) * (C coil_SA)) / ((C coil_HTC) * 
   (C coil_SA)) * ((C pcm_HTC) * (C pcm_SA)) / ((C w_mass) * 
   (C htCap_W)) * ((C temp_PCM) - (C temp_W))),
  Paragraph (S "Which simplifies to:"),
  EqnBlock 
   (Deriv Total (C temp_W) (C time) := ((C coil_HTC) * 
   (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) -
   (C temp_W)) + ((C pcm_HTC) * (C pcm_SA)) / ((C coil_HTC) *
   (C coil_SA)) * ((C coil_HTC) * (C coil_SA)) / ((C w_mass) * 
   (C htCap_W)) * ((C temp_PCM) - (C temp_W))),
  Paragraph (S "Setting" +:+ P (tau_W ^. symbol) :+: S "=" :+:
  P (w_mass ^. symbol) :+: P (htCap_W ^. symbol) :+: S "/" :+:
  P (coil_HTC ^. symbol) :+: P (coil_SA ^. symbol) :+: 
  S " and" +:+ P (eta ^. symbol) :+: S "=" :+: P (pcm_HTC ^. 
  symbol) :+: P (pcm_SA ^. symbol) :+: S "/" :+: P (coil_HTC ^.
  symbol) :+: P (coil_SA ^. symbol) :+: S ", Equation (5) can" +:+
  S "be written as:"),
  EqnBlock
   (Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) *
   ((C temp_C) - (C temp_W)) + ((C eta) / (C tau_W)) *
   ((C temp_PCM) - (C temp_W))),
  Paragraph (S "Finally, factoring out 1/" :+: P (tau_W ^. 
  symbol) :+: S ", we are left with the governing" +:+
  (ordDiffEq ^. term) +:+ S "for IM1:"),
  EqnBlock
   (Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) *
   (((C temp_C) - (C temp_W)) + (C eta) * ((C temp_PCM) - 
   (C temp_W))))
  ]

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Fractions in paragraph?

s4_2_5_deriv2 = [Paragraph (S "Detailed derivation of the energy balance on" +:+
  S "the" +:+ (phsChgMtrl ^. term) +:+ S "during" +:+ 
  (sLower (sens_heat ^. term)) :+: S "ing phase:"),
  Paragraph (S "To find the rate of change of" +:+ P (temp_PCM ^.
  symbol) :+: S ", we look at the energy balance on the" +:+ 
  (phsChgMtrl ^. term) :+: S ". The" +:+ (volume ^. term) +:+
  S "being considered is the" +:+ (pcm_vol ^. term) :+: 
  S "," +:+ P (pcm_vol ^. symbol) :+: S ". The derivation" +:+
  S "that follows is initially for the" +:+ (sLower 
  ((solid ^. term))) :+: S "" +:+ (phsChgMtrl ^. term) :+:
  S ". The" +:+ (pcm_mass ^. term) +:+ S "is" +:+ 
  P (pcm_mass ^. symbol) +:+ S "and the" +:+ (htCap_S_P ^. 
  term) +:+ S "is" +:+. P (htCap_S_P ^. symbol) +:+
  S "The" +:+ (ht_flux_P ^. term) +:+ S "is" +:+ 
  P (ht_flux_P ^. symbol) +:+ S "over" +:+ (pcm_SA ^. term) +:+ P (pcm_SA ^. symbol) :+: S ". There is no" +:+ 
  (ht_flux_out ^. term) :+: S ". Assuming no" +:+ (vol_ht_gen ^.
  term) +:+ S "(A16)," +:+ P (vol_ht_gen ^. symbol) :+: 
  S "=0, the equation for GD2 can be written as:"),
  EqnBlock 
   ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM) 
   (C time) := (C ht_flux_P) * (C pcm_SA)),
   Paragraph (S "Using" +:+ makeRef s4_2_4_DD2 +:+ S "for" +:+
   P (ht_flux_P ^. symbol) :+: S ", this equation can be" +:+
   S "written as:"),
  EqnBlock 
   ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM) 
   (C time) := (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - 
   (C temp_PCM))),
   Paragraph (S "Dividing by" +:+ P (pcm_mass ^. symbol) :+:
   P (htCap_S_P ^. symbol) +:+ S "we obtain:"),
  EqnBlock
   (Deriv Total (C temp_PCM) (C time) := ((C pcm_HTC) * 
   (C pcm_SA)) / ((C pcm_mass) * (C htCap_S_P)) * ((C temp_W) - 
   (C temp_PCM))),
  Paragraph (S "Setting" +:+ P (tau_S_P ^. symbol) :+: S "=" :+:
  P (pcm_mass ^. symbol) :+: P (htCap_S_P ^. symbol) :+: S "/" :+:
  P (pcm_HTC ^. symbol) :+: P (pcm_SA ^. symbol) :+: S "," +:+
  S "this can be written as:"),
  EqnBlock 
   (Deriv Total (C temp_PCM) (C time) := (1 / (C tau_S_P)) *
   ((C temp_W) - (C temp_PCM))),
  Paragraph (S "Equation (6) applies for the" +:+ (sLower ((solid ^. term))) :+: S "" +:+ (phsChgMtrl ^. 
  term) :+: S ". In the case where all of the" +:+
  (phsChgMtrl ^. term) +:+ S "is melted, the same" +:+
  S "derivation applies, except that" +:+ P (htCap_S_P ^. 
  symbol) +:+ S "is replaced by" +:+ P (htCap_L_P ^. symbol) :+:
  S ", and thus" +:+ P (tau_S_P ^. symbol) +:+ S "is" +:+ 
  S "replaced by" +:+. P (tau_L_P ^. symbol) +:+
  S "Although a small change in surface area would be" +:+
  S "expected with" +:+ (sLower ((melting ^. 
  term))) :+: S ", this is not included, since the" +:+
  (volume ^. term) +:+ S "change of the" +:+ (phsChgMtrl ^. 
  term) +:+ S "with" +:+ (sLower ((melting ^. 
  term))) +:+ S "is assumed to be negligible (A17)."),
  Paragraph (S "In the case where" +:+ P (temp_PCM ^. symbol) :+:
  S "=" :+: P (temp_melt_P ^. symbol) +:+ S "and not all of" +:+
  S "the" +:+ (phsChgMtrl ^. term) +:+ S "is melted, the" +:+
  (temp_PCM ^. term) +:+ S "does not change. Therefore, in" +:+
  S "this case d" :+: P (temp_PCM ^. symbol) :+: S "/d" :+:
  P (time ^. symbol) :+: S "=0."),
  Paragraph (S "This derivation does not consider the" +:+ 
  (sLower ((boiling ^. term))) +:+ S "of the" +:+ 
  (phsChgMtrl ^. term) :+: S ", as the" +:+ (phsChgMtrl ^. 
  term) +:+ S "is assumed to either be in" +:+ S "a" +:+
  (solid ^. defn) +:+ S "or a" +:+ (liquid ^. defn) :+: 
  S " (A18).")]

-- Add GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Derivative notation in paragraph?

s4_2_6 = Section (S "Data Constraints") [Con s4_2_6_intro]

s4_2_6_intro = Paragraph (S "Tables 1 and 2 show the data" +:+
  S "constraints on the input and output variables," +:+
  S "respectively. The column for physical constraints gives" +:+ 
  S "the physical limitations on the range of values that can" +:+
  S "be taken by the variable. The column for software" +:+
  S "constraints restricts the range of inputs to reasonable" +:+
  S "values. The constraints are conservative, to give the" +:+
  S "user of the model the flexibility to experiment with" +:+
  S "unusual situations. The column of typical values is" +:+
  S "intended to provide a feel for a common scenario. The" +:+
  S "uncertainty column provides an estimate of the confidence" +:+
  S "with which the physical quantities can be measured. This" +:+
  S "information would be part of the input if one were" +:+
  S "performing an uncertainty quantification exercise. (The" +:+
  S "tables are left out because features they should use are" +:+
  S "not yet implemented in Drasil.)")

-- General paragraph, repeated between examples. Can be abstracted out.

-- I do not think Table 2 will end up being necessary for the Drasil version
---- The info from table 2 will likely end up in table 1.

inputVar :: [UnitalChunk]
inputVar = [tank_length, diam, pcm_vol, pcm_SA, pcm_density, temp_melt_P,
  htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C, w_density, htCap_W, 
  coil_HTC, pcm_HTC, temp_init, time_final]
  
-- Typical values and constraints must be added to UC definitions for mkTable 
-- to work here.

-- table1 = Table [S "Var", S "Physical Constraints", S "Software Constraints",
  -- S "Typical Value", S "Uncertainty"] (mkTable
  -- [\ch -> P (ch ^. symbol),
  -- \ch -> Sy (ch ^. unit),
  -- \ch -> Sy (ch ^. unit),
  -- \ch -> Sy (ch ^. unit),
  -- \ch -> Sy (ch ^. unit)] inputVar)
  -- (S "Input Variables") True

-- Add constraints (and typical values) to the knowledge capture of each 
-- variable, so that lambdas can be used to extract constraints?
-- Add "Uncertainty" to UnitalChunks??
-- Other Notes:
---- Will there be a way to have asterisks for certain pieces of the table?

--Tables 2 and 3 will be delayed for now bc they are similar to table 1

s4_2_7 = Section (S "Properties of a Correct Solution") (map Con s4_2_7_deriv)

s4_2_7_deriv = [Paragraph (S "A correct solution must exhibit the" +:+ 
  (sLower (law_cons_energy ^. term)) :+:
  S ". This means that the" +:+ (w_E ^. term) +:+
  S "should equal the difference between" +:+
  S " the total energy input from the" +:+ (sLower 
  ((coil ^. term))) +:+ S "and the energy output to the" +:+
  (phsChgMtrl ^. term) :+: S ". This can be shown as an" +:+
  S "equation by taking" +:+ makeRef s4_2_4_DD1 +:+ S "and" +:+
  makeRef s4_2_4_DD2 :+: S ", multiplying each by their" +:+
  S "respective surface area of" +:+ (heat_trans ^. term) :+:
  S ", and integrating each over the simulation" +:+ (time ^. 
  term) :+: S ", as follows:"),
  EqnBlock 
  ((C w_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time))) 
  ((C coil_HTC) * (C coil_SA) * ((C temp_C) - FCall (C temp_W) 
  [C time])) time) - UnaryOp (Integral (Just (Low 0), Just (High (C time))) 
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) -
  (FCall (C temp_PCM) [C time]))) time)),
  Paragraph (S "In addition, the" +:+ (pcm_E ^. term) :+: 
  S " should equal the energy input to the" +:+ (phsChgMtrl ^. 
  term) +:+ S "from the" +:+ (sLower ((water ^.
  term))) :+: S ". This can be expressed as"),
  EqnBlock
  ((C pcm_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) - (FCall
  (C temp_PCM) [C time]))) time)),
  Paragraph (S "Equations (reference) and (reference) can be" +:+
  S "used as" +:+ Quote (S "sanity") :+: S "checks to gain" +:+ 
  S "confidence in any solution computed by" +:+ (progName ^. 
  term) :+: S ". The relative error between the results" +:+
  S "computed by" +:+ (progName ^. term) +:+ S "and the" +:+
  S "results calculated from the" +:+ (rightSide ^. term) :+: 
  S " of these equations should be less than 0.001% (R9).")]

-- Above section only occurs in this example (although maybe it SHOULD be in 
-- the others).

-- Remember to insert references in above derivation when available

s5 = Section ((requirement ^. defn) :+: S "s") [Con s5_intro, Sub s5_1, 
  Sub s5_2]

s5_intro = Paragraph (S "This section provides the functional" +:+ (sLower
  (requirement ^. defn)) :+: S "s, the business tasks" +:+
  S "that the software is expected to complete, and the" +:+
  S "nonfunctional" +:+ (sLower (requirement ^. 
  defn)) :+:S "s, the qualities that the software is expected to" +:+
  S "exhibit.")

-- General paragraph, repeated in every example. Can be abstracted out.

s5_1 = Section (S "Functional" +:+ (requirement ^. defn) :+: S "s") 
  (map Con s5_1_list)

s5_1_list = [Enumeration (Simple [((requirement ^. term) :+: S "1", Flat 
  (S "Input the following quantities, which define the" +:+
  (sLower ((tank ^. term))) :+: 
  S " parameters, material properties and initial conditions:"))]), 
  (Table [S "symbol", S "unit", S "description"] (mkTable
  [(\ch -> P (ch ^. symbol)),
  (\ch -> Sy (ch ^. unit)),
  (\ch -> ch ^. term)] inputVar) 
  (S "Input Variable" +:+ (requirement ^. defn)) False),
--
  Enumeration (Simple [((requirement ^. term) :+: S "2", Flat 
  (S "Use the inputs in R1 to find the" +:+ (mass ^. term) +:+
  S "needed for IM1 to IM4, as follows, where" +:+
  P (w_vol ^. symbol) +:+ S "is the" +:+(w_vol ^. term) +:+
  S "and" +:+ P (tank_vol ^. symbol) +:+ S "is the" +:+.
  (tank_vol ^. term)))]),
  EqnBlock ((C w_mass) := (C w_vol) * (C w_density) := ((C tank_vol) -
  (C pcm_vol)) * (C w_density) := (((C diam) / 2) * (C tank_length) - 
  (C pcm_vol)) * (C w_density)),
  EqnBlock ((C pcm_mass) := (C pcm_vol) * (C pcm_density)),
--
  Enumeration (Simple [((requirement ^. term) :+: S "3", Flat 
  (S "Verify that the inputs satisfy the required physical" +:+
  S "constraints shown in Table 1.")),
--
  ((requirement ^. term) :+: S "4", Flat (S "Output the input" :+: 
  S " quantities and derived quantities in the following list: "  :+:
  S "the quantities from R1, the" +:+ (mass ^. term) :+: S "es" +:+
  S "from R2," +:+ P (tau_W ^. symbol) +:+ S "(from IM1)," +:+ 
  P (eta ^. symbol) +:+ S "(from IM1)," +:+ P (tau_S_P ^. 
  symbol) +:+ S "(from IM2) and" +:+ P (tau_L_P ^. symbol) +:+
  S "(from IM2).")),
--
  ((requirement ^. term) :+: S "5", Flat (S "Calculate and" +:+
  S "output the" +:+ (temp_W ^. term) +:+ S "(" :+: P (temp_W ^.
  symbol) :+: S "(" :+: P (time ^. symbol) :+: S "))" +:+
  S "over the simulation" +:+ (time ^. term) +:+ S "(from IM1).")),
--
  ((requirement ^. term) :+: S "6", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (temp_PCM ^. term) +:+ S "(" :+:
  P (temp_PCM ^. symbol) :+: S "(" :+: P (time ^. symbol) :+:
  S ")) over the simulation" +:+ (time ^. term) :+: 
  S " (from IM2).")),
--
  ((requirement ^. term) :+: S "7", Flat (S "Calculate and" +:+ 
  S " output the" +:+ (w_E ^. term) +:+ S "(" :+: P (w_E ^. 
  symbol) :+: S "(" :+: P (time ^. symbol) :+: S "))" +:+
  S "over the simulation" +:+ (time ^. term) +:+ S "(from IM3).")),
--
  ((requirement ^. term) :+: S "8", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (pcm_E ^. term) +:+ S "(" :+: P (pcm_E ^.
  symbol) :+: S "(" :+: P (time ^. symbol) :+: S ")) over the" +:+
  S "simulation" +:+ (time ^. term) +:+ S "(from IM4).")),
--
  ((requirement ^. term) :+: S "9", Flat (S "Verify that the" +:+
  S "energy outputs (" :+: P (w_E ^. symbol) :+: S "(" :+: P (time ^. 
  symbol) :+: S ") and" +:+ P (pcm_E ^. symbol) :+: S "(" :+:
  P (time ^. symbol) :+: S ")) follow the" +:+ (sLower 
  (law_cons_energy ^. term)) :+: S ", as outlined in" +:+ 
  makeRef s4_2_7 :+: S ", with relative error no greater than" +:+
  S "0.001%.")),
--
  ((requirement ^. term) :+: S "10", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (time ^. term) +:+ S "at which the" +:+ 
  (phsChgMtrl ^. term) +:+ S "begins to melt" +:+
  P (t_init_melt ^. symbol) +:+ S "(from IM2).")),
--
  ((requirement ^. term) :+: S "11", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (time ^. term) +:+ S "at which the" +:+ 
  (phsChgMtrl ^. term) +:+
  S "stops" +:+ (sLower ((melting ^. term))) :+: 
  S "" +:+ P (t_final_melt ^. symbol) +:+ S "(from IM2)."))])
  ]

-- List structure same between all examples

--How to include pi?
--How to add exponents?

s5_2 = Section (S "Nonfunctional" +:+ (requirement ^. defn) :+: S "s") 
  [Con s5_2_contents]

s5_2_contents = Paragraph (S "Given the small size, and relative simplicity" `sC`
  S "of this problem, performance is not a priority. Any" +:+
  S "reasonable implementation will be very quick and use" +:+
  S "minimal storage. Rather than performance, the priority" +:+
  S "nonfunctional" +:+ (sLower (requirement ^. 
  defn)) :+: S "s are correctness, verifiability" `sC`
  S "understandability, reusability, and maintainability.")

-- The second sentence of the above paragraph is repeated in all examples (not 
-- exactly, but the general idea is). The first sentence is not always 
-- repeated, but it is always either stating that performance is a priority or 
-- performance is not a priority. This is probably something that can be 
-- abstracted out.

s6 = Section ((likelyChg ^. defn) :+: S "s") [Con s6_list]

-- The game physics example has a short intro paragraph that can likely be 
-- abstracted out and used for all examples.

s6_list = Enumeration (Simple [((likelyChg ^. term) :+: S "1", Flat 
  (S "A4 -" +:+ (phsChgMtrl ^. term) +:+ S "is actually a poor" +:+
  (sLower (thermal_conductor ^. term)) :+: S ", so" +:+
  S "the" +:+ (sLower (assumption ^. defn)) +:+
  S "of uniform" +:+ (temp_PCM ^. term) +:+. S "is not likely")),
--
  ((likelyChg ^. term) :+: S "2", Flat (S "A8 - The" +:+ (temp_C ^. 
  term) +:+ S "will change over the course of the day, depending" +:+.
  S "on the energy received from the sun")),
--
  ((likelyChg ^. term) :+: S "3", Flat (S "A9 - The" +:+ (temp_C ^. 
  term) +:+ S "will actually change along its length as the" +:+
  (sLower ((water ^. term))) +:+. S "within it cools")),
--
  ((likelyChg ^. term) :+: S "4", Flat (S "A11 - The model" +:+
  S "currently only accounts for" +:+. (sLower (charging ^. 
  defn)) +:+ S "A more complete model would also account for" +:+.
  (sLower (discharging ^. defn)))),
--
  ((likelyChg ^. term) :+: S "5", Flat (S "A12 - To add more" +:+
  S " flexibility to the simulation, the" +:+ (temp_init ^. term) +:+
  S "of the" +:+ (sLower ((water ^. term))) :+: 
  S " and the" +:+ (phsChgMtrl ^. term) +:+ S "could be" +:+.
  S "allowed to have different values")),
--
  ((likelyChg ^. term) :+: S "6", Flat (S "A15 - Any real" +:+
  (sLower ((tank ^. term))) +:+ S "cannot be" +:+
  (perfect_insul ^. term) +:+. S "and will lose heat"))])

-- List structure same in all examples.

--add referencing to assumptions?
  
s7 = Section (S "Traceability Matrices and Graphs") ([Con s7_intro1, 
  Con s7_table1, Con s7_table2, Con s7_table3] ++ (map Con s7_intro2) ++ 
  [Con s7_fig1, Con s7_fig2])

s7_intro1 = Paragraph (S "The purpose of the traceability matrices is to" +:+ 
  S "provide easy references on what has to be additionally" +:+
  S "modified if a certain component is changed. Every time a" +:+
  S "component is changed, the items in the column of that" +:+
  S "component that are marked with an" +:+ Quote (S "X") :+: 
  S " should be modified as well." +:+ makeRef s7_table1 +:+
  S "shows the dependencies of" +:+ (sLower (thModel ^. 
  defn)) :+: S "s," +:+ (sLower (genDefn ^. defn)) :+:
  S "s," +:+ (sLower (dataDefn ^. defn)) :+: S "s," +:+
  S "and" +:+ (sLower (inModel ^. defn)) :+: S "s" +:+
  S "with each other." +:+ makeRef s7_table2 +:+ S "shows the" +:+
  S "dependencies of" +:+ (sLower (inModel ^. defn)) :+:
  S "s," +:+ (sLower (requirement ^. defn)) :+:
  S "s, and data constraints on each other." +:+ 
  makeRef s7_table3 +:+ S "shows the dependencies of" +:+ 
  (sLower (thModel ^. defn)) :+: S "s," +:+
  (sLower (genDefn ^. defn)) :+: S "s," +:+ 
  (sLower (dataDefn ^. defn)) :+: S "s," +:+
  (sLower (inModel ^. defn)) :+: S "s, and" +:+ 
  (sLower (likelyChg ^. defn)) :+: S "s on the" +:+
  (assumption ^. defn) :+: S "s.")

-- Completely general paragraph, and similar ones in other example, but slight 
-- differences in what is included in each matrix. Perhaps we can abstract out 
-- which types of items are associated with each matrix i.e. instance models, 
-- assumptions, requirements, etc.. If so, this paragraph can be abstracted out.

s7_table1 = Table [S "", makeRef s4_2_2_T1, makeRef s4_2_2_T2, 
  makeRef s4_2_2_T3, S "GD1", S "GD2", makeRef s4_2_4_DD1, 
  makeRef s4_2_4_DD2, makeRef s4_2_4_DD3, makeRef s4_2_4_DD3, S "IM1",
  S "IM2", S "IM3", S "IM4"]
  [[makeRef s4_2_2_T1, S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S ""],
  [makeRef s4_2_2_T2, S "", S "", S "X", S "", S "", S "", S "", S "",
  S"", S "", S "", S "", S ""],
  [makeRef s4_2_2_T3, S "", S "", S "", S "", S "", S "", S "", S "", 
  S "", S "", S "", S "", S ""],
  [S "GD1", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S ""],
  [S "GD2", S "X", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S ""],
  [makeRef s4_2_4_DD1, S "", S "", S "", S "X", S "", S "", S "",
  S "", S "", S "", S "", S "", S ""],
  [makeRef s4_2_4_DD2, S "", S "", S "", S "X", S "", S "", S "",
  S"", S "", S "", S "", S "", S ""],
  [makeRef s4_2_4_DD3, S "", S "", S "", S "", S "", S "", S "", S "",
  S"", S "", S "", S "", S ""],
  [makeRef s4_2_4_DD3, S "", S "", S "", S "", S "", S "", S "",
  S "X", S "", S "", S "", S "", S ""],
  [S "IM1", S "", S "", S "", S "", S "X", S "X", S "X", S "", S "",
  S "", S "X", S "", S ""],
  [S "IM2", S "", S "", S "", S "", S "X", S "", S "X", S "", S "X",
  S "X", S "", S "", S "X"],
  [S "IM3", S "", S "X", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S ""],
  [S "IM4", S "", S "X", S "X", S "", S "", S "", S "X", S "X", S "X",
  S "", S "X", S "", S ""]]
  (S "Traceability Matrix Showing the Connections Between Items" +:+
  S "of Different Sections") True

-- Wrong DD reference above, change when DD4 is available (twice)

s7_table2 = Table [S "", S "IM1", S "IM2", S "IM3", S "IM4", makeRef s4_2_6,
  S "R1", S "R2"]
  [[S "IM1", S "", S "X", S "", S "", S "", S "X", S "X"],
  [S "IM2", S "X", S "", S "", S "X", S "", S "X", S "X"],
  [S "IM3", S "", S "", S "", S "", S "", S "X", S "X"],
  [S "IM4", S "", S "X", S "", S "", S "", S "X", S "X"],
  [S "R1", S "", S "", S "", S "", S "", S "", S ""],
  [S "R2", S "", S "", S "", S "", S "", S "X", S ""],
  [S "R3", S "", S "", S "", S "", S "X", S "", S ""],
  [S "R4", S "X", S "X", S "", S "", S "", S "X", S "X"],
  [S "R5", S "X", S "", S "", S "", S "", S "", S ""],
  [S "R6", S "", S "X", S "", S "", S "", S "", S ""],
  [S "R7", S "", S "", S "X", S "", S "", S "", S ""],
  [S "R8", S "", S "", S "", S "X", S "", S "", S ""],
  [S "R9", S "", S "", S "X", S "X", S "", S "", S ""],
  [S "R10", S "", S "X", S "", S "", S "", S "", S ""],
  [S "R11", S "", S "X", S "", S "", S "", S "", S ""]]
  (S "Traceability Matrix Showing the Connections Between" +:+
  (requirement ^. defn) :+: S "s and" +:+ (inModel ^. defn) :+:
  S "s") True

s7_table3 = Table [S "", S "A1", S "A2", S "A3", S "A4", S "A5", S "A6", S "A7",
  S "A8", S "A9", S "A10", S "A11", S "A12", S "A13", S "A14",
  S "A15", S "A16", S "A17", S "A18", S "A19"]
  [[makeRef s4_2_2_T1, S "X", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", 
  S ""],
  [makeRef s4_2_2_T2, S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [makeRef s4_2_2_T3, S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "GD1", S "", S "X", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "GD2", S "", S "", S "X", S "X", S "X", S "X", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [makeRef s4_2_4_DD1, S "", S "", S "", S "", S "", S "", S "X",
  S "X", S "X", S "", S "", S "", S "", S "", S "", S "", S "" , 
  S "", S ""],
  [makeRef s4_2_4_DD2, S "", S "", S "X", S "X", S "", S "", S "",
  S "", S "", S "X", S "", S "", S "", S "", S "", S "", S "", S "",
  S ""],
  [makeRef s4_2_4_DD3, S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [makeRef s4_2_4_DD3, S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "IM1", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "X", S "X", S "", S "X", S "X", S "X", S "", S "", S "X"],
  [S "IM2", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "X", S "X", S "", S "", S "X", S "X", S "X", S ""],
  [S "IM3", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "X", S "", S "", S "", S "", S "X"],
  [S "IM4", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "", S "X", S "", S "", S "", S "", S "X", S ""],
  [S "LC1", S "", S "", S "", S "X", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "LC2", S "", S "", S "", S "", S "", S "", S "", S "X", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "LC3", S "", S "", S "", S "", S "", S "", S "", S "", S "X",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "LC4", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "X", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "LC5", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S "", S "", S "X", S "", S "", S "", S "", S "", S "", S ""],
  [S "LC6", S "", S "", S "", S "", S "", S "", S "", S "", S "", 
  S "", S "", S "", S "", S "", S "X", S "", S "", S "", S ""]]
  (S "Traceability Matrix Showing the Connections Between" +:+
  (assumption ^. defn) :+: S "s and Other Items") True

-- These matrices can probably be generated automatically when enough info is 
-- abstracted out.

-- Wrong DD reference above, change when DD4 is available

s7_intro2 = [Paragraph (S "The purpose of the traceability graphs is also" +:+
  S "to provide easy references on what has to be additionally" +:+
  S "modified if a certain component is changed. The arrows in" +:+
  S "the graphs represent dependencies. The component at the tail" +:+
  S "of an arrow is depended on by the component at the head of" +:+
  S "that arrow. Therefore, if a component is changed, the" +:+
  S "components that it points to should also be changed." +:+
  makeRef s7_fig1 +:+ S "shows the dependencies of" +:+
  (sLower (thModel ^. defn)) :+: S "s," +:+
  (sLower (genDefn ^. defn)) :+: S "s," +:+
  (sLower (dataDefn ^. defn)) :+: S "s," +:+
  (sLower (inModel ^. defn)) :+: S "s," +:+
  (sLower (likelyChg ^. defn)) :+: S "s, and" +:+
  (sLower (assumption ^. defn)) :+: S "s on each" +:+
  S "other." +:+ makeRef s7_fig2 +:+ S "shows the dependencies" +:+
  S "of" +:+ (sLower (inModel ^. defn)) :+: S "s," +:+
  (sLower (requirement ^. defn)) :+: S "s, and data" +:+
  S "constraints on each other."),
  Paragraph (S "NOTE: Building a tool to automatically generate" +:+
  S "the graphical representation of the matrix by scanning the" +:+
  S "labels and reference can be future work.")]

-- Same comments on this paragraph as I had for s7_intro1. 

s7_fig1 = Figure (S "Traceability Graph Showing the Connections Between" +:+
  S "Items of Different Sections") "ATrace.png"

s7_fig2 = Figure (S "Traceability Graph Showing the Connections Between" +:+
  (requirement ^. defn) :+: S "s," +:+ (inModel ^. defn) :+: 
  S "s, and Data Constraints") "RTrace.png"

--References?

