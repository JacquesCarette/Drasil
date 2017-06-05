module Drasil.SWHS.Body where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Data.Drasil.SI_Units 
import Data.Drasil.Authors

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.PhysicalProperties hiding (density, mass, vol)
import qualified Data.Drasil.Concepts.Thermodynamics as CT
import Data.Drasil.Concepts.Physics (mech_energy)
import Data.Drasil.Concepts.Math (ode, unit_, rOfChng, equation, change)

import Data.Drasil.Concepts.Software (program, performance)
import Data.Drasil.Software.Products
import Data.Drasil.Utils (enumSimple, foldle, weave)

import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Quantities.Math (gradient, surface, uNormalVect)
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.PhysicalProperties (density, mass, vol)

import Drasil.SWHS.Unitals
import Drasil.SWHS.Concepts
import Drasil.SWHS.TModel1
import Drasil.SWHS.TModel2
import Drasil.SWHS.TModel3
import Drasil.SWHS.DataDefs
import Drasil.SWHS.Modules
import Drasil.SWHS.Changes
import Drasil.SWHS.Reqs

import Drasil.OrganizationOfSRS
import qualified Drasil.SRS as SRS
import Drasil.ReferenceMaterial (intro)
import Drasil.DocumentLanguage
import Drasil.Template.MG
import Drasil.Template.DD
import Drasil.TraceabilityMandGs
import Drasil.Introduction
import Drasil.Requirements
import Drasil.GeneralSystDesc

acronyms :: [CI]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ode,
  phsChgMtrl,physSyst,requirement,rightSide,srs,progName,thModel]

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

--Will there be a table of contents?

authors :: Sentence
authors = manyNames [thulasi, brooks, spencerSmith]

swhs_si :: SystemInformation
swhs_si = SI swhs_pcm srs [thulasi, brooks, spencerSmith] 
  this_si swhsSymbols (swhsSymbols) acronyms 
  --Note: The second swhsSymbols here is 
    -- Redundant b/c the unitals are not really concepts (yet). There
    -- Will still likely be a better way to do this.
  --FIXME: Should be all Named, not just acronyms at the end.
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro 
  [ TUnits, tsymb'' tsymb_intro (TermExcept [uNormalVect]), TAandA ]
  ) : map Verbatim [s2, s3, s4, s5, s6, s7]

tsymb_intro :: [TSIntro]
tsymb_intro = [TSPurpose,SymbConvention
  [Lit (nw CT.heat_trans), Doc' (nw progName)], SymbOrder]

swhs_srs' :: Document
swhs_srs' = mkDoc mkSRS swhs_si

-- It is sometimes hard to remember to add new sections both here and above.

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

swhs_mg :: Document
swhs_mg = mgDoc swhsFull authors mgBod


-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

s2 :: Section
s2 = introductionF progName (s2_intro, s2_kSent) s2_1_par1 s2_2_tuple s2_3_tuple True s2_4_tuple
-- Previous: s2 = introF s2_intro s2_kSent [s2_1, s2_2, s2_3, s2_4]

s2_intro :: Sentence
s2_intro = S "Due to increasing cost, diminishing" +:+
  S "availability, and negative environmental impact of" +:+
  S "fossil fuels, there is a higher demand for renewable" +:+
  phrase energy +:+ S "sources and" +:+
  phrase energy +:+. S "storage technology" +:+ (swhs_pcm ^. defn)
  +:+ sParen (short phsChgMtrl) +:+ S "use a renewable" +:+
  phrase energy +:+. S "source and provide a novel way of" +:+
  S "storing" +:+. phrase energy +:+
  at_start swhs_pcm +:+ S "improve over the traditional" +:+
  phrase progName :+: S "s because of their smaller size. The" +:+
  S "smaller size is possible because of the ability of" +:+ 
  short phsChgMtrl +:+ S "to store" +:+ phrase CT.thermal_energy +:+
  S "as" +:+ phrase latent_heat `sC`
  S "which allows higher" +:+ phrase CT.thermal_energy +:+
  S "storage capacity per" +:+ phrase unit_ +:+. S "weight"

s2_kSent :: Sentence
s2_kSent = EmptyS +:+. (phrase swhs_pcm) +:+ S "The developed" +:+
  (phrase program) +:+ S "will be referred to as" +:+
  (titleize progName) +:+ sParen (short progName)

-- In Concepts.hs "swhs_pcm" gives "s for program name, and there is a 
-- similar paragraph in each of the other eolar water heating systems incorporating
-- PCM" which is not capitlaized whereas the stable version is

-- NamedChunks... Sometimes capitalized, sometimes not, sometimes plural, 
-- sometimes not, sometimes need to be used in different tenses. How to 
-- accomodate all this?

-- The second paragraph is general exceptxamples. It can probably be 
-- abstracted out.

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

s2_1_par1 :: Sentence
s2_1_par1 = S "The main" +:+ phrase purpose +:+ S "of this" +:+
  phrase document +:+ S "is to describe the modelling of" +:+.
  phrase swhs_pcm +:+ S "The" +:+ plural goalStmt +:+
  S "and" +:+ plural thModel +:+ S "used in the" +:+ short progName +:+
  S "code are provided, with an emphasis on explicitly identifying" +:+ 
  plural assumption +:+ S "and unambiguous" +:+. plural definition +:+
  S "This" +:+ phrase document +:+ S "is intended to be used as a" +:+
  phrase reference +:+ S "to provide ad hoc access to all" +:+
  phrase information +:+ S "necessary to understand and verify the" +:+.
  phrase model +:+ S "The" +:+ short srs +:+ 
  S "is abstract because the contents say what" +:+
  phrase problem +:+. S "is being solved, but do not say how to solve it"
 

-- Besides program name, these two paragraphs are general, mostly repeated 
-- between examples, and can be abstracted out.

--How to italicize words in sentence?
--How to cite?

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

s2_2_tuple :: (Sentence, Sentence)
s2_2_tuple = (s2_2_contents, s2_2_end)

s2_2_contents :: Sentence
s2_2_contents = phrase CT.thermal_analysis +:+ S "of a single" +:+
  phrase tank_pcm --FIXME: Caps issue
  
s2_2_end :: Sentence
s2_2_end = S "predict the" +:+
  phrase temp +:+ S "and" +:+ phrase CT.thermal_energy +:+
  S "histories for the" +:+ phrase water +:+ S "and the" +:+.
  short phsChgMtrl +:+ S "This entire" +:+ phrase document +:+
  S "is written assuming that the substances inside the" +:+
  phrase sWHT +:+ S "are" +:+ phrase water +:+ S "and" +:+.
  short phsChgMtrl

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

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

s2_3_tuple :: (Sentence, Sentence, Sentence)
s2_3_tuple = (s2_3_knowlegde, s2_3_understanding, EmptyS)

s2_3_knowlegde :: Sentence
s2_3_knowlegde = phrase CT.heat +:+ S "transfer" +:+. phrase theory +:+
  S "A third or fourth year Mechanical Engineering course on this topic" +:+
  S "is recommended"

s2_3_understanding :: Sentence
s2_3_understanding = S "differential" +:+ plural equation `sC` S "as typically" +:+
  S "covered in first and second year Calculus courses"

{-s2_3 = SRS.charOfIR [s2_3_contents] []

s2_3_contents = Paragraph (S "Reviewers of this" +:+ phrase documentation +:+
  S "should have a strong knowledge in" +:+ (plural $ CT.heat) +:+
  S "transfer" +:+. phrase theory +:+ S "A third or fourth year" +:+
  S "Mechanical Engineering course on this topic is recommended. The" +:+
  plural reviewer +:+ S "should also have an understanding of differential" +:+
  (plural $ equation) `sC` S "as typically covered in" +:+
  S "first and second year Calculus courses. The" +:+ plural user +:+
  S "of" +:+ short progName +:+ S "can have a lower level of expertise," +:+
  S "as explained in" +:+. (makeRef s3))-}
-- should reference User characteristics


------------------------------------
-- 2.4 : Organization of Document --
------------------------------------

s2_4_tuple :: (Sentence, CI, Section, Sentence)
s2_4_tuple = (s2_4_intro, inModel, (SRS.inModel SRS.missingP []), s2_4_trail)

s2_4_intro :: Sentence
s2_4_intro = S "The" +:+ phrase organization +:+ S "of this" +:+
  phrase document +:+ S "follows the template for an" +:+ short srs +:+
  S "for" +:+ phrase sciCompS +:+ S "proposed by [citation] and" +:+.
  sSqBr (S "citation")

s2_4_trail :: Sentence
s2_4_trail = S "The" +:+ plural inModel +:+ sParen (
  makeRef (SRS.inModel SRS.missingP [])) +:+. 
  S "to be solved are referred to as IM1 to IM4" +:+ S "The" +:+
  plural inModel +:+ S "provide the" +:+ phrase ode +:+
  sParen (short ode :+: S "s") +:+ S "and algebraic" +:+
  plural equation +:+ S "that" +:+
  phrase model +:+ S "the" +:+. phrase swhs_pcm +:+
  short progName +:+ S "solves these" +:+ short ode :+: S "s"
-- This part is close to the function but not exactly,
-- so keeping it here for reference

  {-S "The presentation follows the standard" +:+
  S "pattern for presenting" +:+ plural goalStmt `sC`
  plural thModel `sC`
  (plural dataDefn) `sC` S "and" +:+. 
  (plural assumption) +:+
  S "For readers that would like a more bottom" +:+ 
  S "up approach, they can start reading the" +:+ 
  (plural inModel) +:+ S "in" +:+ 
  makeRef s4_2_5 +:+ S "and trace back to find any" +:+
  S "additional" +:+ phrase information +:+ S "they require."-}

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

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

s3 :: Section
s3 = genSysF [s3_1] s3_2_contents [] []

-- Completely general paragraph, same between examples. Easily abstracted out.

--------------------------
-- 3.1 : System Context --
--------------------------

s3_1 :: Section
s3_1 = SRS.sysCont [s3_1_contents, sys_context_fig, s3_1_2_intro,
  s3_1_2_bullets] []

s3_1_contents ::Contents
s3_1_contents = Paragraph $ makeRef sys_context_fig +:+ S "shows the" +:+.
  phrase sysCont +:+ S "A circle represents an external entity outside the" +:+
  phrase software `sC` S "the" +:+ phrase user +:+ S "in this case. A" +:+
  S "rectangle represents the" +:+ phrase softwareSys +:+
  S "itself" +:+. (sParen $ short progName) +:+ S "Arrows are used to show the" +:+
  plural datum +:+ S "flow between the" +:+ phrase system +:+
  S "and its" +:+. phrase environment

sys_context_fig :: Contents
sys_context_fig = Figure (makeRef sys_context_fig :+: S ":" +:+
  titleize sysCont) "SystemContextFigure.png"

s3_1_2_intro :: Contents
s3_1_2_intro = Paragraph $ short progName +:+. S "is mostly self-contained" +:+
  S "The only external interaction is through the" +:+ phrase user +:+
  S "interface. The responsibilities of the" +:+ phrase user +:+
  S "and the" +:+ phrase system +: S "are as follows"

s3_1_2_bullets :: Contents
s3_1_2_bullets = Enumeration (Bullet $
  [Nested (titleize user +: S "Responsibilities")
  (Bullet $ map (\c -> Flat c)
  [S "Provide the" +:+ phrase input_ +:+ plural datum +:+ S "to the" +:+
  phrase system `sC` S "ensuring no errors in the" +:+ plural datum +:+
  S "entry",
  S "Take care that consistent" +:+ plural unit_ +:+
  S "are used for" +:+ phrase input_ +:+ plural variable
  ]),
  Nested (short progName +: S "Responsibilities")
  (Bullet $ map (\c -> Flat c)
  [S "Detect" +:+ plural datum +:+ S "type mismatch, such as a string of" +:+
  S "characters instead of a floating point number",
  S "Determine if the" +:+ plural input_ +:+ S "satisfy the required" +:+
  phrase physical +:+ S "and" +:+ phrase software +:+ plural constraint,
  S "Calculate the required" +:+ plural output_
  ])])

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

---s3_2 = SRS.userChar [s3_2_contents] []

s3_2_contents :: Contents
s3_2_contents = Paragraph $ S "The end" +:+ phrase user +:+ S "of" +:+
  short progName +:+ S "should have an understanding of undergraduate" +:+
  S "Level 1 Calculus and" +:+. (titleize physics)

-- Some of these course names are repeated between examples, could potentially 
-- be abstracted out.

------------------------------
-- 3.3 : System Constraints --
------------------------------

--s3_3 = systCon Nothing []

-- This is the same for all of our examples... but there could potentially be 
-- system constraints in other projects so it can't be abstracted out as is...

---------------------------------------------
-- Section 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

s4 :: Section
s4 = specSysDesF s4_intro_end [s4_1, s4_2]
 
-- using plural solutionCharSpec is a hack in order to pluralize the middle word,
-- based on compoundNPNC''' in NamedIdea.hs
s4_intro_end :: Sentence
s4_intro_end = plural thModel `sC` plural genDefn `sC` plural dataDefn `sC`
  S "and finally the" +:+ plural inModel +:+ sParen (short ode :+:
  S "s") +:+ S "that" +:+ phrase model +:+ S "the" +:+ phrase swhs_pcm

-- Completely general except for solar water heating tank (object of analysis) 
-- and similar between all examples; can be abstracted out.
 
-- The swhs_pcm reference at the end would be better if singular, but concept
-- is plural.

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1 :: Section
s4_1 = SRS.probDesc [s4_1_intro]
  [s4_1_1, s4_1_2, s4_1_3]

s4_1_intro :: Contents
s4_1_intro = Paragraph $ short progName +:+ S "is a computer" +:+
  phrase program +:+ S "developed to investigate the effect of" +:+
  S "employing" +:+ short phsChgMtrl +:+ S "within a" +:+. phrase sWHT

--  section is very different between all examples

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1 :: Section
s4_1_1 = termDefnF Nothing [s4_1_1_bullets]

-- Above paragraph is repeated in all examples, can be abstracted out. (Note: 
-- GlassBR has an additional sentence with a reference at the end.)

s4_1_1_bullets :: Contents
s4_1_1_bullets = Enumeration (Bullet $ map s411_bullet_map_f
  [CT.ht_flux, phase_change_material, CT.heat_cap_spec, 
  CT.thermal_conduction, transient])

s411_bullet_map_f :: Concept c => c -> ItemType
s411_bullet_map_f c = Flat $ at_start c :+: S ":" +:+. (c ^. defn)
  
-- Structure of this list is same in all examples, probably can be automated.

-- Included heat flux and specific heat in NamedChunks even though they are 
-- already in SWHSUnits

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

s4_1_2 :: Section
s4_1_2 = physSystDesc (short progName) (fig_tank) [s4_1_2_list, fig_tank]

-- Above paragraph is general except for progName and figure. However, not 
-- every example has a physical system. Also, the SSP example is different, so
-- this paragraph can not be abstracted out as is.

s4_1_2_list :: Contents
s4_1_2_list = enumSimple 1 (short physSyst) s4_1_2_physSystList

s4_1_2_physSystList :: [Sentence]
physSyst1, physSyst2, physSyst3 :: Sentence
s4_1_2_physSystList = [physSyst1, physSyst2, physSyst3]

physSyst1 = at_start tank +:+ S "containing" +:+. phrase water
--
physSyst2 = at_start coil +:+ S "at bottom of" +:+.
  phrase tank +:+ sParen (P (ht_flux_C ^. symbol) +:+
  S "represents the" +:+. phrase ht_flux_C)
--
physSyst3 = short phsChgMtrl +:+ S "suspended in" +:+.
  phrase tank +:+ sParen  (P (ht_flux_P ^. symbol) +:+
  S "represents the" +:+. phrase ht_flux_P)

-- Structure of list would be same between examples but content is completely 
-- different
-- FIXME: Figures have different IDs than stable structure
fig_tank :: Contents
fig_tank = Figure (at_start sWHT `sC` S "with" +:+
  phrase ht_flux_C +:+ S "of" +:+ P (ht_flux_C ^. symbol) +:+
  S "and" +:+ (phrase $ ht_flux_P) +:+ S "of" +:+
  P (ht_flux_P ^. symbol)) "Tank.png"

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

s4_1_3 :: Section
s4_1_3 = SRS.goalStmt [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro :: Contents
s4_1_3_intro = Paragraph $ S "Given the" +:+ phrase temp_C `sC`
  S "initial" +:+ plural condition +:+ S "for the" +:+
  phrase temp_W +:+ S "and the" +:+ phrase temp_PCM `sC`
  S "and material" +:+ plural property `sC` S "the" +:+ plural goalStmt +:
  S "are"

-- 2 examples include this paragraph, 2 don't. The "givens" would need to be 
-- abstracted out if this paragraph were to be abstracted out.

s4_1_3_list :: Contents
s4_1_3_list = enumSimple 1 (short goalStmt) $ map (goalState) [temp_W, temp_PCM, w_E, pcm_E]

goalState :: NamedIdea b => b -> Sentence
goalState b = S "Predict the" +:+ phrase b +:+ S "over" +:+. phrase time

--  ((short goalStmt) :+: S "1", Flat (S "Predict the" +:+
--  (phrase $ temp_W) +:+ S "over" +:+. (phrase $ time))),
--
--  ((short goalStmt) :+: S "2", Flat (S "Predict the" +:+
--  (phrase $ temp_PCM) +:+ S "over" +:+. (phrase $ time))),
--
--  ((short goalStmt) :+: S "3", Flat (S "Predict the" +:+
--  (phrase $ w_E) +:+ S "over" +:+. (phrase $ time))),
--
--  ((short goalStmt) :+: S "4", Flat (S "Predict the" +:+
--  (phrase $ pcm_E) +:+ S "over" +:+. (phrase $ time)))

-- List structure is repeated between examples. (For all of these lists I am 
-- imagining the potential for something like what was done with the lists in 
-- MG, where you define goals, assumptions, physical system components, etc. in
-- separate files, import them and pass them as arguments to some "makeSRS" 
-- function and the rest is automated.)

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

s4_2 :: Section
s4_2 = solChSpecF progName (s4_1, s6) True s4_2_4_intro_end 
  ((makeRef s4_2_6_table1 +:+ S "and" +:+ makeRef s4_2_6_table2 +:+
  S "show"), mid, True, end)
  ([s4_2_1_list], s4_2_2_T1 ++ s4_2_2_T2 ++ s4_2_2_T3, s4_2_3_deriv,
    s4_2_4_DD1 ++ s4_2_4_DD2 ++ s4_2_4_DD3, (s4_2_5_subpar ++ s4_2_5_deriv1 ++ s4_2_5_deriv2),
    [s4_2_6_table1, s4_2_6_table2]) [s4_2_7]

  where mid = S "The" +:+ phrase column +:+ S "for" +:+ phrase software +:+ 
              plural constraint +:+ S "restricts the range of" +:+ plural input_ +:+ 
              S "to reasonable" +:+. plural value
        end = sParen (S "The" +:+ plural table_ +:+ S "are left out" +:+.
              S "because features they should use are not yet implemented in Drasil")

-- General besides progName, repeated in only one other example but it could be
-- used for all of them. So it can be abstracted out.

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

-- General paragraph, repeated in every example. Can be abstracted out.

s4_2_1_list :: Contents
s4_2_1_list = enumSimple 1 (short assumption) s4_2_1_assump_list

s4_2_1_assump_list :: [Sentence]

assump1, assump2, assump3, assump4, assump5, assump6,
  assump7, assump8, assump9, assump10, assump11, assump12, assump13, assump14,
  assump15, assump16, assump17, assump18, assump19 :: Sentence

s4_2_1_assump_list = [assump1, assump2, assump3, assump4, assump5, assump6,
  assump7, assump8, assump9, assump10, assump11, assump12, assump13, assump14,
  assump15, assump16, assump17, assump18, assump19]

assump1 = S "The only form of" +:+ phrase energy +:+ S "that is" +:+
  S "relevant for this" +:+ phrase problem +:+ S "is" +:+.
  phrase CT.thermal_energy +:+ S "All other forms of" +:+
  phrase energy `sC` S "such as" +:+
  phrase mech_energy `sC` S "are assumed to be negligible" +:+. sSqBr
  (makeRef (swhsSymbMapT t1ConsThermE))
--
assump2 = S "All" +:+
  phrase CT.heat_trans +:+ S "coefficients are constant over" +:+
  phrase time +:+. sSqBr (S "GD1")
--
assump3 = S "The" +:+ 
  phrase water +:+ S "in the" +:+ phrase tank +:+
  S "is fully mixed, so the" +:+ phrase temp_W +:+
  S "is the same throughout the entire" +:+ phrase tank +:+. sSqBr
  (S "GD2" `sC` (makeRef (swhsSymbMapD dd2HtFluxP)))
--
assump4 = S "The" +:+ phrase temp_PCM +:+ S "is the same throughout the" +:+
  phrase pcm_vol +:+. sSqBr (S "GD2" `sC`
  (makeRef (swhsSymbMapD dd2HtFluxP)) `sC` S "LC1")
--
assump5 = S "The" +:+ 
  phrase w_density +:+ S "and" +:+
  phrase pcm_density +:+ S "have no spatial variation; that" +:+
  S "is, they are each constant over their entire" +:+
  phrase vol +:+. sSqBr (S "GD2")
--
assump6 = S "The" +:+ phrase htCap_W `sC` (phrase $ htCap_S_P) `sC` S "and" +:+ 
  phrase htCap_L_P +:+ S "have no spatial variation; that" +:+
  S "is, they are each constant over their entire" +:+
  phrase vol +:+. sSqBr (S "GD2")
--
assump7 = (CT.law_conv_cooling ^. defn) +:+
  S "applies between the" +:+ phrase  coil +:+ S "and the" +:+
  phrase water +:+. sSqBr (makeRef (swhsSymbMapD dd1HtFluxC))
--
assump8 = S "The" +:+ phrase temp_C +:+ S "is constant over" +:+ phrase time +:+.
  sSqBr (makeRef (swhsSymbMapD dd1HtFluxC) `sC` S "LC2")
--
assump9 = S "The" +:+ phrase temp_C +:+ S "does not vary along its length" +:+. sSqBr
  (makeRef (swhsSymbMapD dd1HtFluxC) `sC` S "LC3")
--
assump10 = (CT.law_conv_cooling ^. 
  defn) +:+ S "applies between the" +:+
  phrase water +:+ S "and the" +:+ short phsChgMtrl +:+.
  sSqBr (makeRef (swhsSymbMapD dd2HtFluxP))
--
assump11 = S "The" +:+ phrase model +:+
  S "only accounts for" +:+ (charging ^. defn) `sC`
  S "not" +:+. phrase discharging +:+
  S "The" +:+ phrase temp_W +:+ S "and" +:+ 
  phrase temp_PCM +:+ S "can only increase, or remain" +:+
  S "constant; they do not decrease. This implies that the" +:+
  phrase temp_init +:+ S "(A12) is less than (or equal)" +:+
  S "to the" +:+ phrase temp_C +:+. sSqBr (S "IM1, LC4")
--
assump12 = S "The" +:+
  phrase temp_init +:+ S "of the" +:+
  phrase water +:+ S "and the" +:+ short phsChgMtrl +:+
  S "is the same" +:+. sSqBr (S "IM1, IM2, LC5")
--
assump13 = S "The" +:+ phrase simulation +:+ S "will start with the" +:+
  short phsChgMtrl +:+ S "in a" +:+ (solid ^. defn) +:+. sSqBr (S "IM2, IM4")
--
assump14 = S "The operating" +:+
  phrase temp +:+ S "range of the" +:+ phrase system +:+
  S "is such that the" +:+ phrase water +:+
  S "is always in" +:+. (liquid ^. defn) +:+ S "That is," +:+
  S "the" +:+ phrase temp +:+ S "will not drop below the" +:+
  phrase melt_pt +:+ S "of" +:+
  phrase water `sC` S "or rise above its" +:+
  phrase boil_pt +:+. sSqBr (S "IM1, IM3")
--
assump15 = S "The" +:+
  phrase tank +:+ S "is" +:+ phrase perfect_insul +:+
  S "so that there is no" +:+ phrase CT.heat +:+
  S "loss from the" +:+ phrase tank +:+. sSqBr (S "IM1, LC6")
--
assump16 = S "No internal" +:+
  phrase CT.heat +:+ S "is generated by either the" +:+
  phrase water +:+ S "or the" +:+ short phsChgMtrl `semiCol`
  S "therefore, the" +:+ phrase vol_ht_gen +:+.
  S "is zero [IM1, IM2]"
--
assump17 = S "The" +:+ phrase vol +:+ phrase change +:+ S "of the" +:+
  short phsChgMtrl +:+ S "due to" +:+ phrase CT.melting +:+.
  S "is negligible [IM2]"
--
assump18 = S "The" +:+ 
  short phsChgMtrl +:+ S "is either in a" +:+
  (liquid ^. defn) +:+ S "or a" +:+ (solid ^. defn) +:+
  S "but not a" +:+ (gaseous ^. defn) +:+. sSqBr (S "IM2, IM4")
--
assump19 = S "The pressure in" +:+ S "the" +:+
  phrase tank +:+ S "is atmospheric, so the" +:+
  phrase melt_pt +:+ S "and" +:+ phrase boil_pt +:+
  S "are 0" :+: Sy (unit_symb temp) +:+ S "and 100" :+:
  Sy (unit_symb temp) `sC` S "respectively [IM1, IM3]."

-- Again, list structure is same between all examples.

-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

-- General paragraph, repeated in one other example but could be included in 
-- all. Can be abstracted out.
 
-- s4_2_3_GDs :: [LayoutObj]
-- s4_2_3_GDs = map Definition (map General [gd1NewtonCooling])

--General definitions not yet implemented

s4_2_3_deriv :: [Contents]
s4_2_3_deriv = [Paragraph (S "Detailed derivation of simplified"
  +:+ phrase rOfChng +:+ S "of" +: phrase temp),
  Paragraph (S "Integrating" +:+ makeRef (swhsSymbMapT t1ConsThermE) +:+ 
  S "over a" +:+ phrase vol +:+ sParen (
  P (vol ^. symbol)) `sC` S "we have:"),
  EqnBlock 
  ((Neg (UnaryOp (Integral (Just (Low (C vol)), Nothing)
  ((C gradient) :. (C thFluxVect)) vol))) + 
  UnaryOp (Integral (Just (Low (C vol)), Nothing) 
  (C vol_ht_gen) vol) := 
  UnaryOp (Integral (Just (Low (C vol)), Nothing) ((C density) 
  * (C heat_cap_spec) * Deriv Part (C temp) (C time)) vol)),
  Paragraph (S "Applying" +:+ titleize gauss_div +:+ S "to" +:+
  S "the first term over the" +:+ phrase surface +:+
  P (surface ^. symbol) +:+ S "of the" +:+ 
  phrase vol `sC` S "with" +:+ P (thFluxVect ^. symbol) +:+ S "as the" +:+
  phrase thFluxVect +:+ S "for the" +:+ phrase surface +:+ S "and" +:+
  P (uNormalVect ^. symbol) +:+ S "as a" +:+ phrase unit_ +:+
  S "outward" +:+ phrase uNormalVect +:+ S "for a" +:
  phrase surface),
  EqnBlock 
  ((Neg (UnaryOp (Integral (Just (Low (C surface)), Nothing) 
  ((C thFluxVect) :. (C uNormalVect)) surface))) + 
  (UnaryOp (Integral (Just 
  (Low (C vol)), Nothing) (C vol_ht_gen) vol)) := 
  UnaryOp (Integral (Just (Low (C vol)), Nothing) 
  ((C density) * (C heat_cap_spec) * Deriv Part (C temp) (C time)) vol)),
  Paragraph (S "We consider an arbitrary" +:+. phrase vol +:+ S "The" +:+
  phrase vol_ht_gen :+: S "is" +: S "assumed constant. Then (1) can be written as"),
  EqnBlock 
  ((C ht_flux_in) * (C in_SA) - (C ht_flux_out) * 
  (C out_SA) + (C vol_ht_gen) * (C vol) := UnaryOp (Integral 
  (Just (Low (C vol)), Nothing) ((C density) * (C heat_cap_spec) *
  Deriv Part (C temp) (C time)) vol)),
  Paragraph (S "Where" +:+ P (ht_flux_in ^. symbol) `sC`
  P (ht_flux_out ^. symbol) `sC` P (in_SA ^. symbol) `sC`
  S "and" +:+ P (out_SA ^. symbol) +:+ S "are explained in" +:+
  S "GD2. Assuming" +:+ P (density ^. symbol) `sC`
  P (heat_cap_spec ^. symbol) +:+ S "and" +:+ P (temp ^. symbol) +:+
  S "are constant over the" +:+ phrase vol `sC`
  S "which is true in our case by" +:+ titleize' assumption +:
  S "(A3), (A4), (A5), and (A6), we have"),
  EqnBlock 
  ((C density) * (C heat_cap_spec) * (C vol) * Deriv Total (C temp) 
  (C time) := (C ht_flux_in) * (C in_SA) - (C ht_flux_out) * 
  (C out_SA) + (C vol_ht_gen) * (C vol)),
  Paragraph (S "Using the fact that" +:+ P (density ^. symbol) :+:
  S "=" :+: P (mass ^. symbol) :+: S "/" :+: 
  P (vol ^. symbol) `sC` S "(2) can be written as:"),
  EqnBlock 
  ((C mass) * (C heat_cap_spec) * Deriv Total (C temp) (C time) :=
  (C ht_flux_in) * (C in_SA) - (C ht_flux_out) * (C out_SA) + 
  (C vol_ht_gen) * (C vol))]

-- Created a unitalChunk for "S"... should I add it to table of symbols?
-- Add references to above when available (assumptions, GDs)
-- Replace relevant Derivs with the regular derivative when it is available

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------


s4_2_4_intro_end :: Sentence
s4_2_4_intro_end = S "The dimension of each" +:+ phrase quantity +:+.
  S "is also given"

-- General paragraph, repeated in most examples but would work for all. Can be 
-- absracted out.

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5_subpar :: [Contents]
s4_2_5_subpar = [Paragraph (S "The goals GS1 to GS4 are solved by IM1 to IM4." +:+
  S "The" +:+ plural solution +:+ S "for IM1 and IM2 are coupled since" +:+
  S "the" +:+ phrase solution +:+ S "for" +:+ P (temp_W ^. symbol) +:+
  S "and" +:+ P (temp_PCM ^. symbol) +:+ S "depend on one another. IM3" +:+
  S "can be solved once IM1 has been solved. The" +:+ phrase solution +:+
  S "of IM2 and IM4 are also coupled, since the" +:+ 
  phrase temp_PCM +:+ S "and" +:+ phrase pcm_E +:+
  S "depend on the" +:+. phrase CT.phase_change +:+
  sParen (at_start' inModel +:+ S "are left out because" +:+.
  S "they are not currently implemented in Drasil"))]

{-s4_2_5 = SRS.inModel ((s4_2_5_intro) ++ (s4_2_5_deriv1) ++
  (s4_2_5_deriv2)) []

s4_2_5_intro = [Paragraph (S "This" +:+ phrase section_ +:+ S "transforms" +:+
  S "the" +:+ phrase problem +:+ S "defined in" +:+ (makeRef s4_1) +:+
  S "into one which is expressed in mathematical terms. It uses concrete" +:+
  plural symbol_ +:+ S "defined in" +:+ (makeRef s4_2_4) +:+
  S "to replace the abstract" +:+ plural symbol_ +:+ S "in the" +:+
  plural model +:+ S "identified in" +:+ (makeRef s4_2_2) +:+ S "and" +:+.
  (makeRef s4_2_3)),-}

-- The first paragraph is completely general and repeated in other examples. 
-- The second paragraph is very specific, and the other examples don't even 
-- include a paragraph analogous to this one.

-- Instance Models aren't implemented yet

s4_2_5_deriv1 :: [Contents]
s4_2_5_deriv1 = s4_2_5_d1startPara ++ (weave [s4_2_5_d1sent_list, s4_2_5_d1eqn_list])

s4_2_5_d1startPara, s4_2_5_d2startPara, s4_2_5_d2endPara, s4_2_5_d1eqn_list, 
  s4_2_5_d1sent_list, s4_2_5_d2eqn_list, s4_2_5_d2sent_list :: [Contents]

s4_2_5_d1startPara = [Paragraph (S "Derivation of the" +:+
  phrase energy +:+ S "balance on" +: phrase water)]

s4_2_5_d1eqn_list = map (EqnBlock) [s4_2_5_d_eqn1, s4_2_5_d_eqn2, s4_2_5_d_eqn3, 
  s4_2_5_d_eqn4, s4_2_5_d_eqn5, s4_2_5_d_eqn6, s4_2_5_d_eqn7]

s4_2_5_d1sent_list = map (Paragraph . (foldle (+:+) (+:) EmptyS)) [
  [S "To find the", phrase rOfChng, S "of", (P $ temp_W ^. symbol) `sC` 
  S "we look at the", phrase energy, S "balance on" +:+. 
  phrase water, S "The", phrase vol, 
  S "being considered is the", phrase w_vol, EmptyS, 
  (P $ w_vol ^. symbol) `sC` S "which has", phrase w_mass, EmptyS, -- why is EmptyS here?
  (P $ w_mass ^. symbol), S "and" +:+. (phrase htCap_W `sC` 
  (P $ htCap_W ^. symbol)), (P $ ht_flux_C ^. symbol), S "represents the", 
  phrase ht_flux_C, S "and", (P $ ht_flux_P ^. symbol), S "represents",
  S "the", phrase ht_flux_P `sC` S "over", phrase coil_SA,
  S "and", phrase pcm_SA, S "of", (P $ coil_SA ^. symbol), S "and", 
  (P $ pcm_SA ^. symbol) `sC` S "respectively. No", 
  phrase CT.heat_trans, S "occurs to the outside of the",
  phrase tank `sC` S "since it",
  S "has been assumed to be", phrase perfect_insul +:+. 
  sParen (S "A15"), S "Assuming no", phrase vol_ht_gen +:+.
  ((sParen $ S  "A16") `sC` ((P $ vol_ht_gen ^. symbol) :+: S "=0")),
  S "Therefore, the", phrase equation, S "for GD2 can be",
  S "written as"],
  [S "Using", makeRef (swhsSymbMapD dd1HtFluxC), S "and",
  makeRef (swhsSymbMapD dd2HtFluxP), S "for", 
  (P $ ht_flux_C ^. symbol), S "and", (P $ ht_flux_P ^. symbol), 
  S "respectively,", S "this can be written as"],
  [S "Dividing (3) by", P (w_mass ^. symbol) :+:
  P (htCap_W ^. symbol) `sC` S "we obtain"],
  [S "Factoring the negative sign out of the second", S "term of the", 
  short rightSide, S "of", titleize equation, 
  S "(4) and multiplying it by", 
  (P $ coil_HTC ^. symbol) :+: (P $ coil_SA ^. symbol) :+: S "/" :+: 
  (P $ coil_HTC ^. symbol) :+: (P $ coil_SA ^. symbol), S "yields"],
  [S "Which simplifies to"],
  [S "Setting", P (tau_W ^. symbol) :+: S "=" :+:
  (P $ w_mass ^. symbol) :+: (P $ htCap_W ^. symbol) :+: S "/" :+:
  (P $ coil_HTC ^. symbol) :+: (P $ coil_SA ^. symbol), 
  S "and", (P $ eta ^. symbol) :+: S "=" :+: (P $ pcm_HTC ^. symbol) :+: 
  (P $ pcm_SA ^. symbol) :+: S "/" :+: (P $ coil_HTC ^. symbol) :+: 
  (P $ coil_SA ^. symbol) `sC` (titleize $ equation), S "(5) can",
  S "be written as"],
  [S "Finally, factoring out 1/" :+: (P $ tau_W ^. 
  symbol) `sC` S "we are left with the governing",
  (short ode), S "for IM1"]]


s4_2_5_d_eqn1, s4_2_5_d_eqn2, s4_2_5_d_eqn3, s4_2_5_d_eqn4, s4_2_5_d_eqn5,
  s4_2_5_d_eqn6, s4_2_5_d_eqn7 :: Expr

s4_2_5_d_eqn1 = ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) (C time) := 
  (C ht_flux_C) * (C coil_SA) - (C ht_flux_P) * (C pcm_SA)) 

s4_2_5_d_eqn2 = ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) (C time) := 
  (C coil_HTC) * (C coil_SA) * ((C temp_C) - (C temp_W)) -
  (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d_eqn3 = (Deriv Total (C temp_W) (C time) := ((C coil_HTC) * 
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - 
  (C temp_W)) - ((C pcm_mass) * (C pcm_SA)) / ((C w_mass) *
  (C htCap_W)) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d_eqn4 = (Deriv Total (C temp_W) (C time) := ((C coil_HTC) * 
   (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - 
   (C temp_W)) + ((C coil_HTC) * (C coil_SA)) / ((C coil_HTC) * 
   (C coil_SA)) * ((C pcm_HTC) * (C pcm_SA)) / ((C w_mass) * 
   (C htCap_W)) * ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn5 = (Deriv Total (C temp_W) (C time) := ((C coil_HTC) * 
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) -
  (C temp_W)) + ((C pcm_HTC) * (C pcm_SA)) / ((C coil_HTC) *
  (C coil_SA)) * ((C coil_HTC) * (C coil_SA)) / ((C w_mass) * 
  (C htCap_W)) * ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn6 = (Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) *
  ((C temp_C) - (C temp_W)) + ((C eta) / (C tau_W)) *
  ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn7 = (Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) *
   (((C temp_C) - (C temp_W)) + (C eta) * ((C temp_PCM) - 
   (C temp_W))))

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Fractions in paragraph?

s4_2_5_deriv2 :: [Contents]
s4_2_5_deriv2 = s4_2_5_d2startPara ++ (weave [s4_2_5_d2eqn_list, s4_2_5_d2sent_list]) ++
  s4_2_5_d2endPara

s4_2_5_d2sent_list = map (Paragraph . (foldle (+:+) (+:) EmptyS)) [[S "Using", 
  (makeRef (swhsSymbMapD dd2HtFluxP)), S "for",
  (P $ ht_flux_P ^. symbol) `sC` S "this", (phrase $ equation),
   S "can be written as"],
  [S "Dividing by", (P $ pcm_mass ^. symbol) :+: P (htCap_S_P ^. symbol), 
  S "we obtain"],
  [S "Setting", P (tau_S_P ^. symbol) :+: S "=" :+:
  P (pcm_mass ^. symbol) :+: P (htCap_S_P ^. symbol) :+: S "/" :+:
  P (pcm_HTC ^. symbol) :+: P (pcm_SA ^. symbol) `sC`
  S "this can be written as"]]
 

s4_2_5_d2eqn_list = map (EqnBlock) [s4_2_5_d2eqn1, s4_2_5_d2eqn2, s4_2_5_d2eqn3, 
  s4_2_5_d2eqn4]

s4_2_5_d2eqn1, s4_2_5_d2eqn2, s4_2_5_d2eqn3, s4_2_5_d2eqn4 :: Expr

s4_2_5_d2eqn1 = ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM) 
   (C time) := (C ht_flux_P) * (C pcm_SA))

s4_2_5_d2eqn2 = ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM) 
  (C time) := (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d2eqn3 = (Deriv Total (C temp_PCM) (C time) := ((C pcm_HTC) * 
   (C pcm_SA)) / ((C pcm_mass) * (C htCap_S_P)) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d2eqn4 = (Deriv Total (C temp_PCM) (C time) := (1 / (C tau_S_P)) *
  ((C temp_W) - (C temp_PCM)))

s4_2_5_d2startPara = map Paragraph [(S "Detailed derivation of the" +:+
  (phrase $ energy) +:+ S "balance on the" +:+ (short phsChgMtrl) +:+
  S "during" +:+ (phrase $ sens_heat) :+: S "ing phase:"),
  (S "To find the" +:+ (phrase $ rOfChng) +:+ S "of" +:+
  P (temp_PCM ^. symbol) `sC` S "we look at the" +:+
  (phrase $ energy) +:+ S "balance on the" +:+.
  (short phsChgMtrl) +:+ S "The" +:+ (phrase $ vol) +:+
  S "being considered is the" +:+ (phrase $ pcm_vol) :+: 
  S "," +:+. P (pcm_vol ^. symbol) +:+ S "The derivation" +:+
  S "that follows is initially for the" +:+
  (phrase $ solid) +:+ EmptyS +:+. (short phsChgMtrl) +:+
  S "The" +:+ (phrase $ pcm_mass) +:+ S "is" +:+ 
  P (pcm_mass ^. symbol) +:+ S "and the" +:+ (phrase $ htCap_S_P) +:+ S "is" +:+. P (htCap_S_P ^. symbol) +:+
  S "The" +:+ (phrase $ ht_flux_P) +:+ S "is" +:+ 
  P (ht_flux_P ^. symbol) +:+ S "over" +:+ (phrase $ pcm_SA) +:+.
  P (pcm_SA ^. symbol) +:+ S "There is no" +:+. 
  (phrase $ ht_flux_out) +:+ S "Assuming no" +:+
  (phrase $ vol_ht_gen) +:+ sParen (S "A16") `sC` P (vol_ht_gen ^. symbol) :+:
  S "=0, the" +:+ (phrase $ equation) +:
  S "for GD2 can be written as")]

s4_2_5_d2endPara = map Paragraph [((titleize $ equation) +:+ S "(6) applies for the" +:+ 
  (phrase $ solid) +:+ EmptyS +:+. (short phsChgMtrl) +:+
  S "In the case where all of the" +:+
  (short phsChgMtrl) +:+ S "is melted, the same" +:+
  S "derivation applies, except that" +:+ P (htCap_S_P ^. 
  symbol) +:+ S "is replaced by" +:+ P (htCap_L_P ^. symbol) `sC`
  S "and thus" +:+ P (tau_S_P ^. symbol) +:+ S "is" +:+ 
  S "replaced by" +:+. P (tau_L_P ^. symbol) +:+
  S "Although a small change in" +:+ (phrase $ surface) +:+
  S "area would be expected with" +:+ (phrase $ CT.melting) `sC`
  S "this is not included, since the" +:+
  (phrase $ vol) +:+ S "change of the" +:+ (short phsChgMtrl) 
  +:+ S "with" +:+ (phrase $ CT.melting) +:+.
  S "is assumed to be negligible (A17)"),
  (S "In the case where" +:+ P (temp_PCM ^. symbol) :+:
  S "=" :+: P (temp_melt_P ^. symbol) +:+ S "and not all of" +:+
  S "the" +:+ (short phsChgMtrl) +:+ S "is melted, the" +:+
  (phrase $ temp_PCM) +:+ S "does not change. Therefore, in" +:+
  S "this case d" :+: P (temp_PCM ^. symbol) :+: S "/d" :+:
  P (time ^. symbol) :+: S "=0."),
  (S "This derivation does not consider the" +:+ 
  (phrase $ CT.boiling) +:+ S "of the" +:+ 
  (short phsChgMtrl) `sC` S "as the" +:+ (short phsChgMtrl) 
  +:+ S "is assumed to either be in" +:+ S "a" +:+
  (solid ^. defn) +:+ S "or a" +:+ (liquid ^. defn) +:+.
  sParen (S "A18"))]

-- Add GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Derivative notation in paragraph?

----------------------------
-- 4.2.6 Data Constraints --
----------------------------

--[s4_2_6_table1, s4_2_6_table2]

-- I do not think Table 2 will end up being necessary for the Drasil version
---- The info from table 2 will likely end up in table 1.

-- FIXME: Temporary dummy tables
s4_2_6_table1 :: Contents
s4_2_6_table1 = Table [EmptyS,EmptyS] [[EmptyS,EmptyS],[EmptyS,EmptyS]]
  (S "Table 1")
  True

s4_2_6_table2 :: Contents
s4_2_6_table2 = Table [EmptyS,EmptyS] [[EmptyS,EmptyS],[EmptyS,EmptyS]]
  (S "Table 2")
  True


inputVar :: [UCWrapper]
inputVar = map ucw [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P] ++ [ucw htFusion] ++ map ucw [coil_SA,
  temp_C, w_density, htCap_W, coil_HTC, pcm_HTC, temp_init, time_final]
  
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


----------------------------------------------
-- 4.2.7 : Properties of A Correct Solution --
----------------------------------------------

s4_2_7 :: Section
s4_2_7 = SRS.propCorSol (s4_2_7_deriv) []

s4_2_7_deriv :: [Contents]
s4_2_7_deriv = [Paragraph (S "A" +:+ phrase corSol +:+ 
  S "must exhibit the" +:+. (phrase $ CT.law_cons_energy) +:+
  S "This means that the" +:+ (phrase $ w_E) +:+
  S "should equal the difference between" +:+
  S "the total" +:+ (phrase $ energy) +:+ phrase input_ +:+
  S "from the" +:+ (phrase $ coil) +:+ S "and the" +:+
  (phrase $ energy) +:+ phrase output_ +:+ S "to the" +:+.
  (short phsChgMtrl) +:+ S "This can be shown as an" +:+
  (phrase $ equation) +:+ S "by taking" +:+ (makeRef (swhsSymbMapD dd1HtFluxC)) +:+
  S "and" +:+ (makeRef (swhsSymbMapD dd2HtFluxP)) `sC` S "multiplying each by their" +:+
  S "respective" +:+ (phrase $ surface) +:+ S "area of" +:+
  (phrase $ CT.heat_trans) `sC`
  S "and integrating each over the" +:+ phrase simulation +:+
  (phrase $ time) `sC` S "as follows:"),
  EqnBlock 
  ((C w_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time))) 
  ((C coil_HTC) * (C coil_SA) * ((C temp_C) - FCall (C temp_W) 
  [C time])) time) - UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) -
  (FCall (C temp_PCM) [C time]))) time)),
  Paragraph (S "In addition, the" +:+ (phrase $ pcm_E) +:+ 
  S "should equal the" +:+ (phrase $ energy) +:+ phrase input_ +:+
  S "to the" +:+ short phsChgMtrl +:+ S "from the" +:+.
  (phrase $ water) +:+ S "This can be expressed as"),
  EqnBlock
  ((C pcm_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) - (FCall
  (C temp_PCM) [C time]))) time)),
  Paragraph ((titleize' $ equation) +:+ S "(reference) and" +:+
  S "(reference) can be used as" +:+ Quote (S "sanity") :+: S "checks to" +:+
  S "gain confidence in any" +:+ phrase solution +:+ S "computed by" +:+.
  (short progName) +:+ S "The relative error between the results" +:+
  S "computed by" +:+ (short progName) +:+ S "and the" +:+
  S "results calculated from the" +:+ (short rightSide) +:+
  S "of these" +:+ (plural $ equation) +:+.
  S "should be less than 0.001% (R9)")]

-- Above section only occurs in this example (although maybe it SHOULD be in
-- the others).

-- Remember to insert references in above derivation when available

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------

s5 :: Section
s5 = reqF [s5_1, s5_2]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1 = SRS.funcReq (s5_1_list) []

s5_1_list :: [Contents]
s5_1_list = [Enumeration (Simple [((short requirement) :+: S "1", Flat 
  (titleize input_ +:+ S "the following" +:+ plural quantity `sC`
  S "which define the" +:+ (phrase $ tank) +:+
  S "parameters, material" +:+ plural property +:+ S "and initial" +:
  plural condition))]),
  (Table [phrase symbol_, (phrase $ unit_), phrase description]
  (mkTable
  [(\ch -> P (ch ^. symbol)),
  (\ch -> Sy (unit_symb ch)),
  (\ch -> phrase $ ch)] inputVar) 
  ((titleize input_) +:+ (titleize variable) +:+ (titleize requirement)) False),
--
  Enumeration (Simple [((short requirement) :+: S "2", Flat 
  (S "Use the" +:+ plural input_ +:+ S "in R1 to find the" +:+
  (phrase $ mass) +:+ S "needed for IM1 to IM4, as follows, where" +:+
  P (w_vol ^. symbol) +:+ S "is the" +:+(phrase $ w_vol) +:+
  S "and" +:+ P (tank_vol ^. symbol) +:+ S "is the" +:+.
  (phrase $ tank_vol)))]),
  EqnBlock ((C w_mass) := (C w_vol) * (C w_density) := ((C tank_vol) -
  (C pcm_vol)) * (C w_density) := (((C diam) / 2) * (C tank_length) - 
  (C pcm_vol)) * (C w_density)),
  EqnBlock ((C pcm_mass) := (C pcm_vol) * (C pcm_density)),
--
  enumSimple 3 (short requirement) reqList
  ]

-- Want to add req1 and req2 but they include a table and another enumeration
-- so not sure how to implement yet

reqList :: [Sentence]

req3, req4, req5, req6, req7, req8, req9, req10, req11 :: Sentence

reqList = [req3, req4, req5, req6, req7, req8, req9, req10, req11]

req3 = S "Verify that the" +:+ plural input_ +:+ S "satisfy the required" +:+
  phrase physical +:+ plural constraint +:+ S "shown in" +:+ makeRef s7_table1
--
req4 = titleize output_ +:+ S "the" +:+
  phrase input_ +:+ plural quantity +:+ S "and derived" +:+ plural quantity +:+
  S "in the following list: the" +:+ plural quantity +:+ S "from R1, the" +:+
  (plural $ mass) +:+ S "from R2" `sC` P (tau_W ^. symbol) +:+
  sParen (S "from IM1") `sC` P (eta ^. symbol) +:+ sParen (S "from IM1") `sC`
  P (tau_S_P ^. symbol) +:+ S "(from IM2) and" +:+ P (tau_L_P ^. symbol) +:+.
  sParen (S "from IM2")
--
req5 = S "Calculate and" +:+
  phrase output_ +:+ S "the" +:+ (phrase $ temp_W) +:+ sParen
  (P (temp_W ^. symbol) :+: sParen (P $ time ^. symbol)) +:+
  S "over the" +:+ phrase simulation +:+ (phrase $ time) +:+.
  sParen (S "from IM1")
--
req6 = S "Calculate and" +:+ 
  phrase output_ +:+ S "the" +:+ (phrase $ temp_PCM) +:+ sParen
  (P (temp_PCM ^. symbol) :+: sParen (P $ time ^. symbol)) +:+
  S "over the" +:+ phrase simulation +:+ (phrase $ time) +:+.
  sParen (S "from IM2")
--
req7 = S "Calculate and" +:+ 
  phrase output_ +:+ S "the" +:+ (phrase $ w_E) +:+ sParen
  (P (w_E ^. symbol) :+: sParen (P $ time ^. symbol)) +:+
  S "over the" +:+ phrase simulation +:+ (phrase $ time) +:+.
  sParen (S "from IM3")
--
req8 = S "Calculate and" +:+ 
  phrase output_ +:+ S "the" +:+ (phrase $ pcm_E) +:+ sParen
  (P (pcm_E ^. symbol) :+: sParen (P $ time ^. symbol)) +:+ S "over the" +:+
  phrase simulation +:+ (phrase $ time) +:+. sParen (S "from IM4")
--
req9 = S "Verify that the" +:+
  (phrase $ energy) +:+ plural output_ +:+ sParen
  (P (w_E ^. symbol) :+: sParen (P $ time ^. 
  symbol) +:+ S "and" +:+ P (pcm_E ^. symbol) :+: sParen
  (P $ time ^. symbol)) +:+ S "follow the" +:+
  (phrase $ CT.law_cons_energy) `sC` S "as outlined in" +:+ 
  makeRef s4_2_7 `sC` S "with relative error no greater than" +:+.
  S "0.001%"
--
req10 = S "Calculate and" +:+ 
  phrase output_ +:+ S "the" +:+ (phrase $ time) +:+
  S "at which the" +:+ (short phsChgMtrl) +:+ S "begins to melt" +:+
  P (t_init_melt ^. symbol) +:+. sParen (S "from IM2")
--
req11 = S "Calculate and" +:+ 
  phrase output_ +:+ S "the" +:+ (phrase $ time) +:+
  S "at which the" +:+ (short phsChgMtrl) +:+
  S "stops" +:+ (phrase $ CT.melting) +:+
  EmptyS +:+ P (t_final_melt ^. symbol) +:+. sParen (S "from IM2")

-- List structure same between all examples

--How to include pi?
--How to add exponents?

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------

s5_2 :: Section
s5_2 = SRS.nonfuncReq [s5_2_contents] []

s5_2_contents :: Contents
s5_2_contents = Paragraph (S "Given the small size, and relative simplicity"
  `sC`
  S "of this" +:+ phrase problem `sC` (phrase $ performance) +:+
  S "is not a" +:+. phrase priority +:+
  S "Any reasonable implementation will be very quick and use" +:+
  S "minimal storage. Rather than" +:+ (phrase $ performance) `sC` S "the" +:+
  phrase priority +:+ plural nonfunctionalRequirement +:+
  S "are correctness, verifiability" `sC`
  S "understandability, reusability, and maintainability.")

-- The second sentence of the above paragraph is repeated in all examples (not 
-- exactly, but the general idea is). The first sentence is not always 
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be 
-- abstracted out.

--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6 = SRS.likeChg [s6_list] []

-- The game physics example has a short intro paragraph that can likely be 
-- abstracted out and used for all examples.

s6_list :: Contents
s6_list = enumSimple 1 (short likelyChg) s6_likeChg_list

s6_likeChg_list :: [Sentence]

likeChg1, likeChg2, likeChg3, likeChg4, likeChg5, likeChg6 :: Sentence

s6_likeChg_list = [likeChg1, likeChg2, likeChg3, likeChg4, likeChg5, likeChg6]

likeChg1 = S "A4 -" +:+ (short phsChgMtrl) +:+ S "is actually a poor" +:+
  (phrase $ CT.thermal_conductor) `sC` S "so" +:+
  S "the" +:+ (phrase assumption) +:+
  S "of uniform" +:+ (phrase $ temp_PCM) +:+. S "is not likely"
--
likeChg2 = S "A8 - The" +:+ (phrase $ temp_C) +:+
  S "will change over the course of the day, depending" +:+
  S "on the" +:+ (phrase $ energy) +:+. S "received from the sun"
--
likeChg3 = S "A9 - The" +:+ (phrase $ temp_C) +:+ S "will actually change along its length as the" +:+
  (phrase $ water) +:+. S "within it cools"
--
likeChg4 = S "A11 - The" +:+ phrase model +:+
  S "currently only accounts for" +:+. (charging ^. defn) +:+
  S "A more complete" +:+ phrase model +:+ S "would also" +:+
  S "account for" +:+. (discharging ^. defn)
--
likeChg5 = S "A12 - To add more" +:+
  S "flexibility to the" +:+ phrase simulation `sC`
  S "the" +:+ (phrase $ temp_init) +:+
  S "of the" +:+ (phrase $ water) +:+ 
  S "and the" +:+ (short phsChgMtrl) +:+ S "could be" +:+
  S "allowed to have different" +:+. plural value
--
likeChg6 = S "A15 - Any real" +:+
  (phrase $ tank) +:+ S "cannot be" +:+
  (phrase $ perfect_insul) +:+ S "and will lose" +:+.
  (phrase $ CT.heat)

-- List structure same in all examples.

--add referencing to assumptions?

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

s7 :: Section
s7 = traceMGF s7_refList s7_trailing
  ([s7_table1, s7_table2, s7_table3] ++ (s7_intro2) ++ [s7_fig1, s7_fig2]) []

s7_refList :: [Contents]
s7_refList = [s7_table1, s7_table2, s7_table3]

s7_trailing :: [Sentence]
s7_trailing = [

  plural thModel `sC` plural genDefn `sC` plural dataDefn `sC`
  S "and" +:+ plural inModel +:+. S "with each other",

  plural inModel `sC` plural requirement `sC` S "and" +:+ plural datum +:+
  plural constraint +:+. S "on each other",

  plural thModel `sC` plural genDefn `sC` plural dataDefn `sC`
  plural inModel `sC` S "and" +:+ plural likelyChg +:+ S "on the" +:+
  plural assumption

  ]

s7_table1 :: Contents
s7_table1 = Table [EmptyS, (makeRef (swhsSymbMapT t1ConsThermE)), (makeRef (swhsSymbMapT t2SensHtE)), 
  (makeRef (swhsSymbMapT t3LatHtE)), S "GD1", S "GD2", (makeRef (swhsSymbMapD dd1HtFluxC)), 
  (makeRef (swhsSymbMapD dd2HtFluxP)),(makeRef (swhsSymbMapD dd3HtFusion)),(makeRef (swhsSymbMapD dd3HtFusion)), S "IM1",
  S "IM2", S "IM3", S "IM4"]
  [[(makeRef (swhsSymbMapT t1ConsThermE)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapT t2SensHtE)), EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapT t3LatHtE)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "GD1", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "GD2", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapD dd1HtFluxC)), EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapD dd2HtFluxP)), EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapD dd3HtFusion)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapD dd3HtFusion)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "IM1", EmptyS, EmptyS, EmptyS, EmptyS, S "X", S "X", S "X", EmptyS,
  EmptyS, EmptyS, S "X", EmptyS, EmptyS],
  [S "IM2", EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, S "X", EmptyS,
  S "X", S "X", EmptyS, EmptyS, S "X"],
  [S "IM3", EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "IM4", EmptyS, S "X", S "X", EmptyS, EmptyS, EmptyS, S "X", S "X", S "X",
  EmptyS, S "X", EmptyS, EmptyS]]
  (showingCxnBw traceyMatrix (titleize' item +:+ S "of Different" +:+ titleize' section_))
  True

-- Wrong DD reference above, change when DD4 is available (twice)

s7_table2 :: Contents
s7_table2 = Table [EmptyS, S "IM1", S "IM2", S "IM3", S "IM4", makeRef s4_2,
  S "R1", S "R2"]
  [[S "IM1", EmptyS, S "X", EmptyS, EmptyS, EmptyS, S "X", S "X"],
  [S "IM2", S "X", EmptyS, EmptyS, S "X", EmptyS, S "X", S "X"],
  [S "IM3", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", S "X"],
  [S "IM4", EmptyS, S "X", EmptyS, EmptyS, EmptyS, S "X", S "X"],
  [S "R1", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R2", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS],
  [S "R3", EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS],
  [S "R4", S "X", S "X", EmptyS, EmptyS, EmptyS, S "X", S "X"],
  [S "R5", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R6", EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R7", EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R8", EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS],
  [S "R9", EmptyS, EmptyS, S "X", S "X", EmptyS, EmptyS, EmptyS],
  [S "R10", EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R11", EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS]]
  (showingCxnBw traceyMatrix (titleize' requirement +:+ S "and" +:+ titleize' inModel))
  True

s7_table3 :: Contents
s7_table3 = Table [EmptyS, S "A1", S "A2", S "A3", S "A4", S "A5", S "A6",
  S "A7", S "A8", S "A9", S "A10", S "A11", S "A12", S "A13", S "A14",
  S "A15", S "A16", S "A17", S "A18", S "A19"]
  [[(makeRef (swhsSymbMapT t1ConsThermE)), S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapT t2SensHtE)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapT t3LatHtE)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "GD1", EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [S "GD2", EmptyS, EmptyS, S "X", S "X", S "X", S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS],
  [(makeRef (swhsSymbMapD dd1HtFluxC)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X",
  S "X", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [(makeRef (swhsSymbMapD dd2HtFluxP)), EmptyS, EmptyS, S "X", S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapD dd3HtFusion)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [(makeRef (swhsSymbMapD dd3HtFusion)), EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "IM1", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, S "X", S "X", EmptyS, S "X", S "X", S "X", EmptyS, EmptyS,
  S "X"],
  [S "IM2", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, S "X", S "X", EmptyS, EmptyS, S "X", S "X", S "X",
  EmptyS],
  [S "IM3", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, S "X"],
  [S "IM4", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, S "X",
  EmptyS],
  [S "LC1", EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [S "LC2", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X",
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [S "LC3", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [S "LC4", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [S "LC5", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [S "LC6", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS,
  EmptyS, EmptyS]]
  (showingCxnBw traceyMatrix (titleize' assumption +:+ S "and Other" +:+ titleize' item))
  True

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

-- Wrong DD reference above, change when DD4 is available

------------------------
-- Traceabilty Graphs --
------------------------

s7_intro2 :: [Contents]
s7_intro2 = traceGIntro [s7_fig1, s7_fig2] [(plural thModel `sC` plural genDefn `sC`
  plural dataDefn `sC` plural inModel `sC` plural likelyChg `sC` 
  S "and" +:+ plural assumption +:+. S "on each other"), (plural inModel
  `sC` plural requirement `sC` S "and" +:+ plural datumConstraint +:+
  S "on each other")]

-- Same comments on this paragraph as I had for s7_intro1. 

s7_fig1 :: Contents
s7_fig1 = Figure (
  showingCxnBw traceyGraph (titleize' item +:+ S "of Different" +:+ titleize' section_)
  ) "ATrace.png"

s7_fig2 :: Contents
s7_fig2 = Figure (
  showingCxnBw traceyGraph ((titleize' requirement) `sC`
  titleize' inModel `sC` S "and" +:+ titleize' datumConstraint)
  ) "RTrace.png"

--References?