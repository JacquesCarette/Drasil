module Drasil.SWHS.Body where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Data.Drasil.SI_Units 
import Data.Drasil.Authors

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.PhysicalProperties hiding (density, mass)
import Data.Drasil.Concepts.Thermodynamics hiding (temp)
import Data.Drasil.Concepts.Math (ode, graph, unit_)

import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.Math (gradient, normalVect, surface)
import Data.Drasil.Quantities.Thermodynamics (temp)
import Data.Drasil.Quantities.PhysicalProperties (density, mass)

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

acronyms :: [CINP]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ode,
  phsChgMtrl,physSyst,requirement,rightSide,srs,progName,thModel]

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

--Will there be a table of contents?

s2, s2_1, s2_2, s2_3, s2_4, s3, s3_1, s3_2, s4, s4_1,
  s4_1_1, s4_1_2, s4_1_3, s4_2, s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5,
  s4_2_6, s4_2_7, s5, s5_1, s5_2, s6, s7 :: Section

s2_2_contents, s2_3_contents, s3_intro, s3_1_contents, s3_2_contents, s4_intro, 
  s4_1_intro, s4_1_1_intro, s4_1_1_bullets, s4_1_2_intro, s4_1_2_list,
  fig_tank, s4_1_3_intro, s4_1_3_list, s4_2_intro, s4_2_1_intro, 
  s4_2_1_list, s4_2_2_intro, s4_2_3_intro, s4_2_4_intro, s4_2_6_intro, 
  s5_intro, s5_2_contents, s6_list, s7_intro1, s7_table1, s7_table2,
  s7_table3, s7_fig1, s7_fig2 :: Contents
  
s2_intro, s2_1_contents, s2_4_contents, s4_2_3_deriv, s4_2_5_intro, 
  s4_2_5_deriv1, s4_2_5_deriv2, s4_2_7_deriv, s5_1_list, s7_intro2
  :: [Contents]

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
  [ TUnits, tsymb'' tsymb_intro (TermExcept [normalVect]), TAandA ]
  ) : map Verbatim [s2, s3, s4, s5, s6, s7]

tsymb_intro :: [TSIntro]
tsymb_intro = [TSPurpose,SymbConvention [Lit (nw heat_trans),
  Doc (nw progName)], SymbOrder]

swhs_srs' :: Document
swhs_srs' = mkDoc mkSRS swhs_si

-- It is sometimes hard to remember to add new sections both here and above.

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

swhs_mg :: Document
swhs_mg = mgDoc swhsFull authors mgBod
  
-- This section name and table structure are same between all examples.
  
s2 = SRS.intro $ ((map Con s2_intro)++[Sub s2_1, Sub s2_2, Sub s2_3, Sub s2_4])

s2_intro = [Paragraph (S "Due to increasing cost, diminishing" +:+
  S "availability, and negative environmental impact of" +:+
  S "fossil fuels, there is a higher demand for renewable" +:+.
  S "energy sources and energy storage technology" +:+
  (swhs_pcm ^. defn) +:+ S "(" :+: (short phsChgMtrl) :+: 
  S ") use a renewable energy source and provide a novel way of" +:+.
  S "storing energy" +:+ (phrase $ swhs_pcm ^. term) +:+ S "improve" +:+
  S "over the traditional" +:+ (phrase $ progName ^. term) :+: 
  S "s because of their smaller size. The smaller size" +:+
  S "is possible because of the ability of" +:+ 
  (short phsChgMtrl) +:+ S "to store" +:+
  (phrase $ thermal_energy ^. term) +:+ S "as" +:+
  (phrase $ latent_heat ^. term) :+: S ", which" +:+
  S "allows higher" +:+ (phrase $ thermal_energy ^. term) +:+
  S "storage capacity per" +:+ (phrase $ unit_ ^. term) +:+. S "weight"),
  Paragraph (S " The following" +:+ phrase section_ +:+ S "provides an" +:+
  S "overview of the" +:+ titleize srs +:+ S "(" :+: (short srs) :+:
  S ") for" +:+ (phrase $ swhs_pcm ^. term) :+: S ". The developed" +:+
  S "program will be referred to as" +:+ (titleize $ progName ^. term) +:+
  S "(" :+: (short progName) :+: S "). This" +:+ phrase section_ +:+
  S "explains the" +:+ phrase purpose +:+ S "of this" +:+ phrase document :+:
  S ", the" +:+ phrase scope +:+ S "of the" +:+ phrase system :+: S ", the" +:+
  phrase organization +:+ S "of the" +:+ phrase document +:+ S  "and the" +:+
  plural characteristic +:+ S "of the" +:+ S "intended readers.")]
 
-- In Concepts.hs "swhs_pcm" gives "solar water heating systems incorporating
-- PCM" which is not capitlaized whereas the stable version is

-- NamedChunks... Sometimes capitalized, sometimes not, sometimes plural, 
-- sometimes not, sometimes need to be used in different tenses. How to 
-- accomodate all this?

-- The second paragraph is general except for program name, and there is a 
-- similar paragraph in each of the other examples. It can probably be 
-- abstracted out.

s2_1 = section (titleize prpsOfDoc) (s2_1_contents) []

s2_1_contents = [Paragraph (S "The main" +:+ phrase purpose +:+ S "of this" +:+
  phrase document +:+ S "is to describe the modelling of" +:+
  (phrase $ swhs_pcm ^. term) :+: S ". The" +:+ plural goalStmt +:+
  S "and" +:+ plural thModel +:+ 
  S "used in the" +:+ (short progName) +:+ S "code" +:+
  S "are provided, with an emphasis on explicitly identifying" +:+ 
  (plural assumption) +:+ 
  S "and unambiguous" +:+ plural definition :+: S ". This" +:+
  phrase document +:+ S "is intended to be used as a reference to provide" +:+
  S "ad hoc access to all" +:+ phrase information +:+ S "necessary to" +:+
  S "understand and verify the" +:+ phrase model :+: S ". The" +:+
  (short srs) +:+ S "is abstract because the contents say what" +:+
  phrase problem +:+ S "is being solved, but do not say how to solve it."),
  Paragraph (S "This" +:+ phrase document +:+ S "will be used as a" +:+
  S "starting point for subsequent development phases, including" +:+ 
  S "writing the" +:+ phrase desSpec +:+ S "and the software" +:+
  (phrase vav) +:+ S "plan. The design" +:+ phrase document +:+
  S "will show how the" +:+ (plural requirement) +:+
  S "are to be realized, including decisions" +:+.
  S "on the numerical algorithms and programming environment" +:+
  S "The" +:+ phrase vav +:+ S "plan will show the" +:+
  S "steps that will be used to increase confidence in the" +:+
  S "software documentation and the implementation. Although" +:+
  S "the" +:+ (short srs) +:+ S "fits in a series of" +:+ 
  plural document +:+ S "that follow the so-called waterfall" +:+
  phrase model :+: S ", the actual development process is not constrained" +:+
  S "in any way. Even when the process is not waterfall, as Parnas" +:+
  S "and Clements [citation] point out, the most logical way" +:+
  S "to present the documentation is still to" +:+
  Quote (S "fake") +:+. S "a rational design process")]

-- Besides program name, these two paragraphs are general, mostly repeated 
-- between examples, and can be abstracted out.

--How to italicize words in sentence?
--How to cite?

s2_2 = section (titleize' scpOfReq) [s2_2_contents] []

s2_2_contents = Paragraph (S "The" +:+ phrase scope +:+ S "of the" +:+
  plural requirement +:+ S "is limited to" +:+
  (phrase $ thermal_analysis ^. term) +:+ S "of a single" +:+.
  (phrase $ tank_pcm ^. term) +:+ --FIXME: Caps issue
  S "Given the appropriate inputs, the code for" +:+
  (short progName) +:+ S "is intended to predict the" +:+
  (phrase $ temp ^. term) +:+ S "and" +:+ (phrase $ thermal_energy ^. term) +:+
  S "histories for the" +:+ (phrase $ water ^. term) +:+ S "and the" +:+
  (short phsChgMtrl) :+: S ". This entire" +:+ phrase document +:+
  S "is written assuming that the substances inside the" +:+
  (tank ^. defn) +:+ S "are" +:+ (phrase $ water ^. term) +:+ S "and" +:+.
  (short phsChgMtrl))

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

s2_3 = section (of'' titleize' titleize characteristic intReader) (s2_4_contents) []

s2_3_contents = Paragraph (S "Reviewers of this documentation should" +:+
  S "have a strong knowledge in heat transfer theory. A third or fourth" +:+
  S "year Mechanical Engineering course on this topic is recommended." +:+
  S "The reviewers should also have an understanding of differential" +:+
  S "equations, as typically covered in first and second year Calculus" +:+
  S "courses. The users of SWHS can have a lower level of expertise, as" +:+
  S "explained in" +:+. (makeRef s3_2))



s2_4 = section (titleize orgOfDoc) (s2_4_contents) []

s2_4_contents = [Paragraph (S "The" +:+ phrase organization +:+ S "of this" +:+
  phrase document +:+ S "follows the template for an" +:+ (short srs) +:+
  S "for" +:+ phrase sciCompS +:+ S "proposed by [citation] and" +:+
  S "[citation]. The presentation follows the standard" +:+
  S "pattern for presenting" +:+ plural goalStmt `sC`
  plural thModel `sC`
  (plural dataDefn) `sC` S "and" +:+. 
  (plural assumption) +:+
  S "For readers that would like a more bottom" :+: 
  S " up approach, they can start reading the" +:+ 
  (plural inModel) +:+ S "in" +:+ 
  makeRef s4_2_5 +:+ S "and trace back to find any" +:+
  S "additional" +:+ phrase information +:+ S "they require. The" +:+
  (plural inModel) +:+ S "provide" +:+
  S "the" +:+ (phrase $ ode ^. term) +:+ S "(" :+: (short ode) :+:
  S "s) and algebraic equations that" +:+ phrase model +:+ S "the" +:+.
  (phrase $ swhs_pcm ^. term) +:+ (short progName) +:+
  S "solves these" +:+ (short ode) :+: S "s."),
  --FIXME: Update refineChain after fixing goalStmt and thModel
  Paragraph $ refineChain [goalStmt, thModel, inModel] +:+
  S "The" +:+ plural inModel +:+ sParen (makeRef s4_2_5) +:+. 
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

s3 = section (titleize generalSystemDescription) [s3_intro] [s3_1, s3_2]

s3_intro = Paragraph (S "This" +:+ phrase section_ +:+ S "provides" +:+
  phrase general +:+ phrase information +:+ S "about the" +:+ phrase system :+:
  S ", identifies" +:+ S "the interfaces between the" +:+ phrase system +:+
  S "and its environment, and describes the user" +:+ plural characteristic +:+
  S "and the" +:+ phrase system +:+ plural constraint :+: S ".")

-- Completely general paragraph, same between examples. Easily abstracted out.

s3_1 = section (titleize' userCharacteristic) [s3_1_contents] []

s3_1_contents = Paragraph (S "The end user of" +:+ (short progName) :+: 
  S " should have an understanding of undergraduate Level 1" +:+.
  S "Calculus and Physics")

-- Some of these course names are repeated between examples, could potentially 
-- be abstracted out.

s3_2 = section (titleize' systemConstraint) [s3_2_contents] []

s3_2_contents = Paragraph (S "There are no" +:+ phrase system +:+
  plural constraint :+: S ".")

-- This is the same for all of our examples... but there could potentially be 
-- system constraints in other projects so it can't be abstracted out as is...

s4 = section (titleize specificsystemdescription) [s4_intro] [s4_1, s4_2]

s4_intro = Paragraph (S "This" +:+ phrase section_ +:+ S "first presents" +:+
  S "the" +:+ phrase problem +:+ phrase description :+: S ", which gives a" +:+
  S "high-level view of the" +:+ phrase problem +:+ S "to be solved. This" +:+
  S "is followed by the" +:+ phrase solution +:+
  phrase characteristicSpecification :+: S ", which presents the" +:+
  (plural assumption) `sC` 
  plural thModel `sC`
  (plural genDefn) `sC` 
  (plural dataDefn) `sC` S "and finally" +:+
  S "the" +:+ plural inModel +:+ S "(" :+:
  (short ode) :+: S "s) that" +:+ phrase model +:+ S "the" +:+.
  (phrase $ swhs_pcm ^. term))

-- Completely general except for solar water heating tank (object of analysis) 
-- and similar between all examples; can be abstracted out.
 
-- The swhs_pcm reference at the end would be better if singular, but concept
-- is plural.

s4_1 = section (titleize problemDescription) [s4_1_intro]
  [s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = Paragraph ((short progName) +:+ S "is a computer program" +:+
  S "developed to investigate the effect of employing" +:+
  (short phsChgMtrl) +:+ S "within a" +:+. (tank ^. defn))

--  section is very different between all examples

s4_1_1 = section (titleize' $ terminology `and_'` definition)
  [s4_1_1_intro, s4_1_1_bullets] []

s4_1_1_intro = Paragraph (S "This subsection provides a list of terms" +:+
  S "that are used in the subsequent" +:+ plural section_ +:+ S "and their" +:+
  S "meaning, with the" +:+ phrase purpose +:+ S "of reducing ambiguity" +:+
  S "and making it easier to correctly understand the" +:+
  plural requirement :+: S ":")

-- Above paragraph is repeated in all examples, can be abstracted out. (Note: 
-- GlassBR has an additional sentence with a reference at the end.)

s4_1_1_bullets = Enumeration (Bullet $ map s411_bullet_map_f [heat_flux,
   phase_change_material, specific_heat, 
   thermal_conduction, transient])

s411_bullet_map_f :: Concept c => c -> ItemType
s411_bullet_map_f c = Flat ((at_start $ c ^. term) :+: S ":" +:+. (c ^. defn))
  
-- Structure of this list is same in all examples, probably can be automated.

-- Included heat flux and specific heat in NamedChunks even though they are 
-- already in SWHSUnits

s4_1_2 = section (titleize physSyst) [s4_1_2_intro, s4_1_2_list, fig_tank] []

s4_1_2_intro = Paragraph (S "The" +:+ phrase physicalSystem +:+ S "of" +:+
  (short progName) :+: S ", as shown in" +:+ (makeRef fig_tank) :+:
  S ", includes the following elements:")

-- Above paragraph is general except for progName and figure. However, not 
-- every example has a physical system. Also, the SSP example is different, so
-- this paragraph can not be abstracted out as is.

s4_1_2_list = Enumeration (Simple $ [((short physSyst) :+: S "1", Flat
  ((phrase $ tank ^. term) +:+ S "containing" +:+. (phrase $ water ^. term))),
--
  ((short physSyst) :+: S "2", Flat ((phrase $ coil ^. term) :+: 
  S " at bottom of" +:+. (phrase $ tank ^. term) +:+
  sParen (P (ht_flux_C ^. symbol) +:+ S "represents the" +:+.
  (phrase $ ht_flux_C ^. term)))),
--
  ((short physSyst) :+: S "3", Flat ((short phsChgMtrl) :+: 
  S " suspended in" +:+. (phrase $ tank ^. term) +:+
  sParen (P (ht_flux_P ^. symbol) +:+ S "represents the" +:+.
  (phrase $ ht_flux_P ^. term))))])

-- Structure of list would be same between examples but content is completely 
-- different
-- FIXME: Figures have different IDs than stable structure

fig_tank = Figure ((tank ^. defn) :+: S ", with" +:+
  (phrase $ ht_flux_C ^. term) +:+ S "of" +:+ P (ht_flux_C ^. symbol) +:+
  S "and" +:+ (phrase $ ht_flux_P ^. term) +:+ S "of" +:+
  P (ht_flux_P ^. symbol)) "Tank.png"

s4_1_3 = section (titleize' goalStmt) [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro = Paragraph (S "Given the" +:+ (phrase $ temp_C ^. term) :+:
  S ", initial" +:+ plural condition +:+ S "for the" +:+
  (phrase $ temp_W ^. term) +:+ S "and the" +:+ (phrase $ temp_PCM ^. term) :+:
  S ", and material properties, the" +:+ plural goalStmt +:+ S "are:")

-- 2 examples include this paragraph, 2 don't. The "givens" would need to be 
-- abstracted out if this paragraph were to be abstracted out.

s4_1_3_list = Enumeration (Simple [((short goalStmt) :+: S "1", Flat 
  (S "Predict the" +:+ (phrase $ temp_W ^. term) +:+ S "over" +:+.
  (phrase $ time ^. term))),
--
  ((short goalStmt) :+: S "2", Flat (S "Predict the" +:+
  (phrase $ temp_PCM ^. term) +:+ S "over" +:+. (phrase $ time ^. term))),
--
  ((short goalStmt) :+: S "3", Flat (S "Predict the" +:+
  (phrase $ w_E ^. term) +:+ S "over" +:+. (phrase $ time ^. term))),
--
  ((short goalStmt) :+: S "4", Flat (S "Predict the" +:+
  (phrase $ pcm_E ^. term) +:+ S "over" +:+. (phrase $ time ^. term)))])

-- List structure is repeated between examples. (For all of these lists I am 
-- imagining the potential for something like what was done with the lists in 
-- MG, where you define goals, assumptions, physical system components, etc. in
-- separate files, import them and pass them as arguments to some "makeSRS" 
-- function and the rest is automated.)

s4_2 = section (titleize solution +:+ titleize' characteristic +:+
  titleize specification) [s4_2_intro] [s4_2_1, s4_2_2,
  s4_2_3, s4_2_4, s4_2_5, s4_2_6, s4_2_7]

s4_2_intro = Paragraph (S "The" +:+ plural inModel +:+
  S "(" :+: (short ode) :+: S "s) that govern" +:+
  (short progName) +:+ S "are" +:+ S "presented in" +:+. 
  (makeRef s4_2_5) +:+ S "The" +:+ phrase information +:+ S "to" +:+
  S "understand the meaning of the" +:+ (plural inModel) +:+
  S "and their derivation is also presented, so that the" +:+
  (plural inModel) +:+. S "can be verified")

-- General besides progName, repeated in only one other example but it could be
-- used for all of them. So it can be abstracted out.

s4_2_1 = section (titleize' assumption) [s4_2_1_intro, s4_2_1_list] []

s4_2_1_intro = Paragraph (S "This" +:+ phrase section_ +:+ S "simplifies" +:+
  S "the original" +:+ phrase problem +:+ S "and helps in developing the" +:+
  phrase thModel +:+ S "by filling in the missing" +:+
  phrase information +:+ S "for the" +:+ phrase physicalSystem :+:
  S ". The numbers given in the square brackets refer to the" +:+ 
  phrase thModel +:+ S "[" :+: (short thModel) :+: 
  S "]," +:+ (phrase genDefn) +:+ 
  S "[" :+: (short genDefn) :+: S "]" `sC` (phrase dataDefn) +:+ S "[" :+: 
  (short dataDefn) :+: S "]," +:+ (phrase inModel) +:+
  S "[" :+: (short inModel) :+: S "], or" +:+ phrase likelyChg +:+ 
  S "[" :+: (short likelyChg) :+: S "], in which the respective" +:+
  (phrase assumption) +:+. S "is used") 

-- General paragraph, repeated in every example. Can be abstracted out.

s4_2_1_list = Enumeration (Simple [((short assumption) :+: S "1", Flat 
  (S "The only form of energy that is relevant for this" +:+ phrase
  problem +:+ S "is" +:+ (phrase $ thermal_energy ^. term) :+:
  S ". All other forms of energy, such as" +:+
  (phrase $ mech_energy ^. term) :+: S ", are assumed to be" +:+
  S "negligible [" :+: (makeRef s4_2_2_T1) :+: S "].")),
--
  ((short assumption) :+: S "2", Flat (S "All" +:+
  (phrase $ heat_trans ^. term) +:+ S "coefficients are constant over" +:+
  (phrase $ time ^. term) +:+. S "[GD1]")),
--
  ((short assumption) :+: S "3", Flat (S "The" +:+ 
  (phrase $ water ^. term) +:+ S "in the" +:+ (phrase $ tank ^. term) +:+
  S "is fully mixed, so the" +:+ (phrase $ temp_W ^. term) +:+
  S "is the same throughout the entire" +:+ (phrase $ tank ^. term) +:+
  S "[GD2" `sC` makeRef s4_2_4_DD2 :+: S "].")),
--
  ((short assumption) :+: S "4", Flat (S "The" +:+ (phrase $ temp_PCM ^.
  term) +:+ S "is the same throughout the" +:+ (phrase $ pcm_vol ^. 
  term) +:+ S "[GD2" `sC` makeRef s4_2_4_DD2 `sC` S "LC1].")),
--
  ((short assumption) :+: S "5", Flat (S "The" +:+ 
  (phrase $ w_density ^. term) +:+ S "and" +:+
  (phrase $ pcm_density ^. term) +:+ S "have no spatial variation; that" +:+
  S "is, they are each constant over their entire" +:+
  (phrase $ volume ^. term) +:+. S "[GD2]")),
--
  ((short assumption) :+: S "6", Flat (S "The" +:+ (phrase $ htCap_W ^.
  term) :+: S "," +:+ (phrase $ htCap_S_P ^. term) :+: S ", and" +:+ 
  (phrase $ htCap_L_P ^. term) +:+ S "have no spatial variation; that" +:+
  S "is, they are each constant over their entire" +:+
  (phrase $ volume ^. term) +:+. S "[GD2]")),
--
  ((short assumption) :+: S "7", Flat ((law_conv_cooling ^. defn) +:+
  S "applies between the" +:+ (phrase $ coil ^. term) +:+ S "and the" +:+
  (phrase $ water ^. term) +:+ S "[" :+: makeRef s4_2_4_DD1 :+: S "].")),
--
  ((short assumption) :+: S "8", Flat (S "The" +:+ (phrase $ temp_C ^. 
  term) +:+ S "is constant over" +:+ (phrase $ time ^. term) +:+
  S "[" :+: makeRef s4_2_4_DD1 `sC` S "LC2].")),
--
  ((short assumption) :+: S "9", Flat (S "The" +:+ (phrase $ temp_C ^.
  term) +:+ S "does not vary along its length [" :+:
  makeRef s4_2_4_DD1 `sC` S "LC3].")),
--
  ((short assumption) :+: S "10", Flat ((law_conv_cooling ^. 
  defn) +:+ S "applies between the" +:+
  (phrase $ water ^. term) +:+ S "and the" +:+ (short phsChgMtrl) +:+
  S "[" :+: makeRef s4_2_4_DD2 :+: S "].")),
--
  ((short assumption) :+: S "11", Flat (S "The" +:+ phrase model +:+
  S "only accounts for" +:+ (charging ^. defn) :+:
  S ", not" +:+ (phrase $ discharging ^. term) :+:
  S ". The" +:+ (phrase $ temp_W ^. term) +:+ S "and" +:+ 
  (phrase $ temp_PCM ^. term) +:+ S "can only increase, or remain" +:+
  S "constant; they do not decrease. This implies that the" +:+
  (phrase $ temp_init ^. term) +:+ S "(A12) is less than (or equal)" +:+
  S "to the" +:+ (phrase $ temp_C ^. term) +:+. S "[IM1, LC4]")),
--
  ((short assumption) :+: S "12", Flat (S "The" +:+
  (phrase $ temp_init ^. term) +:+ S "of the" +:+
  (phrase $ water ^. term) +:+ S "and the" +:+ (short phsChgMtrl) +:+
  S "is the same" +:+. S "[IM1, IM2, LC5]")),
--
  ((short assumption) :+: S "13", Flat (S "The simulation" +:+
  S "will start with the" +:+ (short phsChgMtrl) +:+
  S "in a" +:+ (solid ^. defn) +:+.
  S "[IM2, IM4]")),
--
  ((short assumption) :+: S "14", Flat (S "The operating" +:+
  (phrase $ temp ^. term) +:+ S "range of the" +:+ phrase system +:+
  S "is such that the" +:+ (phrase $ water ^. term) +:+
  S "is always in" +:+ (liquid ^. defn) :+: S ". That is," +:+
  S "the" +:+ (phrase $ temp ^. term) +:+ S "will not drop below the" +:+
  (phrase $ temp_melt ^. term) +:+ S "of" +:+
  (phrase $ water ^. term) :+: S ", or rise above its" +:+
  (phrase $ temp_boil ^. term) +:+. S "[IM1, IM3]")),
--
  ((short assumption) :+: S "15", Flat (S "The" +:+
  (phrase $ tank ^. term) +:+ S "is" +:+ 
  (phrase $ perfect_insul ^. term) +:+ S "so that there is no heat" +:+
  S "loss from the" +:+ (phrase $ tank ^. term) +:+. S "[IM1, LC6]")),
--
  ((short assumption) :+: S "16", Flat (S "No internal heat" +:+
  S "is generated by either the" +:+ (phrase $ water ^. term) +:+
  S "or the" +:+ (short phsChgMtrl) :+: S "; therefore, the" +:+
  (phrase $ vol_ht_gen ^. term) +:+. S "is zero [IM1, IM2]")),
--
  ((short assumption) :+: S "17", Flat (S "The volume" +:+ 
  S "change of the" +:+ (short phsChgMtrl) +:+ S "due to" +:+ 
  (phrase $ melting ^. term) +:+. S "is negligible [IM2]")),
--
  ((short assumption) :+: S "18", Flat (S "The" +:+ 
  (short phsChgMtrl) +:+ S "is either in a" +:+
  (liquid ^. defn) +:+ S "or a" +:+ (solid ^. defn) +:+
  S "but not a" +:+ (gaseous ^. defn) +:+. S "[IM2, IM4]")),
--
  ((short assumption) :+: S "19", Flat (S "The pressure in" +:+ S "the" +:+
  (phrase $ tank ^. term) +:+ S "is atmospheric, so the" +:+
  (phrase $ temp_melt ^. term) +:+ S "and" +:+ (phrase $ temp_boil ^. term) +:+
  S "are 0" :+: Sy (unit_symb temp) +:+ S "and 100" :+: Sy (unit_symb temp) `sC`
  S "respectively [IM1, IM3]."))])

-- Again, list structure is same between all examples.

-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.

s4_2_2 = section (titleize' thModel)
  [s4_2_2_intro, s4_2_2_T1, s4_2_2_T2, s4_2_2_T3] []

s4_2_2_intro = Paragraph (S "This" +:+ phrase section_ +:+ S "focuses on" +:+
  S "the" +:+ phrase general +:+ S "equations and laws that" +:+
  (short progName) +:+ S "is based on.")

-- General paragraph (besides progName), repeated in all examples. Can be 
-- abstracted out.

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

-- No subsubsubsections... may make things difficult for derivation sections
-- coming up

s4_2_3 = section (titleize' genDefn) ((s4_2_3_intro):(s4_2_3_deriv)) []

s4_2_3_intro = Paragraph (S "This" +:+ phrase section_ +:+ S "collects the" +:+
  S "laws and equations that will be used in deriving the" +:+ 
  (plural dataDefn) `sC` S "which in turn are used to" +:+
  S "build the" +:+. (plural inModel) +:+ S "(" :+: at_start' genDefn +:+
  S "are left out because they are not" +:+
  S "currently implemented in Drasil.)")

-- General paragraph, repeated in one other example but could be included in 
-- all. Can be abstracted out.
 
-- s4_2_3_GDs :: [LayoutObj]
-- s4_2_3_GDs = map Definition (map General [gd1NewtonCooling])

--General definitions not yet implemented

s4_2_3_deriv = [Paragraph (S "Detailed derivation of simplified rate of" +:+
  S "change of" +:+ (phrase $ temp ^. term) :+: S ":"),
  Paragraph (S "Integrating" +:+ makeRef s4_2_2_T1 :+: 
  S " over a" +:+ (phrase $ volume ^. term) +:+ S "(" :+:
  P (volume ^. symbol) :+: S "), we have:"),
  EqnBlock 
  ((Neg (UnaryOp (Integral (Just (Low (C volume)), Nothing)
  ((C gradient) :. (C thFluxVect)) volume))) + 
  UnaryOp (Integral (Just (Low (C volume)), Nothing) 
  (C vol_ht_gen) volume) := 
  UnaryOp (Integral (Just (Low (C volume)), Nothing) ((C density) 
  * (C htCap) * Deriv Part (C temp) (C time)) volume)),
  Paragraph (S "Applying" +:+ (phrase $ gauss_div ^. term) +:+ S "to" +:+
  S "the first term over the" +:+ (phrase $ surface ^. term) +:+
  P (surface ^. symbol) +:+ S "of the" +:+ 
  (phrase $ volume ^. term) :+: S ", with" +:+ P (thFluxVect ^. 
  symbol) +:+ S "as the" +:+ (phrase $ thFluxVect ^. term) +:+
  S "for the" +:+ (phrase $ surface ^. term) +:+ S "and" +:+
  P (normalVect ^. symbol) +:+ S "as a" +:+ (normalVect ^.
  defn) :+: S ":"),
  EqnBlock 
  ((Neg (UnaryOp (Integral (Just (Low (C surface)), Nothing) 
  ((C thFluxVect) :. (C normalVect)) surface))) + 
  (UnaryOp (Integral (Just 
  (Low (C volume)), Nothing) (C vol_ht_gen) volume)) := 
  UnaryOp (Integral (Just (Low (C volume)), Nothing) 
  ((C density) * (C htCap) * Deriv Part (C temp) (C time)) volume)),
  Paragraph (S "We consider an arbitrary" +:+ (phrase $ volume ^. 
  term) :+: S ". The" +:+ (phrase $ vol_ht_gen ^. term) :+: S "is" +:+
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
  S "are constant over the" +:+ (phrase $ volume ^. term) :+: S "," +:+ 
  S "which is true in our case by" +:+ (titleize' assumption) +:+ 
  S "(A3), (A4), (A5), and (A6), we have:"),
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

s4_2_4 = section (titleize' dataDefn) [s4_2_4_intro, s4_2_4_DD1, s4_2_4_DD2,
  s4_2_4_DD3] []

s4_2_4_intro = Paragraph (S "This" +:+ phrase section_ +:+ S "collects and" +:+
  S "defines all the data needed to build the" +:+. plural inModel +:+
  S "The dimension of each quantity is also given.")

-- General paragraph, repeated in most examples but would work for all. Can be 
-- absracted out.

s4_2_5 = section (titleize' inModel) ((s4_2_5_intro) ++ (s4_2_5_deriv1) ++
  (s4_2_5_deriv2)) []

s4_2_5_intro = [Paragraph (S "This" +:+ phrase section_ +:+ S "transforms" +:+
  S "the" +:+ phrase problem +:+ S "defined in" +:+ (makeRef s4_1) +:+
  S "into one which is expressed in mathematical terms. It uses concrete" +:+
  plural symbol_ +:+ S "defined in" +:+ (makeRef s4_2_4) +:+
  S "to replace the abstract" +:+ plural symbol_ +:+ S "in the" +:+
  plural model +:+ S "identified in" +:+ (makeRef s4_2_2) +:+ S "and" +:+.
  (makeRef s4_2_3)), 
  Paragraph (S "The goals GS1 to GS4 are solved by IM1 to IM4." +:+
  S "The" +:+ plural solution +:+ S "for IM1 and IM2 are coupled since" +:+
  S "the" +:+ phrase solution +:+ S "for" +:+ P (temp_W ^. symbol) +:+
  S "and" +:+ P (temp_PCM ^. symbol) +:+ S "depend on one another. IM3" +:+
  S "can be solved once IM1 has been solved. The" +:+ phrase solution +:+
  S "of IM2 and IM4 are also coupled, since the" +:+ 
  (phrase $ temp_PCM ^. term) +:+ S "and" +:+ (phrase $ pcm_E ^. term) +:+
  S "depend on the" +:+ (phrase $ phase_change ^. term) :+:
  S ". (" :+: at_start' inModel +:+ S "are left out because" +:+
  S "they are not currently implemented in Drasil.)")]

-- The first paragraph is completely general and repeated in other examples. 
-- The second paragraph is very specific, and the other examples don't even 
-- include a paragraph analogous to this one.

-- Instance Models aren't implemented yet

s4_2_5_deriv1 = [Paragraph (S "Derivation of the energy balance on" +:+
  (phrase $ water ^. term) :+: S ":"),
  Paragraph (S "To find the rate of change of" +:+ P (temp_W ^.
  symbol) :+: S ", we look at the energy balance on" +:+
  (phrase $ water ^. term) :+: S ". The" +:+ 
  (phrase $ volume ^. term) +:+ S "being considered is the" +:+
  (phrase $ w_vol ^. term) +:+ EmptyS +:+ P (w_vol ^. symbol) :+:
  S ", which has" +:+ (phrase $ w_mass ^. term) +:+ EmptyS +:+ 
  P (w_mass ^. symbol) +:+ S "and" +:+ (phrase $ htCap_W ^. term) :+: 
  S "," +:+. P (htCap_W ^. symbol) +:+ P (ht_flux_C ^. 
  symbol) +:+ S "represents the" +:+ (phrase $ ht_flux_C ^. term) +:+
  S "and" +:+ P (ht_flux_P ^. symbol) +:+ S "represents" +:+
  S "the" +:+ (phrase $ ht_flux_P ^. term) :+: S ", over" +:+
  (phrase $ coil_SA ^. term) +:+ S "and" +:+ (phrase $ pcm_SA ^. term) +:+
  S "of" +:+ P (coil_SA ^. symbol) +:+ S "and" +:+ 
  P (pcm_SA ^. symbol) :+: S ", respectively. No" +:+
  (phrase $ heat_trans ^. term) +:+ S "occurs to the outside of the" +:+
  (phrase $ tank ^. term) :+: S ", since it" +:+
  S "has been assumed to be" +:+ (phrase $ perfect_insul ^. term) :+: 
  S " (A15)." +:+ S "Assuming no" +:+ (phrase $ vol_ht_gen ^. term) :+: 
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
  S "term of the" +:+ (short rightSide) +:+ S "of" +:+
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
  (short ode) +:+ S "for IM1:"),
  EqnBlock
   (Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) *
   (((C temp_C) - (C temp_W)) + (C eta) * ((C temp_PCM) - 
   (C temp_W))))
  ]

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Fractions in paragraph?

s4_2_5_deriv2 = [Paragraph (S "Detailed derivation of the energy balance" +:+
  S "on the" +:+ (short phsChgMtrl) +:+ S "during" +:+ 
  (phrase $ sens_heat ^. term) :+: S "ing phase:"),
  Paragraph (S "To find the rate of change of" +:+ P (temp_PCM ^.
  symbol) :+: S ", we look at the energy balance on the" +:+ 
  (short phsChgMtrl) :+: S ". The" +:+ (phrase $ volume ^. term) +:+
  S "being considered is the" +:+ (phrase $ pcm_vol ^. term) :+: 
  S "," +:+ P (pcm_vol ^. symbol) :+: S ". The derivation" +:+
  S "that follows is initially for the" +:+
  (phrase $ solid ^. term) +:+ EmptyS +:+ (short phsChgMtrl) :+:
  S ". The" +:+ (phrase $ pcm_mass ^. term) +:+ S "is" +:+ 
  P (pcm_mass ^. symbol) +:+ S "and the" +:+ (phrase $ htCap_S_P ^. 
  term) +:+ S "is" +:+. P (htCap_S_P ^. symbol) +:+
  S "The" +:+ (phrase $ ht_flux_P ^. term) +:+ S "is" +:+ 
  P (ht_flux_P ^. symbol) +:+ S "over" +:+ (phrase $ pcm_SA ^. term) +:+
  P (pcm_SA ^. symbol) :+: S ". There is no" +:+ 
  (phrase $ ht_flux_out ^. term) :+: S ". Assuming no" +:+
  (phrase $ vol_ht_gen ^. term) +:+ S "(A16)," +:+ P (vol_ht_gen ^. symbol) :+:
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
  Paragraph (S "Equation (6) applies for the" +:+ 
  (phrase $ solid ^. term) +:+ EmptyS +:+ (short phsChgMtrl) :+:
  S ". In the case where all of the" +:+
  (short phsChgMtrl) +:+ S "is melted, the same" +:+
  S "derivation applies, except that" +:+ P (htCap_S_P ^. 
  symbol) +:+ S "is replaced by" +:+ P (htCap_L_P ^. symbol) :+:
  S ", and thus" +:+ P (tau_S_P ^. symbol) +:+ S "is" +:+ 
  S "replaced by" +:+. P (tau_L_P ^. symbol) +:+
  S "Although a small change in surface area would be" +:+
  S "expected with" +:+ (phrase $ melting ^. term) :+:
  S ", this is not included, since the" +:+
  (phrase $ volume ^. term) +:+ S "change of the" +:+ (short phsChgMtrl) 
  +:+ S "with" +:+ (phrase $ melting ^. term) +:+
  S "is assumed to be negligible (A17)."),
  Paragraph (S "In the case where" +:+ P (temp_PCM ^. symbol) :+:
  S "=" :+: P (temp_melt_P ^. symbol) +:+ S "and not all of" +:+
  S "the" +:+ (short phsChgMtrl) +:+ S "is melted, the" +:+
  (phrase $ temp_PCM ^. term) +:+ S "does not change. Therefore, in" +:+
  S "this case d" :+: P (temp_PCM ^. symbol) :+: S "/d" :+:
  P (time ^. symbol) :+: S "=0."),
  Paragraph (S "This derivation does not consider the" +:+ 
  (phrase $ boiling ^. term) +:+ S "of the" +:+ 
  (short phsChgMtrl) :+: S ", as the" +:+ (short phsChgMtrl) 
  +:+ S "is assumed to either be in" +:+ S "a" +:+
  (solid ^. defn) +:+ S "or a" +:+ (liquid ^. defn) :+: 
  S " (A18).")]

-- Add GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Derivative notation in paragraph?

s4_2_6 = section (titleize' datum +:+ titleize' constraint) [s4_2_6_intro] []

s4_2_6_intro = Paragraph (titleize' table_ +:+ S "1 and 2 show the data" +:+
  plural constraint +:+ S "on the input and output variables," +:+
  S "respectively. The column for" +:+ phrase physical +:+
  plural constraint +:+ S "gives the" +:+ phrase physical +:+
  S "limitations on the range of values that can be taken by the variable." +:+
  S "The column for software" +:+ plural constraint +:+ S "restricts the" +:+
  S "range of inputs to reasonable values. The" +:+ plural constraint +:+
  S "are conservative, to give the user of the" +:+ phrase model +:+
  S "the flexibility to experiment with unusual situations. The column of" +:+
  S "typical values is intended to provide a feel for a common scenario." +:+
  S "The uncertainty column provides an estimate of the confidence with" +:+
  S "which the" +:+ phrase physical +:+ S "quantities can be measured." +:+
  S "This" +:+ phrase information +:+ S "would be part of the input if" +:+
  S "one were performing an uncertainty quantification exercise. (The" +:+
  plural table_ +:+ S "are left out because features they should use are" +:+
  S "not yet implemented in Drasil.)")

-- General paragraph, repeated between examples. Can be abstracted out.

-- I do not think Table 2 will end up being necessary for the Drasil version
---- The info from table 2 will likely end up in table 1.

inputVar :: [UWrapper]
inputVar = map uw [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P] ++ [uw htFusion] ++ map uw [coil_SA,
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

-- s4_2_7 = section (S "Properties of a Correct Solution") (s4_2_7_deriv) []

s4_2_7 = section (of'' titleize' titleize property (a_ corSol)) (s4_2_7_deriv) []

-- should be something like
--s4_2_7 = section (of'' titleize' titleize property (corSol)) (s4_2_7_deriv) []

s4_2_7_deriv = [Paragraph (S "A correct" +:+ phrase solution +:+ 
  S "must exhibit the" +:+ (phrase $ law_cons_energy ^. term) :+:
  S ". This means that the" +:+ (phrase $ w_E ^. term) +:+
  S "should equal the difference between" +:+
  S " the total energy input from the" +:+
  (phrase $ coil ^. term) +:+ S "and the energy output to the" +:+
  (short phsChgMtrl) :+: S ". This can be shown as an" +:+
  S "equation by taking" +:+ makeRef s4_2_4_DD1 +:+ S "and" +:+
  makeRef s4_2_4_DD2 :+: S ", multiplying each by their" +:+
  S "respective surface area of" +:+ (phrase $ heat_trans ^. term) :+:
  S ", and integrating each over the simulation" +:+
  (phrase $ time ^. term) :+: S ", as follows:"),
  EqnBlock 
  ((C w_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time))) 
  ((C coil_HTC) * (C coil_SA) * ((C temp_C) - FCall (C temp_W) 
  [C time])) time) - UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) -
  (FCall (C temp_PCM) [C time]))) time)),
  Paragraph (S "In addition, the" +:+ (phrase $ pcm_E ^. term) :+: 
  S " should equal the energy input to the" +:+ (short phsChgMtrl) +:+ 
  S "from the" +:+ (phrase $ water ^. term) :+:
  S ". This can be expressed as"),
  EqnBlock
  ((C pcm_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) - (FCall
  (C temp_PCM) [C time]))) time)),
  Paragraph (S "Equations (reference) and (reference) can be" +:+
  S "used as" +:+ Quote (S "sanity") :+: S "checks to gain" +:+ 
  S "confidence in any" +:+ phrase solution +:+ S "computed by" +:+
  (short progName) :+: S ". The relative error between the results" +:+
  S "computed by" +:+ (short progName) +:+ S "and the" +:+
  S "results calculated from the" +:+ (short rightSide) :+: 
  S " of these equations should be less than 0.001% (R9).")]

-- Above section only occurs in this example (although maybe it SHOULD be in
-- the others).

-- Remember to insert references in above derivation when available

s5 = section (titleize' requirement) [s5_intro] [s5_1, s5_2]

s5_intro = Paragraph (S "This" +:+ phrase section_ +:+ S "provides the" +:+
  S "functional" +:+ plural requirement `sC` S "the business tasks" +:+
  S "that the software is expected to complete, and the" +:+
  S "non-functional" +:+ plural requirement `sC` 
  S "the qualities that the software is expected to exhibit.")

-- General paragraph, repeated in every example. Can be abstracted out.

s5_1 = section (titleize' functionalRequirement) (s5_1_list) []

s5_1_list = [Enumeration (Simple [((short requirement) :+: S "1", Flat 
  (S "Input the following quantities, which define the" +:+
  (phrase $ tank ^. term) :+: S " parameters, material properties" +:+
  S "and initial" +:+ plural condition :+: S ":"))]),
  (Table [phrase symbol_, (phrase $ unit_ ^. term), phrase description] (mkTable
  [(\ch -> P (ch ^. symbol)),
  (\ch -> Sy (unit_symb ch)),
  (\ch -> phrase $ ch ^. term)] inputVar) 
  (S "Input Variable" +:+ (titleize requirement)) False),
--
  Enumeration (Simple [((short requirement) :+: S "2", Flat 
  (S "Use the inputs in R1 to find the" +:+ (phrase $ mass ^. term) +:+
  S "needed for IM1 to IM4, as follows, where" +:+
  P (w_vol ^. symbol) +:+ S "is the" +:+(phrase $ w_vol ^. term) +:+
  S "and" +:+ P (tank_vol ^. symbol) +:+ S "is the" +:+.
  (phrase $ tank_vol ^. term)))]),
  EqnBlock ((C w_mass) := (C w_vol) * (C w_density) := ((C tank_vol) -
  (C pcm_vol)) * (C w_density) := (((C diam) / 2) * (C tank_length) - 
  (C pcm_vol)) * (C w_density)),
  EqnBlock ((C pcm_mass) := (C pcm_vol) * (C pcm_density)),
--
  Enumeration (Simple [((short requirement) :+: S "3", Flat 
  (S "Verify that the inputs satisfy the required" +:+ phrase physical +:+
  plural constraint +:+ S "shown in" +:+ titleize table_ +:+ S "1.")),
--
  ((short requirement) :+: S "4", Flat (S "Output the input" :+: 
  S " quantities and derived quantities in the following list: "  :+:
  S "the quantities from R1, the" +:+ (phrase $ mass ^. term) :+: S "es" +:+
  S "from R2," +:+ P (tau_W ^. symbol) +:+ S "(from IM1)," +:+ 
  P (eta ^. symbol) +:+ S "(from IM1)," +:+ P (tau_S_P ^. 
  symbol) +:+ S "(from IM2) and" +:+ P (tau_L_P ^. symbol) +:+
  S "(from IM2).")),
--
  ((short requirement) :+: S "5", Flat (S "Calculate and" +:+
  S "output the" +:+ (phrase $ temp_W ^. term) +:+ S "(" :+: P (temp_W ^.
  symbol) :+: S "(" :+: P (time ^. symbol) :+: S "))" +:+
  S "over the simulation" +:+ (phrase $ time ^. term) +:+ S "(from IM1).")),
--
  ((short requirement) :+: S "6", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (phrase $ temp_PCM ^. term) +:+ S "(" :+:
  P (temp_PCM ^. symbol) :+: S "(" :+: P (time ^. symbol) :+:
  S ")) over the simulation" +:+ (phrase $ time ^. term) :+: 
  S " (from IM2).")),
--
  ((short requirement) :+: S "7", Flat (S "Calculate and" +:+ 
  S " output the" +:+ (phrase $ w_E ^. term) +:+ S "(" :+: P (w_E ^. 
  symbol) :+: S "(" :+: P (time ^. symbol) :+: S "))" +:+
  S "over the simulation" +:+ (phrase $ time ^. term) +:+ S "(from IM3).")),
--
  ((short requirement) :+: S "8", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (phrase $ pcm_E ^. term) +:+ S "(" :+: P (pcm_E ^.
  symbol) :+: S "(" :+: P (time ^. symbol) :+: S ")) over the" +:+
  S "simulation" +:+ (phrase $ time ^. term) +:+ S "(from IM4).")),
--
  ((short requirement) :+: S "9", Flat (S "Verify that the" +:+
  S "energy outputs (" :+: P (w_E ^. symbol) :+: S "(" :+: P (time ^. 
  symbol) :+: S ") and" +:+ P (pcm_E ^. symbol) :+: S "(" :+:
  P (time ^. symbol) :+: S ")) follow the" +:+
  (phrase $ law_cons_energy ^. term) :+: S ", as outlined in" +:+ 
  makeRef s4_2_7 :+: S ", with relative error no greater than" +:+
  S "0.001%.")),
--
  ((short requirement) :+: S "10", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (phrase $ time ^. term) +:+ S "at which the" +:+ 
  (short phsChgMtrl) +:+ S "begins to melt" +:+
  P (t_init_melt ^. symbol) +:+ S "(from IM2).")),
--
  ((short requirement) :+: S "11", Flat (S "Calculate and" +:+ 
  S "output the" +:+ (phrase $ time ^. term) +:+ S "at which the" +:+ 
  (short phsChgMtrl) +:+
  S "stops" +:+ (phrase $ melting ^. term) +:+
  EmptyS +:+ P (t_final_melt ^. symbol) +:+ S "(from IM2)."))])
  ]

-- List structure same between all examples

--How to include pi?
--How to add exponents?

s5_2 = section (titleize' nonfunctionalRequirement) [s5_2_contents] []

s5_2_contents = Paragraph (S "Given the small size, and relative simplicity"
  `sC`
  S "of this" +:+ phrase problem :+: S ", performance is not a priority." +:+
  S "Any reasonable implementation will be very quick and use" +:+
  S "minimal storage. Rather than performance, the priority" +:+
  S "nonfunctional" +:+ plural requirement +:+ 
  S "are correctness, verifiability" `sC`
  S "understandability, reusability, and maintainability.")

-- The second sentence of the above paragraph is repeated in all examples (not 
-- exactly, but the general idea is). The first sentence is not always 
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be 
-- abstracted out.

s6 = section (titleize' likelyChg) [s6_list] []

-- The game physics example has a short intro paragraph that can likely be 
-- abstracted out and used for all examples.

s6_list = Enumeration (Simple [((short likelyChg) :+: S "1", Flat 
  (S "A4 -" +:+ (short phsChgMtrl) +:+ S "is actually a poor" +:+
  (phrase $ thermal_conductor ^. term) :+: S ", so" +:+
  S "the" +:+ (phrase assumption) +:+
  S "of uniform" +:+ (phrase $ temp_PCM ^. term) +:+. S "is not likely")),
--
  ((short likelyChg) :+: S "2", Flat (S "A8 - The" +:+ (phrase $ temp_C ^.
  term) +:+ S "will change over the course of the day, depending" +:+.
  S "on the energy received from the sun")),
--
  ((short likelyChg) :+: S "3", Flat (S "A9 - The" +:+ (phrase $ temp_C ^. 
  term) +:+ S "will actually change along its length as the" +:+
  (phrase $ water ^. term) +:+. S "within it cools")),
--
  ((short likelyChg) :+: S "4", Flat (S "A11 - The" +:+ phrase model +:+
  S "currently only accounts for" +:+. (charging ^. defn) +:+
  S "A more complete" +:+ phrase model +:+ S "would also" +:+
  S "account for" +:+ (discharging ^. defn) :+: S ".")),
--
  ((short likelyChg) :+: S "5", Flat (S "A12 - To add more" +:+
  S " flexibility to the simulation, the" +:+ (phrase $ temp_init ^. term) +:+
  S "of the" +:+ (phrase $ water ^. term) :+: 
  S " and the" +:+ (short phsChgMtrl) +:+ S "could be" +:+.
  S "allowed to have different values")),
--
  ((short likelyChg) :+: S "6", Flat (S "A15 - Any real" +:+
  (phrase $ tank ^. term) +:+ S "cannot be" +:+
  (phrase $ perfect_insul ^. term) +:+. S "and will lose heat"))])

-- List structure same in all examples.

--add referencing to assumptions?
  
s7 = section (titleize' traceyMandG) ([s7_intro1, s7_table1, s7_table2,
  s7_table3] ++ (s7_intro2) ++ [s7_fig1, s7_fig2]) []

s7_intro1 = Paragraph (S "The" +:+ phrase purpose +:+ S "of the" +:+
  S "traceability matrices is to" +:+ 
  S "provide easy references on what has to be additionally" +:+
  S "modified if a certain component is changed. Every time a" +:+
  S "component is changed, the items in the column of that" +:+
  S "component that are marked with an" +:+ Quote (S "X") :+: 
  S " should be modified as well." +:+ makeRef s7_table1 +:+
  S "shows the dependencies of" +:+ plural thModel `sC`
  (plural genDefn) `sC` (plural dataDefn) `sC`
  S "and" +:+ plural inModel +:+
  S "with each other." +:+ makeRef s7_table2 +:+ S "shows the" +:+
  S "dependencies of" +:+ plural inModel `sC`
  plural requirement `sC`
  S "and data" +:+ plural constraint +:+ S "on each other." +:+ 
  makeRef s7_table3 +:+ S "shows the dependencies of" +:+ 
  plural thModel `sC`
  (plural genDefn) `sC` 
  (plural dataDefn) `sC`
  (plural inModel) `sC` S "and" +:+ 
  (plural likelyChg) +:+ S "on the" +:+.
  (titleize' assumption))

-- Completely general paragraph, and similar ones in other example, but slight
-- differences in what is included in each matrix. Perhaps we can abstract out 
-- which types of items are associated with each matrix i.e. instance models, 
-- assumptions, requirements, etc. If so, this paragraph can be abstracted out.

s7_table1 = Table [EmptyS, makeRef s4_2_2_T1, makeRef s4_2_2_T2, 
  makeRef s4_2_2_T3, S "GD1", S "GD2", makeRef s4_2_4_DD1, 
  makeRef s4_2_4_DD2, makeRef s4_2_4_DD3, makeRef s4_2_4_DD3, S "IM1",
  S "IM2", S "IM3", S "IM4"]
  [[makeRef s4_2_2_T1, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_2_T2, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_2_T3, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "GD1", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "GD2", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_4_DD1, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_4_DD2, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_4_DD3, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_4_DD3, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "IM1", EmptyS, EmptyS, EmptyS, EmptyS, S "X", S "X", S "X", EmptyS,
  EmptyS, EmptyS, S "X", EmptyS, EmptyS],
  [S "IM2", EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, S "X", EmptyS,
  S "X", S "X", EmptyS, EmptyS, S "X"],
  [S "IM3", EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "IM4", EmptyS, S "X", S "X", EmptyS, EmptyS, EmptyS, S "X", S "X", S "X",
  EmptyS, S "X", EmptyS, EmptyS]]
  (S "Traceability Matrix Showing the Connections Between Items" +:+
  S "of Different" +:+ titleize' section_) True

-- Wrong DD reference above, change when DD4 is available (twice)

s7_table2 = Table [EmptyS, S "IM1", S "IM2", S "IM3", S "IM4", makeRef s4_2_6,
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
  (S "Traceability Matrix Showing the Connections Between" +:+
  (titleize' requirement) +:+ S "and" +:+ (titleize' inModel)) True

s7_table3 = Table [EmptyS, S "A1", S "A2", S "A3", S "A4", S "A5", S "A6",
  S "A7", S "A8", S "A9", S "A10", S "A11", S "A12", S "A13", S "A14",
  S "A15", S "A16", S "A17", S "A18", S "A19"]
  [[makeRef s4_2_2_T1, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_2_T2, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_2_T3, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "GD1", EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [S "GD2", EmptyS, EmptyS, S "X", S "X", S "X", S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS],
  [makeRef s4_2_4_DD1, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X",
  S "X", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS],
  [makeRef s4_2_4_DD2, EmptyS, EmptyS, S "X", S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_4_DD3, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [makeRef s4_2_4_DD3, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
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
  (S "Traceability Matrix Showing the Connections Between" +:+
  (titleize' assumption) +:+ S "and Other Items") True

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

-- Wrong DD reference above, change when DD4 is available

s7_intro2 = [Paragraph (S "The" +:+ phrase purpose +:+ S "of the" +:+
  S "traceability graphs is also to provide easy references on what has" +:+
  S "to be additionally modified if a certain component is changed. The" +:+
  S "arrows in the graphs represent dependencies. The component at the" +:+
  S "tail of an arrow is depended on by the component at the head of" +:+
  S "that arrow. Therefore, if a component is changed, the" +:+
  S "components that it points to should also be changed." +:+
  makeRef s7_fig1 +:+ S "shows the dependencies of" +:+
  plural thModel `sC`
  (plural genDefn) `sC`
  (plural dataDefn) `sC`
  (plural inModel) `sC`
  (plural likelyChg) `sC` S "and" +:+
  (plural assumption) +:+ S "on each" +:+
  S "other." +:+ makeRef s7_fig2 +:+ S "shows the dependencies" +:+
  S "of" +:+ plural inModel `sC`
  plural requirement `sC` S "and data" +:+
  plural constraint +:+ S "on each other."),
  Paragraph (S "NOTE: Building a tool to automatically generate" +:+
  S "the graphical representation of the matrix by scanning the" +:+
  S "labels and reference can be future work.")]

-- Same comments on this paragraph as I had for s7_intro1. 

s7_fig1 = Figure (S "Traceability Graph Showing the Connections Between" +:+
  S "Items of Different" +:+ titleize' section_) "ATrace.png"

s7_fig2 = Figure (S "Traceability Graph Showing the Connections Between" +:+
  (titleize' requirement) `sC` titleize' inModel `sC`
  S "and Data" +:+ titleize' constraint) "RTrace.png"

--References?

